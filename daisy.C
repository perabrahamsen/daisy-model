// daisy.C

#include "daisy.h"
#include "weather.h"
#include "groundwater.h"
#include "horizon.h"
#include "log.h"
#include "parser.h"
#include "am.h"
#include "nitrification.h"
#include "bioclimate.h"
#include "hydraulic.h"
#include "crop.h"
#include "column.h"
#include "harvest.h"
#include "action.h"
#include "filter.h"
#include "library.h"
#include "syntax.h"
#include "condition.h"
#include "alist.h"
#include "frame.h"
#include "common.h"
#include <iostream.h>

Daisy::Daisy (const AttributeList& al)
  : running (false),
    logs (map_create<Log> (al.alist_sequence ("output"))),
    time (al.time ("time")),
    action (Librarian<Action>::create (al.alist ("manager"))),
    weather (Librarian<Weather>::create (al.alist ("weather"))), 
    columns (*new ColumnList (al.alist_sequence ("column"))),
    harvest (*new vector<const Harvest*>)
{ 
  const vector<AttributeList*>& column_alists = al.alist_sequence ("column");
  
  for (unsigned int i = 0; i < columns.size (); i++)
    columns[i]->initialize (*column_alists[i], time, weather);
}

bool
Daisy::check (const Syntax& syntax)
{
  bool all_ok = true;

  // Check columns.
  {
    bool ok = true;
    // This was ::const_iterator in g++
    for (ColumnList::iterator i = columns.begin ();
	 i != columns.end ();
	 i++)
      {
	if (*i == NULL || !(*i)-> check ())
	  ok = false;
      }
    if (!ok)
      {
	cerr << "Malformed column(s)\n";
	all_ok = false;
      }
  }
  // Check filters.
  {
    bool ok = true;
    for (vector<Log*>::const_iterator i = logs.begin ();
	 i != logs.end ();
	 i++)
      {
	if (*i == NULL || !(*i)-> check (syntax))
	  ok = false;
      }
    if (!ok)
      {
	cerr << "Malformed log(s)\n";
	all_ok = false;
      }
  }
  // Check actions.
  if (!action.check (*this))
    {
      cerr << "Malformed action(s)\n";
      all_ok = false;
    }

  return all_ok;
}

void
Daisy::tick_columns ()
{
  for (unsigned int i = 0; i < columns.size (); i++)
    columns[i]->tick (time, weather);
}

void
Daisy::tick_logs ()
{
  for (unsigned int i = 0; i < logs.size (); i++)
    {
      Log& log = *logs[i];
      Filter& filter = log.match (*this);
      log.output ("time", filter, time);
      output_derived (weather, "weather", log, filter);
      output_list (columns, "column", log, filter, 
		   Librarian<Column>::library ());
      output_vector (harvest, "harvest", log, filter);
    }
}

void 
Daisy::tick ()
{ 
  action.doIt (Frame (), *this);

  weather.tick (time);
  tick_columns ();
  tick_logs ();
  time.tick_hour ();
}

void 
Daisy::run ()
{ 
  running = true;

  while (running)
    {
      if (time.hour () == 0)
	cout << time.year () << "-" << time.month () << "-" 
	     << time.mday () << "\n";
      tick ();
    }
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Harvest>;
#endif

void
Daisy::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Library::load_syntax (syntax, alist);

  syntax.add ("output", Librarian<Log>::library (),
	      Syntax::Const, Syntax::Sequence);
  syntax.add ("input", Librarian<Parser>::library (), Syntax::Optional, 
	      Syntax::Singleton);
  syntax.add ("manager", Librarian<Action>::library (), Syntax::Const);
  syntax.add ("time", Syntax::Date, Syntax::State);
  syntax.add ("column",
	      Librarian<Column>::library (), 
	      Syntax::State, Syntax::Sequence);
  syntax.add ("weather", Librarian<Weather>::library ());
  add_submodule<Harvest> ("harvest", syntax, alist,
			  Syntax::LogOnly, Syntax::Sequence);
}

Daisy::~Daisy ()
{
  sequence_delete (logs.begin (), logs.end ());
  delete &logs;
  delete &action;
  delete &weather;
#if 0
  delete &columns;
#endif
  delete &harvest;
}
