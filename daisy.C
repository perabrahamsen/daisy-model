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

Daisy::Daisy (const AttributeList& al)
  : syntax (NULL),
    alist (al),
    running (false),
    frame (NULL),
    logs (map_create<Log> (al.alist_sequence ("output"))),
    time (al.time ("time")),
    action (Librarian<Action>::create (al.alist ("manager"))),
    weather (Librarian<Weather>::create (al.alist ("weather"))), 
    columns (*new ColumnList (al.alist_sequence ("column"))),
    harvest (*new vector<const Harvest*>)
{ }

bool
Daisy::check ()
{
  assert (syntax);
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
	CERR << "Malformed column(s)\n";
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
	if (*i == NULL || !(*i)-> check (*syntax))
	  ok = false;
      }
    if (!ok)
      {
	CERR << "Malformed log(s)\n";
	all_ok = false;
      }
  }
  // Check actions.
  if (!action.check (*this))
    {
      CERR << "Malformed action(s)\n";
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
      Filter& filter = log.match (frame, *this);
      log.output ("time", filter, time);
      output_derived (weather, "weather", log, filter);
      output_list (columns, "column", log, filter, 
		   Librarian<Column>::library ());
      output_vector (harvest, "harvest", log, filter);
      log.done ();
    }
}

void
Daisy::tick ()
{ 
  action.doIt (frame, *this);
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
	COUT << time.year () << "-" << time.month () << "-" 
	     << time.mday () << "\n";
      tick ();
    }
}

void
Daisy::initialize (const Syntax& s)
{ 
  syntax = & s; 

  const vector<AttributeList*>& column_alists 
    = alist.alist_sequence ("column");
  
  for (unsigned int i = 0; i < columns.size (); i++)
    columns[i]->initialize (*column_alists[i], time, weather);
}

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<Harvest>;
#endif

void
Daisy::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Library::load_syntax (syntax, alist);

  syntax.add ("description", Syntax::String, Syntax::Optional,
	      "Description of this simulation setup.");
  syntax.add ("output", Librarian<Log>::library (),
	      Syntax::Const, Syntax::Sequence,
	      "List of logs for output during the simulation.");
  syntax.add ("input", Librarian<Parser>::library (), Syntax::Optional, 
	      Syntax::Singleton,
	      "Command to add more information about the simulation.");
  syntax.add ("manager", Librarian<Action>::library (), Syntax::Const,
	      Syntax::Singleton,
	      "Specify the management operations to perform during \
the simulation.");
  syntax.add ("time", Syntax::Date, Syntax::State,
	      "Current time in the simulation.");
  syntax.add ("column",
	      Librarian<Column>::library (), 
	      Syntax::State, Syntax::Sequence,
	      "List of columns to use in this simulation.");
  syntax.add ("weather", Librarian<Weather>::library (),
	      Syntax::State, Syntax::Singleton,
	      "Weather model for probiding climate information during \
the simulation.");
  add_submodule_sequence<Harvest> ("harvest", syntax, Syntax::LogOnly, 
				   "Total list of all crop yields.");
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
