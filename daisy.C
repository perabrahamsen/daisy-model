// daisy.C

#include "daisy.h"
#include "weather.h"
#include "groundwater.h"
#include "horizon.h"
#include "log.h"
#include "parser.h"
#include "am.h"
#include "nitrification.h"
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
#include "common.h"
#include <iostream.h>

#ifdef MIKE_SHE
#include "mike_she.h"
#endif

Daisy::Daisy (const AttributeList& al)
  : running (false),
    logs (map_create<Log> (al.alist_sequence ("output"))),
    time (al.time ("time")),
    action (Action::create (al.alist ("manager"), NULL)),
    weather (Librarian<Weather>::create (al.alist ("weather"))), 
    groundwater (Librarian<Groundwater>::create (al.alist ("groundwater"))), 
    columns (*new ColumnList (al.alist_sequence ("column"))),
    harvest (*new vector<const Harvest*>)
{ 
#ifdef MIKE_SHE
  assert (!mike_she);
  mike_she = new MikeSHE (al.alist ("MikeSHE"), time);
#endif
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
    columns[i]->tick (time, weather, groundwater);
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
      groundwater.output (log, filter);
      output_list (columns, "column", log, filter);
      output_vector (harvest, "harvest", log, filter);
    }
}

void 
Daisy::tick ()
{ 
  action.doIt (*this);

  weather.tick (time);
  groundwater.tick (time);
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
      switch (time.hour ())
	{
	case 0:
	  cout << "Tick " << time.year () << "-" << time.month () << "-"
	       << time.mday ();
	default:
	  cout << " " << time.hour ();
	  cout.flush ();
	  break;
	case 23:
	  cout << " " << time.hour () << "\n";
	  break;
	}
      tick ();
    }
  if (time.hour () != 0)
    cout << "\n";
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Harvest>;
#endif

void
Daisy::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Libraries.
  syntax.add_library ("defcrop", Crop::library (), &Crop::derive_type);
  Librarian<Horizon>::add_library (syntax, "defhorizon");
  Librarian<Column>::add_library (syntax, "defcolumn");
  Librarian<Log>::add_library (syntax, "deflog");
  syntax.add_library ("defparser", Parser::library (), &Parser::derive_type);
  syntax.add_library ("defam", AM::library (), &AM::derive_type);

  syntax.add_library ("defaction", Action::library (), &Action::derive_type);
  Librarian<Condition>::add_library (syntax, "defcondition");
  Librarian<Weather>::add_library (syntax, "defweather");
  Librarian<Groundwater>::add_library (syntax, "defgroundwater");
  Librarian<UZmodel>::add_library (syntax, "defuzmodel");
  Librarian<Hydraulic>::add_library (syntax, "defhydraulic");
  Librarian<Nitrification>::add_library (syntax, "defnitrification");
  Librarian<Filter>::add_library (syntax, "deffilter");

  // The actual data.
  syntax.add ("output", Librarian<Log>::library (),
	      Syntax::Const, Syntax::Sequence);
  syntax.add ("input", Parser::library (), Syntax::Optional, 
	      Syntax::Singleton);
  syntax.add ("manager", Action::library (), Syntax::Const);
  syntax.add ("time", Syntax::Date, Syntax::State);
  syntax.add ("column",
	      Librarian<Column>::library (), 
	      Syntax::State, Syntax::Sequence);
  syntax.add ("weather", Librarian<Weather>::library ());
  syntax.add ("groundwater", Librarian<Groundwater>::library (), 
	      Syntax::Const);
  add_submodule<Harvest> ("harvest", syntax, alist,
			  Syntax::LogOnly, Syntax::Sequence);
#ifdef MIKE_SHE
  add_submodule<MikeSHE> ("MikeSHE", syntax, alist);
#endif
}

Daisy::~Daisy ()
{
#ifdef MIKE_SHE
  assert (mike_she);
  delete mike_she;
  mike_she = NULL;
#endif
  sequence_delete (logs.begin (), logs.end ());
  delete &logs;
  delete &action;
  delete &weather;
  delete &groundwater;
#if 0
  delete &columns;
#endif
  delete &harvest;
}
