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
    logs (map_create<Log> (al.list_sequence ("output"))),
    time (al.time ("time")),
    action (Action::create (al.list ("manager"), NULL)),
    weather (Librarian<Weather>::create (al.list ("weather"))), 
    groundwater (Groundwater::create (time, al.list ("groundwater"))), 
    columns (*new ColumnList (al.list_sequence ("column"))),
    harvest (*new vector<const Harvest*>)
{ 
#ifdef MIKE_SHE
  assert (!mike_she);
  mike_she = new MikeSHE (al.list ("MikeSHE"), time);
#endif
  
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      if (*i == NULL || !(*i)-> check ())
	ok = false;
    }
  if (!ok)
    THROW (Initialization ("Malformed column(s)"));

  if (!action.check (*this))
    THROW (Initialization ("Malformed action(s)"));
}

void 
Daisy::run ()
{ 
  running = true;

  while (running)
    {
#ifdef MIKE_SHE 
      mike_she->receive ();
#endif
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
      action.doIt (*this);

      weather.tick (time);
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
#ifdef MIKE_SHE 
	  mike_she->select ((*i)->name);
#endif
	  (*i)->tick (time, weather, groundwater);
	}
      for (vector<Log*>::const_iterator i = logs.begin ();
	   i != logs.end ();
	   i++)
	{
	  Log& log = **i;
	  const Filter& filter = log.match (*this);
	  if (&filter == Filter::none)
	    // Don't waste time with empty filters.
	    continue;
	  log.output ("time", filter, time);
	  output_derived (weather, "weather", log, filter);
	  output_list (columns, "column", log, filter);
	  output_vector (harvest, "harvest", log, filter);
	}
      time.tick ();
#ifdef MIKE_SHE 
      if (mike_she->done ())
	running = false;
      else
	mike_she->send ();
#endif
    }
  if (time.hour () != 0)
    cout << "\n";
}

void
Daisy::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_class ("defcrop", Crop::library (), &Crop::derive_type);
  syntax.add_class ("defhorizon", Horizon::library (), &Horizon::derive_type);
  syntax.add_class ("defcolumn", Column::library (), &Column::derive_type);
  syntax.add_class ("deflog", Log::library (), &Log::derive_type);
  syntax.add_class ("defparser", Parser::library (), &Parser::derive_type);
  syntax.add_class ("defam", AM::library (), &AM::derive_type);
  // These are mostly for making 
  syntax.add_class ("defaction", Action::library (), &Action::derive_type);
  syntax.add_class ("defcondition",
		    Condition::library (), &Condition::derive_type);
  syntax.add_class ("defweather", 
		    Librarian<Weather>::library (), 
		    Librarian<Weather>::derive_type);
  syntax.add_class ("defgroundwater",
		    Groundwater::library (), &Groundwater::derive_type);
  syntax.add_class ("defuzmodel", UZmodel::library (), &UZmodel::derive_type);
  syntax.add_class ("defhydraulic",
		    Hydraulic::library (), &Hydraulic::derive_type);
  syntax.add_class ("defnitrification", 
		    Librarian<Nitrification>::library (),
		    &Librarian<Nitrification>::derive_type);
  // The actual data.
  syntax.add ("output", Log::library (), Syntax::Const, Syntax::Sequence);
  syntax.add ("input", Parser::library (), Syntax::Optional, 
	      Syntax::Singleton);
  syntax.add ("manager", Action::library (), Syntax::Const);
  syntax.add ("time", Syntax::Date, Syntax::State);
  syntax.add ("column", Column::library (), Syntax::State, Syntax::Sequence);
  syntax.add ("weather", Librarian<Weather>::library ());
  syntax.add ("groundwater", Groundwater::library (), Syntax::Const);
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
