// daisy.C

#include "daisy.h"
#include "weather.h"
#include "groundwater.h"
#include "horizon.h"
#include "log.h"
#include "parser.h"
#include "aom.h"
#include "crop.h"
#include "column.h"
#include "action.h"
#include "filter.h"
#include "library.h"
#include "syntax.h"
#include "condition.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>

Daisy::Daisy (const AttributeList& al)
  : running (false),
    logs (map_create<Log> (al.list_sequence ("output"))),
    time (al.time ("time")),
    action (Action::create (al.list ("manager"))),
    weather (Weather::create (time, al.list ("weather"))), 
    groundwater (Groundwater::create (time, al.list ("groundwater"))), 
    columns (*new ColumnList (al.list_sequence ("field")))
{ 
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

      weather.tick ();
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
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
	  weather.output ("weather", log, filter);
	  if (filter.check ("field"))
	    {
	      const Filter& f = filter.lookup ("field");
	      log.open ("field");
	      for (ColumnList::iterator column = columns.begin();
		   column != columns.end();
		   column++)
		{
		  if (f.check ((*column)->name))
		    (*column)->output (log, f.lookup ((*column)->name));
		}
	      log.close ();
	    }
	}
      time.tick ();
    }
  if (time.hour () != 0)
    cout << "\n";
}

void
Daisy::load_syntax (Syntax& syntax)
{
  syntax.add_class ("crop", Crop::library (), &Crop::derive_type);
  syntax.add_class ("horizon", Horizon::library (), &Horizon::derive_type);
  syntax.add_class ("column", Column::library (), &Column::derive_type);
  syntax.add_class ("log", Log::library (), &Log::derive_type);
  syntax.add_class ("parser", Parser::library (), &Parser::derive_type);
  syntax.add_class ("am", AOM::library (), &AOM::derive_type);
  syntax.add ("output", Log::library (), Syntax::Const, Syntax::Sequence);
  syntax.add ("input", Parser::library (), Syntax::Optional, 
	      Syntax::Singleton);
  syntax.add ("manager", Action::library (), Syntax::Const);
  syntax.add ("time", Syntax::Date, Syntax::State);
  syntax.add ("field", Column::library (), Syntax::State, Syntax::Sequence);
  syntax.add ("weather", Weather::library ());
  syntax.add ("groundwater", Groundwater::library (), Syntax::Const);
}

Daisy::~Daisy ()
{
  sequence_delete (logs.begin (), logs.end ());
  delete &logs;
  delete &action;
  delete &weather;
  delete &groundwater;
  delete &columns;
}
