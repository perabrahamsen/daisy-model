// daisy.C

#include "daisy.h"
#include "input.h"
#include "manager.h"
#include "weather.h"
#include "groundwater.h"
#include "horizon.h"
#include "log.h"
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
  : logs (map_create<Log> (al.list_sequence ("output"))),
    time (al.time ("time")),
    manager (Manager::create (al.list ("chief"))),
    weather (Weather::create (time, al.list ("weather"))), 
    groundwater (Groundwater::create (time, al.list ("groundwater"))), 
    columns (*new ColumnList (al.list_sequence ("field")))
{ 
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      if (!(*i)-> check ())
	ok = false;
    }
  if (!ok)
    THROW (Initialization ("Malformed column(s)"));
}

void 
Daisy::run ()
{ 
  while (true)
    {
      const Action* action = manager.action (*this);
	    
      if (action->stop ())
	{
	  if (time.hour () != 0)
	    cout << "\n";
	  break;
	}
	    
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
      action->doIt (*this);

      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  (*i)->tick (time, weather, groundwater);
	}
      time.tick ();
      for (vector<Log*>::const_iterator i = logs.begin ();
	   i != logs.end ();
	   i++)
	{
	  Log& log = **i;
	  const Filter* filter= &log.match (*this);
	  if (filter == Filter::none)
	    // Don't waste time with empty filters.
	    continue;
	  log.open ();
	  log.output ("time", filter, time);
	  weather.output ("weather", log, filter);
	  if (filter->check ("field"))
	    {
	      const Filter* f = filter->lookup ("field");
	      log.open ("field");
	      for (ColumnList::iterator column = columns.begin();
		   column != columns.end();
		   column++)
		{
		  if (f->check ((*column)->name))
		    (*column)->output (log, f->lookup ((*column)->name));
		}
	      log.close ();
	    }
	  log.close ();
	}
      weather.tick ();
    }
}

void
Daisy::load_syntax (Syntax& syntax)
{
  syntax.add_class ("crop", Crop::library (), &Crop::derive_type);
  syntax.add_class ("horizon", Horizon::library (), &Horizon::derive_type);
  syntax.add_class ("column", Column::library (), &Column::derive_type);
  syntax.add_class ("manager", Manager::library (), &Manager::derive_type);
  syntax.add_class ("log", Log::library (), &Manager::derive_type);
  syntax.add ("output", Log::library (), Syntax::Const, Syntax::Sequence);
  syntax.add ("chief", Manager::library (), Syntax::Const);
  syntax.add ("time", Syntax::Date, Syntax::InOut);
  syntax.add ("field", Column::library (), Syntax::InOut, Syntax::Sequence);
  syntax.add ("weather", Weather::library ());
  syntax.add ("groundwater", Groundwater::library (), Syntax::Const);
}

Daisy::~Daisy ()
{
  sequence_delete (logs.begin (), logs.end ());
  delete &logs;
  delete &manager;
  delete &weather;
  delete &groundwater;
  delete &columns;
}
