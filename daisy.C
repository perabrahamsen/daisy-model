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
#include <iostream.h>

struct Daisy::Implementation
{   
  Implementation () { };
};

Daisy::Daisy (Log& l, const AttributeList& al)
  : impl (*new Implementation ()), 
    log (l), 
    time (al.time ("time")),
    manager (Manager::create (al.list ("chief"))),
    weather (Weather::create (time, al.list ("weather"))), 
    groundwater (Groundwater::create (time, al.list ("groundwater"))), 
    columns (*new ColumnList (al.sequence ("field")))
{ }

void 
Daisy::run ()
{ 
  while (true)
    {
      const Action* action = manager.action (*this);
	    
      if (action->stop ())
	break;
	    
      cout << "Tick " << time.year () << "-" << time.month () << "-"
	   << time.mday () << " " << time.hour () << " ";

      action->doIt (columns, weather, log);

      for (ColumnList::iterator column = columns.begin ();
	   column != columns.end ();
	   column++)
	{
	  (*column)->tick (time, weather, groundwater);
	}

      time.tick ();
      weather.tick ();
      log.tick (*this);
    }
}

bool
Daisy::match (const Condition* c) const
{
  return c->match (columns, weather, time);
}

void
Daisy::output (Log& log, const Filter* filter) const
{
  log.open ();
  log.output ("time", filter, time);
  if (filter->check ("field"))
    output_field (log, filter->lookup ("field"));
  log.close ();
}

void
Daisy::output_field (Log&, const Filter* filter) const

{
  log.open ("field");
  for (ColumnList::iterator column = columns.begin();
       column != columns.end();
       column++)
    {
      if (filter->check ((*column)->name))
	(*column)->output (log, filter->lookup ((*column)->name));
    }
  log.close ();
}

void
Daisy::load_syntax (Syntax& syntax)
{
  syntax.add_class ("crop", Crop::par_library (), &Crop::derive_type);
  syntax.add_class ("horizon", Horizon::library (), &Horizon::derive_type);
  syntax.add_class ("column", Column::library (), &Column::derive_type);
  syntax.add_class ("manager", Manager::library (), &Manager::derive_type);
  syntax.add_object ("chief", Manager::library ());
  syntax.add ("time", Syntax::Date);
  syntax.add_sequence ("field", Column::library ());
  syntax.add_output ("log", syntax, Syntax::Sparse);
  syntax.add_object ("weather", Weather::library ());
  syntax.add_object ("groundwater", Groundwater::library ());
}

Daisy::~Daisy ()
{
  delete &impl;
  delete &manager;
  delete &weather;
  delete &groundwater;
  delete &columns;
}
