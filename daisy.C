// daisy.C

#include "daisy.h"
#include "input.h"
#include "manager.h"
#include "weather.h"
#include "log.h"
#include "column.h"
#include "action.h"
#include "filter.h"
#include "library.h"
#include "syntax.h"
#include "condition.h"
#include <iostream.h>

struct Daisy::Implementation
{   
  Implementation () { };
};

Daisy::Daisy (const Input& input)
  : impl (*new Implementation ()), 
    log (input.makeLog ()), 
    time (input.makeTime ()),
    manager (input.makeManager ()),
    weather (input.makeWeather ()), 
    groundwater (input.makeGroundwater ()), 
    columns (input.makeColumns ())
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

Daisy::~Daisy ()
{
  delete &manager;
  delete &weather;
  delete &log;
  delete &impl;
  for (ColumnList::iterator column = columns.begin();
       column != columns.end();
       column++)
    delete *column;
}
