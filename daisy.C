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
#include "field.h"
#include "harvest.h"
#include "action.h"
#include "filter.h"
#include "library.h"
#include "syntax.h"
#include "condition.h"
#include "alist.h"
#include "common.h"
#include "column.h"
#include "submodel.h"

Daisy::Daisy (const AttributeList& al)
  : syntax (NULL),
    alist (al),
    running (false),
    logs (map_create<Log> (al.alist_sequence ("output"))),
    activate_output (Librarian<Condition>::create
		     (al.alist ("activate_output"))),
    time (al.time ("time")),
    action (Librarian<Action>::create (al.alist ("manager"))),
    weather (Librarian<Weather>::create (al.alist ("weather"))), 
    field (*new Field (al.alist_sequence ("column"))),
    harvest (map_construct_const<Harvest> (al.alist_sequence ("harvest")))
{ }

bool
Daisy::check ()
{
  assert (syntax);
  bool all_ok = true;

  // Check field.
  {
    bool ok = true;
    // This was ::const_iterator in g++
    if (!field.check ())
      ok = false;

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
{ field.tick (time, weather); }

void
Daisy::tick_logs ()
{
  if (!activate_output.match (*this))
    return;

  for (unsigned int i = 0; i < logs.size (); i++)
    {
      Log& log = *logs[i];
      Filter& filter = log.match (*this);
      log.output ("time", filter, time);
      output_derived (weather, "weather", log, filter);
      output_submodule (field, "column", log, filter);
      output_vector (harvest, "harvest", log, filter);
      log.done ();
    }
}

void
Daisy::tick ()
{ 
  action.doIt (*this);
  weather.tick (time);

  tick_columns ();
  time.tick_hour ();
  tick_logs ();
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
  syntax = &s; 
  field.initialize (time, weather);
}

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<Harvest>;
#endif

void
Daisy::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Daisy");
  alist.add ("description", "The Daisy Crop/Soil/Atmosphere Model.");
  Library::load_syntax (syntax, alist);

  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this simulation setup.");
  syntax.add ("output", Librarian<Log>::library (),
	      Syntax::Const, Syntax::Sequence,
	      "List of logs for output during the simulation.");
  syntax.add ("activate_output", Librarian<Condition>::library (),
	      "Activate output logs when this condition is true.\n\
You can use the `after' condition to avoid logging during an initialization\n\
period.");
  AttributeList true_alist;
  true_alist.add ("type", "true");
  alist.add ("activate_output", true_alist);
  syntax.add ("input", Librarian<Parser>::library (), Syntax::OptionalConst, 
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
	      "Weather model for providing climate information during \
the simulation.");
  vector<AttributeList*> empty_alist_sequence;
  add_submodule_sequence<Harvest> ("harvest", syntax, Syntax::State, 
				   "Total list of all crop yields.");
  alist.add ("harvest", empty_alist_sequence);
}

Daisy::~Daisy ()
{
  sequence_delete (logs.begin (), logs.end ());
  delete &logs;
  delete &activate_output;
  delete &action;
  delete &weather;
#if 0
  delete &field;
#endif
  delete &harvest;
}

static Submodel::Register 
daisy_submodel ("Daisy", Daisy::load_syntax);
