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
#include "field.h"
#include "harvest.h"
#include "action.h"
#include "library.h"
#include "syntax.h"
#include "condition.h"
#include "alist.h"
#include "common.h"
#include "column.h"
#include "submodel.h"

const char *const Daisy::default_description = "\
The Daisy Crop/Soil/Atmosphere Model.";

Daisy::Daisy (const AttributeList& al)
  : syntax (NULL),
    alist (al),
    running (false),
    logs (map_create<Log> (al.alist_sequence ("output"))),
    activate_output (Librarian<Condition>::create
		     (al.alist ("activate_output"))),
    time (al.time ("time")),
    action (Librarian<Action>::create (al.alist ("manager"))),
    weather (al.check ("weather") 
	     ? &Librarian<Weather>::create (al.alist ("weather"))
	     : NULL), 
    field (*new Field (al.alist_sequence ("column"))),
    harvest (map_construct_const<Harvest> (al.alist_sequence ("harvest")))
{ }

bool
Daisy::check (Treelog& err)
{
  assert (syntax);
  bool ok = true;

  // Check weather.
  {
    Treelog::Open nest (err, "weather");
    if (weather && !weather->check (time, time, err))
      ok = false;
  }

  // Check field.
  {
    Treelog::Open nest (err, "column");
    if (!field.check (weather == NULL, time, time, err))
      ok = false;
  }
  // Check logs.
  {
    Treelog::Open nest (err, "output");
    for (vector<Log*>::const_iterator i = logs.begin ();
	 i != logs.end ();
	 i++)
      {
	if (*i == NULL || !(*i)-> check (*syntax, err))
	  ok = false;
      }
  }
  // Check actions.
  {
    Treelog::Open nest (err, "manager");
    if (!action.check (*this, err))
      ok = false;
  }
  return ok;
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
      if (log.match (*this))
	{
	  log.output ("time", time);
	  if (weather)
	    output_derived (*weather, "weather", log);
	  output_submodule (field, "column", log);
	  output_vector (harvest, "harvest", log);
	  output_derived (action, "manager", log);
	  log.done ();
	}
    }
}

void
Daisy::tick ()
{ 
  if (weather)
    weather->tick (time);
  action.doIt (*this);

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
  syntax = &s; 
  if (weather)
    weather->initialize (time);
  field.initialize (time, weather);
}

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<Harvest>;
#endif

void
Daisy::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Daisy");
  alist.add ("description", Daisy::default_description);

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
  syntax.add ("manager", Librarian<Action>::library (), Syntax::State,
	      Syntax::Singleton,
	      "Specify the management operations to perform during\n\
the simulation.");
  syntax.add ("time", Syntax::Date, Syntax::State,
	      "Current time in the simulation.");
  syntax.add ("column",
	      Librarian<Column>::library (), 
	      Syntax::State, Syntax::Sequence,
	      "List of columns to use in this simulation.");
  syntax.add ("weather", Librarian<Weather>::library (),
	      Syntax::OptionalState, Syntax::Singleton,
	      "Weather model for providing climate information during\n\
the simulation.  Can be overwritten by column specific weather.");
  add_submodule_sequence<Harvest> ("harvest", syntax, Syntax::State, 
				   "Total list of all crop yields.");
  alist.add ("harvest", vector<AttributeList*> ());
}

Daisy::~Daisy ()
{
  sequence_delete (logs.begin (), logs.end ());
  delete &logs;
  delete &activate_output;
  delete &action;
  if (weather)
    delete weather;
  delete &field;
  delete &harvest;
}

static Submodel::Register 
daisy_submodel ("Daisy", Daisy::load_syntax);
