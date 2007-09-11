// daisy.C -- A soil-crop-atmosphere simulation model.
//
// Copyright 1996-2001, 2004 Per Abrahamsen.
// Copyright 2000-2001, 2004 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "daisy.h"
#include "weather.h"
#include "groundwater.h"
#include "horizon.h"
#include "output.h"
#include "log.h"
#include "parser.h"
#include "nitrification.h"
#include "bioclimate.h"
#include "hydraulic.h"
#include "field.h"
#include "harvest.h"
#include "action.h"
#include "timestep.h"
#include "library.h"
#include "syntax.h"
#include "condition.h"
#include "alist.h"
#include "submodeler.h"
#include "column.h"
#include "scope.h"
#include "scopesel.h"
#include "mathlib.h"
#include "memutils.h"
#include "librarian.h"
#include <sstream>

const char *const Daisy::default_description = "\
The Daisy crop/soil/atmosphere model.";

void 
Daisy::attach_ui (Run* run, const std::vector<Log*>& logs)
{ 
  Program::attach_ui (run, logs);

  for (size_t i = 0; i < logs.size (); i++)
    output_log->add_log (logs[i]);
}

bool
Daisy::run (Treelog& msg)
{ 
  daisy_assert (extern_scope);

  // Run simulation.
  {
    Treelog::Open nest (msg, "Running");

    running = false;

    do
      {
	Treelog::Open nest (msg, time.print ());

	if (!running)
	  {
	    running = true;
	    msg.message ("Begin simulation");
	  }

	print_time->tick (*this, *extern_scope, msg);
	const bool force_print 
          = print_time->match (*this, *extern_scope, msg);

	tick (msg);

	if (!running)
	  msg.message ("End simulation");
	print_time->tick (*this, *extern_scope, msg);
	if (force_print)
	  msg.touch ();
      }
    while (running);
  }
  output_log->summarize (msg);
  return true;
}

void
Daisy::tick (Treelog& msg)
{ 
  tick_before (msg);
  tick_columns (msg);
  tick_after (msg);
}

void
Daisy::tick_before (Treelog& msg)
{ 
  daisy_assert (extern_scope);

  output_log->initial_logs (*this, msg);
  if (weather.get ())
    weather->tick (time, msg);
  action->tick (*this, *extern_scope, msg);
  action->doIt (*this, *extern_scope, msg);
}

void
Daisy::tick_columns (Treelog& msg)
{ field->tick_all (time, dt, weather.get (), *extern_scope, msg); }

void
Daisy::tick_column (const size_t col, Treelog& msg)
{ field->tick_one (col, time, dt, weather.get (), *extern_scope, msg); }

void
Daisy::tick_after (Treelog& msg)
{ 
  output_log->tick (*this, msg);
  field->clear ();
  time += *timestep;
  
  if (time >= stop)
    running = false;

  if (!ui_running ())
    {
      msg.error ("Simulation aborted");
      running = false;
    }
  if (!running)
    ui_set_progress (1.0);
  else if (duration > 0)
    {
      const double total_hours = duration; // int -> double
      const double hours_left = Time::hours_between (time, stop);
      ui_set_progress ((total_hours - hours_left) / total_hours);
    }
  else if (duration != -42)
    {
      duration = -42;           // Magic to call this only once
      ui_set_progress (-1.0);
    }
}

void
Daisy::output (Log& log) const
{
  if (weather.get ())
    output_derived (weather, "weather", log);
  output_submodule (*field, "column", log);
  output_vector (harvest, "harvest", log);
  output_derived (action, "manager", log);
}

void
Daisy::initialize (Block& block)
{ 
  Treelog& msg = block.msg ();
  if (weather.get () && !weather->initialize (time, msg))
    return;
  {
    Treelog::Open nest (msg, "output");
    output_log->initialize (metalib, msg);
  }
  extern_scope = scopesel->lookup (*output_log, msg); 
  const Scope& scope = extern_scope ? *extern_scope : Scope::null ();
  field->initialize (block, *output_log, time, weather.get (), scope);
  {                       
    Treelog::Open nest (msg, "manager");
    action->initialize (*this, scope, msg);
  }
}

bool
Daisy::check (Treelog& msg)
{
  bool ok = true;
  const Scope& scope = extern_scope ? *extern_scope : Scope::null ();

  if (!approximate (dt, 1.0))
    {
      std::ostringstream tmp;
      tmp << "Daisy only works with a timestep of 1 hour, you specified " 
          << dt << " hours";
      msg.warning (tmp.str ());
    }

  // Check weather.
  {
    Treelog::Open nest (msg, "weather");
    if (weather.get () && !weather->check (time, stop, msg))
      ok = false;
  }

  // Check field.
  {
    Treelog::Open nest (msg, "column");
    if (!field->check (!weather.get (), time, stop, scope, msg))
      ok = false;
  }
  // Check logs.
  {
    Treelog::Open nest (msg, "output");
    if (!output_log->check (*field, msg))
      ok = false;
  }
  if (!extern_scope)
    {
      msg.error ("Extern scope not found");
      ok = false;
    }
  // Check actions.
  {
    Treelog::Open nest (msg, "manager");
    if (!action->check (*this, scope, msg))
      ok = false;
  }
  return ok;
}

Daisy::Daisy (Block& al)
  : Program (al),
    metalib (al.metalib ()),
    running (false),
    output_log (new Output (al)),
    scopesel (Librarian::build_item<Scopesel> (al, "scope")),
    extern_scope (NULL),
    print_time (Librarian::build_item<Condition> (al, "print_time")),
    time (al.alist ("time")),
    timestep (al.check ("timestep") 
              ? submodel<Timestep> (al, "timestep")
              : new Timestep (0, 0, 1, 0, 0)),
    dt (timestep->total_hours ()),
    stop (al.check ("stop")
	  ? Time (al.alist ("stop")) 
	  : Time (9999, 1, 1, 1)),
    duration (al.check ("stop")
              ? Time::hours_between (stop, time)
              :-1),
    action (Librarian::build_item<Action> (al, "manager")),
    weather (al.check ("weather") 
	     ? Librarian::build_item<Weather> (al, "weather")
	     : NULL), 
    field (new Field (al, "column")),
    harvest (map_submodel_const<Harvest> (al, "harvest"))
{ }

void
Daisy::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this simulation setup.");
  alist.add ("description", default_description);
  
  Output::load_syntax (syntax, alist);
  syntax.add_object ("scope", Scopesel::component, 
		     Syntax::Const, Syntax::Singleton, "\
Scope to evaluate expessions in.");
  alist.add ("scope", Scopesel::default_model ());
  syntax.add_object ("print_time", Condition::component,
                     "Print simulation time whenever this condition is true.\n\
The simulation time will also be printed whenever there are any news\n\
to report, like emergence of crop or various management operations.\n\
Good values for this parameter would be hourly, daily or monthly.");
  AttributeList false_alist;
  false_alist.add ("type", "false");
  alist.add ("print_time", false_alist);

  syntax.add_object ("manager", Action::component, Syntax::State,
                     Syntax::Singleton,
                     "Specify the management operations to perform during\n\
the simulation.");
  syntax.add_submodule ("time", alist, Syntax::State,
			"Current time in the simulation.", Time::load_syntax);
  syntax.add_submodule ("timestep", alist, Syntax::OptionalState,
			"Length of timestep in simlation.\n\
The default value is 1 hour, anything else is unlikely to work.",
                        Timestep::load_syntax);
  syntax.add_check ("timestep", Timestep::positive ());
  syntax.add_submodule ("stop", alist, Syntax::OptionalConst,
			"Latest time where the simulation stops.\n\
By default, the simulation will run until the manager request it to stop.",
                        Time::load_syntax);
  syntax.add_object ("column", Column::component, 
                     Syntax::State, Syntax::Sequence,
                     "List of columns to use in this simulation.");
  syntax.add_object ("weather", Weather::component,
                     Syntax::OptionalState, Syntax::Singleton,
                     "Weather model for providing climate information during\n\
the simulation.  Can be overwritten by column specific weather.");
  syntax.add_submodule_sequence ("harvest", Syntax::State, 
				 "Total list of all crop yields.",
				 Harvest::load_syntax);
  alist.add ("harvest", std::vector<AttributeList*> ());
}

Daisy::~Daisy ()
{ }

static struct ProgramDaisySyntax
{
  static Model& make (Block& al)
  { return *new Daisy (al); }
  ProgramDaisySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Daisy::load_syntax (syntax, alist);
    alist.add ("description", "A soil-crop-atmosphere simulation model.");
    Librarian::add_type (Program::component, "Daisy", alist, syntax, &make);
  }
} ProgramDaisy_syntax;

// daisy.C ends here.
