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
#include "condition.h"
#include "submodeler.h"
#include "column.h"
#include "scope.h"
#include "scopesel.h"
#include "mathlib.h"
#include "memutils.h"
#include "librarian.h"
#include "metalib.h"
#include "treelog.h"
#include "frame_model.h"
#include <sstream>

const char *const Daisy::default_description = "\
The Daisy crop/soil/atmosphere model.";

struct Daisy::Implementation
{
  // Content.
  const FrameModel& frame;
  const boost::scoped_ptr<Scopesel> scopesel;
  const Scope* extern_scope;
  const boost::scoped_ptr<Condition> print_time;
  const boost::scoped_ptr<Output> output_log;
  const Timestep timestep;
  const double max_dt;
  double current_dt;
  Time time;
  const Time stop;
  int duration;
  const boost::scoped_ptr<Action> action;
  const boost::scoped_ptr<Weather> weather;
  
  // Use.
  const Scope& scope ()
  { return extern_scope ? *extern_scope : Scope::null (); }
  const Scope* find_scope (const Scopesel& sel, Treelog& msg) const
  { return sel.lookup (*output_log, msg); }

  // Simulation.
  void tick_before (Daisy& daisy, Treelog& msg)
  { 
    const Time previous = time - timestep;
    output_log->initial_logs (daisy, previous, msg);
    if (weather.get ())
      weather->tick (time, msg);
    action->tick (daisy, scope (), msg);
    action->doIt (daisy, scope (), msg);
  }

  void output (Log& log) const
  {
    output_value (current_dt, "dt", log);
    if (weather.get ())
      output_derived (weather, "weather", log);
    output_object (action, "manager", log);
  }
  // Create and Destroy.
  void initialize (Treelog& msg)
  { extern_scope = find_scope (*scopesel, msg); }
  bool check (Treelog& msg)
  {
    bool ok = true;
    if (!extern_scope)
      {
        msg.error ("Extern scope not found");
        ok = false;
      }
    if (!approximate (max_dt, 1.0))
      {
        std::ostringstream tmp;
        tmp << "Daisy is designed for a timestep of 1 hour, you specified " 
            << max_dt << " hours";
        msg.warning (tmp.str ());
      }
    return ok;
  }
  Implementation (const BlockModel& al)
    : frame (al.frame ()),
      scopesel (Librarian::build_item<Scopesel> (al, "scope")),
      extern_scope (NULL),
      print_time (Librarian::build_item<Condition> (al, "print_time")),
      output_log (new Output (al)),
      timestep (al.check ("timestep") 
                ? submodel_value<Timestep> (al, "timestep")
                : Timestep::hour ()),
      max_dt (timestep.total_hours ()),
      current_dt (max_dt),
      time (al.submodel ("time")),
      stop (al.check ("stop")
            ? Time (al.submodel ("stop")) 
            : Time (9999, 1, 1, 1)),
      duration (al.check ("stop")
                ? Time::hours_between (stop, time)
                :-1),
      action (Librarian::build_item<Action> (al, "manager")),
      weather (al.check ("weather") 
               ? Librarian::build_item<Weather> (al, "weather")
               : NULL)
  { }
};

const FrameModel&
Daisy::frame () const
{ return impl->frame; }

const Scope* 
Daisy::find_scope (const Scopesel& sel, Treelog& msg) const
{ return impl->find_scope (sel, msg); }

Scope&
Daisy::find_scope (const size_t index) const
{ return impl->output_log->scope (index); }


size_t 
Daisy::scope_size () const
{ return impl->output_log->scope_size (); }

const Time& 
Daisy::time () const
{ return impl->time; }

const Timestep& 
Daisy::timestep () const
{ return impl->timestep; }

const Units& 
Daisy::units () const
{ return metalib.units (); }

void 
Daisy::attach_ui (Run* run, const std::vector<Log*>& logs)
{ 
  Program::attach_ui (run, logs);

  for (size_t i = 0; i < logs.size (); i++)
    impl->output_log->add_log (logs[i]);
}

bool
Daisy::run (Treelog& msg)
{
  // Run simulation.
  {
    Treelog::Open nest (msg, "Running");

    running = false;

    do
      {
	Treelog::Open nest (msg, impl->time.print ());

	if (!running)
	  {
	    running = true;
	    msg.message ("Begin simulation");
	  }

	impl->print_time->tick (*this, impl->scope (), msg);
	const bool force_print 
          = impl->print_time->match (*this, impl->scope (), msg);

	tick (msg);

	if (!running)
	  msg.message ("End simulation");
	if (force_print)
	  {
	    msg.touch ();
	    msg.flush ();
	  }
      }
    while (running);
  }
  summarize (msg);
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
{ impl->tick_before (*this, msg); }

void
Daisy::tick_columns (Treelog& msg)
{ field->tick_all (metalib, impl->time, impl->current_dt, 
                   impl->weather.get (), impl->scope (), msg); }

void
Daisy::tick_column (const size_t col, Treelog& msg)
{ field->tick_one (metalib, col, impl->time, impl->current_dt,
                   impl->weather.get (), impl->scope (), msg); }

void
Daisy::tick_after (Treelog& msg)
{ 
  const Time next = impl->time + impl->timestep;
  if (next >= impl->stop)
    running = false;

  impl->output_log->tick (*this, impl->time, impl->current_dt, msg);
  field->clear ();
  impl->time = next;
  
  if (!ui_running ())
    {
      msg.error ("Simulation aborted");
      running = false;
    }
  if (!running)
    ui_set_progress (1.0);
  else if (impl->duration > 0)
    {
      const double total_hours = impl->duration; // int -> double
      const double hours_left = Time::hours_between (impl->time, impl->stop);
      ui_set_progress ((total_hours - hours_left) / total_hours);
    }
  else if (impl->duration != -42)
    {
      impl->duration = -42;           // Magic to call this only once
      ui_set_progress (-1.0);
    }
}

void
Daisy::output (Log& log) const
{
  impl->output (log);
  output_submodule (*field, "column", log);
  output_vector (harvest, "harvest", log);
}

void
Daisy::initialize (Metalib& metalib, Block& block)
{ 
  Treelog& msg = block.msg ();
  if (impl->weather.get () && !impl->weather->initialize (impl->time, msg))
    return;
  {
    Treelog::Open nest (msg, "output");
    impl->output_log->initialize (metalib, msg);
  }
  impl->initialize (msg);
  field->initialize (block, *impl->output_log, impl->time, 
                     impl->weather.get (), impl->scope ());
  {                       
    Treelog::Open nest (msg, "manager");
    impl->action->initialize (*this, impl->scope (), msg);
  }
}

bool
Daisy::check (Treelog& msg)
{
  bool ok = true;

  // Check weather.
  {
    Treelog::Open nest (msg, "weather");
    if (impl->weather.get () && !impl->weather->check (impl->time,
                                                       impl->stop, msg))
      return false;
  }

  // Check field.
  {
    Treelog::Open nest (msg, "column");
    if (!field->check (impl->weather.get (), impl->time, impl->stop,
                       impl->scope (), msg))
      ok = false;
  }
  // Check logs.
  {
    Treelog::Open nest (msg, "output");
    if (!impl->output_log->check (*field, msg))
      ok = false;
  }
  // Implementation.
  if (!impl->check (msg))
    ok = false;

  // Check actions.
  {
    Treelog::Open nest (msg, "manager");
    if (!impl->action->check (*this, impl->scope (), msg))
      ok = false;
  }
  return ok;
}

Daisy::Daisy (const BlockModel& al)
  : Program (al),
    impl (new Implementation (al)),
    metalib (al.metalib ()),
    running (false),
    field (new Field (al, "column")),
    harvest (map_submodel_const<Harvest> (al, "harvest"))
{ }

void
Daisy::load_syntax (Frame& frame)
{
  Model::load_model (frame);
  
  Output::load_syntax (frame);
  frame.declare_object ("scope", Scopesel::component, 
		     Attribute::Const, Attribute::Singleton, "\
Scope to evaluate expessions in.");
  frame.set ("scope", "null");
  frame.declare_object ("print_time", Condition::component,
                     "Print simulation time whenever this condition is true.\n\
The simulation time will also be printed whenever there are any news\n\
to report, like emergence of crop or various management operations.\n\
Good values for this parameter would be hourly, daily or monthly.");
  frame.set ("print_time", "periodic");

  frame.declare_object ("manager", Action::component, Attribute::State,
                     Attribute::Singleton,
                     "Specify the management operations to perform during\n\
the simulation.");
  frame.declare_submodule ("time", Attribute::State,
			"Current time in the simulation.", Time::load_syntax);
  frame.declare_submodule ("timestep", Attribute::OptionalState,
			"Length of timestep in simlation.\n\
The default value is 1 hour, anything else is unlikely to work.",
                        Timestep::load_syntax);
  frame.set_check ("timestep", Timestep::positive ());
  frame.declare ("dt", "h", Attribute::LogOnly, "\
Current timestep used by simulation.");
  frame.declare_submodule ("stop", Attribute::OptionalConst,
			"Latest time where the simulation stops.\n\
By default, the simulation will run until the manager request it to stop.",
                        Time::load_syntax);
  frame.declare_object ("column", Column::component, 
                        Attribute::State, Attribute::Variable,
                        "List of columns to use in this simulation.");
  frame.declare_object ("weather", Weather::component,
                     Attribute::OptionalState, Attribute::Singleton,
                     "Weather model for providing climate information during\n\
the simulation.  Can be overwritten by column specific weather.");
  frame.declare_submodule_sequence ("harvest", Attribute::State, 
				 "Total list of all crop yields.",
				 Harvest::load_syntax);
  frame.set_empty ("harvest");
}

void
Daisy::summarize (Treelog& msg) const
{
  impl->output_log->summarize (msg);
  field->summarize (msg);
}

Daisy::~Daisy ()
{ }

static struct ProgramDaisySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new Daisy (al); }
  ProgramDaisySyntax ()
    : DeclareModel (Program::component, "Daisy", Daisy::default_description)
  { }

  void load_frame (Frame& frame) const
  {
    Daisy::load_syntax (frame);
    
    frame.set_strings ("cite", "daisy-def", "daisy-new", "daisy-fertilizer");
  }
} ProgramDaisy_syntax;

// daisy.C ends here.
