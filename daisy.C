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
  const Metalib& metalib;
  const FrameModel& frame;
  const boost::scoped_ptr<Scopesel> scopesel;
  const Scope* extern_scope;
  const boost::scoped_ptr<Condition> print_time;
  const boost::scoped_ptr<Output> output_log;
  const Timestep timestep;
  const double max_dt;
  double current_dt;
  Time time;
  Time previous;
  const Time stop;
  int duration;
  const boost::scoped_ptr<Action> action;
  const boost::scoped_ptr<Weather> weather;
  bool running;
  const boost::scoped_ptr<Field> field;
  auto_vector<const Harvest*> harvest;
  
  // Use.
  const Scope& scope ()
  { return extern_scope ? *extern_scope : Scope::null (); }
  const Scope* find_scope (const Scopesel& sel, Treelog& msg) const
  { return sel.lookup (*output_log, msg); }

  // Simulation.
  bool run (Daisy& daisy, Treelog& msg)
  {
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

          print_time->tick (daisy, scope (), msg);
          const bool force_print 
            = print_time->match (daisy, scope (), msg);

          tick (daisy, msg);

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

  void tick (Daisy& daisy, Treelog& msg)
  { 
    // Initial logs.
    output_log->initial_logs (daisy, previous, msg);

    // Weather and management.
    if (weather.get ())
      weather->tick (time, msg);
    action->tick (daisy, scope (), msg);
    action->doIt (daisy, scope (), msg);

    // Turnover and movement.
    field->tick_source (time, msg); 
    current_dt = field->suggest_dt (max_dt); 
    if (current_dt < max_dt)
      {
        std::ostringstream tmp;
        tmp << "Field suggested dt = " << current_dt << ", ignored";
        msg.warning (tmp.str ());
        current_dt = max_dt;
      }
    field->tick_move (metalib, time, current_dt, weather.get (), scope (), msg);

    // Update time.
    previous = time;
    time += timestep;
    if (time >= stop)
      running = false;

    // Log values.
    output_log->tick (daisy, previous, current_dt, msg);

    // Clear values for next timestep.
    field->clear ();

    // Communicate with the UI.
    if (!daisy.ui_running ())
      {
        msg.error ("Simulation aborted");
        running = false;
      }
    if (!running)
      daisy.ui_set_progress (1.0);
    else if (duration > 0)
      {
        const double total_hours = duration; // int -> double
        const double hours_left = Time::hours_between (time, stop);
        daisy.ui_set_progress ((total_hours - hours_left) / total_hours);
      }
    else if (duration != -42)
      {
        duration = -42;           // Magic to call this only once
        daisy.ui_set_progress (-1.0);
      }
  }


  void output (Log& log) const
  {
    output_submodule (time, "time", log);
    output_submodule (previous, "previous", log);
    output_value (current_dt, "dt", log);
    if (weather.get ())
      output_derived (weather, "weather", log);
    output_object (action, "manager", log);
    output_submodule (*field, "column", log);
    output_vector (harvest, "harvest", log);

  }

  // Create and Destroy.
  void initialize (Metalib& metalib, Daisy& daisy, Block& block)
  { 
    Treelog& msg = block.msg ();
    if (weather.get () && !weather->initialize (time, msg))
      return;
    {
      Treelog::Open nest (msg, "output");
      output_log->initialize (metalib, msg);
    }
    extern_scope = find_scope (*scopesel, msg); 
    field->initialize (block, *output_log, time, 
                             weather.get (), scope ());
    {                       
      Treelog::Open nest (msg, "manager");
      action->initialize (daisy, scope (), msg);
    }
  }

  bool check (const Daisy& daisy, Treelog& msg)
  {
    bool ok = true;

    // Check weather.
    {
      Treelog::Open nest (msg, "weather");
      if (weather.get () && !weather->check (time,
                                                         stop, msg))
        return false;
    }

    // Check field.
    {
      Treelog::Open nest (msg, "column");
      if (!field->check (weather.get (), time, stop,
                               scope (), msg))
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
    if (!approximate (max_dt, 1.0))
      {
        std::ostringstream tmp;
        tmp << "Daisy is designed for a timestep of 1 hour, you specified " 
            << max_dt << " hours";
        msg.warning (tmp.str ());
      }
    // Check actions.
    {
      Treelog::Open nest (msg, "manager");
      if (!action->check (daisy, scope (), msg))
        ok = false;
    }
    return ok;
  }

  void summarize (Treelog& msg) const
  {
    output_log->summarize (msg);
    field->summarize (msg);
  }

  Implementation (const BlockModel& al)
    : metalib (al.metalib ()),
      frame (al.frame ()),
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
      previous (al.check ("previous")
                ? Time (al.submodel ("previous"))
                : time - timestep),
      stop (al.check ("stop")
            ? Time (al.submodel ("stop")) 
            : Time (9999, 1, 1, 1)),
      duration (al.check ("stop")
                ? Time::hours_between (stop, time)
                :-1),
      action (Librarian::build_item<Action> (al, "manager")),
      weather (al.check ("weather") 
               ? Librarian::build_item<Weather> (al, "weather")
               : NULL),
      running (false),
      field (new Field (al, "column")),
      harvest (map_submodel_const<Harvest> (al, "harvest"))
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

const Time& 
Daisy::previous () const
{ return impl->previous; }

const Units& 
Daisy::units () const
{ return impl->metalib.units (); }

Field& 
Daisy::field () const
{ return *impl->field; }

std::vector<const Harvest*>&
Daisy::harvest () const
{ return impl->harvest; }

bool 
Daisy::is_running () const
{ return impl->running; }

void 
Daisy::stop ()
{ impl->running = false; }

void 
Daisy::attach_ui (Run* run, const std::vector<Log*>& logs)
{ 
  Program::attach_ui (run, logs);

  for (size_t i = 0; i < logs.size (); i++)
    impl->output_log->add_log (logs[i]);
}

bool
Daisy::run (Treelog& msg)
{ return impl->run (*this, msg); }

void
Daisy::tick (Treelog& msg)
{ impl->tick (*this, msg); }

void
Daisy::output (Log& log) const
{ impl->output (log); }

void
Daisy::initialize (Metalib& metalib, Block& block)
{ impl->initialize (metalib, *this, block); }

bool
Daisy::check (Treelog& msg)
{ return impl->check (*this, msg); }

Daisy::Daisy (const BlockModel& al)
  : Program (al),
    impl (new Implementation (al))
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
  frame.declare_submodule ("time", Attribute::State, "\
Current time in the simulation.", Time::load_syntax);
  frame.declare_submodule ("previous", Attribute::OptionalState, "\
Previous time in the simulation.", Time::load_syntax);
  frame.declare_submodule ("timestep", Attribute::OptionalState, "\
Length of timestep in simlation.\n\
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
