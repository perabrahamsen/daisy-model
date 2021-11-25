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
#include "wsource.h"
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

class Daisy::Implementation
{
public:
  // Content.
  const Metalib& metalib;
  const FrameModel& frame;
  const std::unique_ptr<Scopesel> scopesel;
  const Scope* extern_scope;
  const std::unique_ptr<Condition> print_time;
  const std::unique_ptr<Output> output_log;
  const bool message_timestep;
  const Timestep timestep;
  const double max_dt;
  const Timestep minimal_timestep;
  const double min_dt;
  double small_dt;
  double current_dt;
  Time time;
  Time previous;
  Time next_large;
  const Time stop;
  double duration;
  const std::unique_ptr<Condition> stop_when;
  const std::unique_ptr<Action> action;
  const std::unique_ptr<WSource> weather;
  bool initialized;
  bool running;
  const std::unique_ptr<Field> field;
  auto_vector<const Harvest*> harvest;

  const Scope& scope ()
  { return extern_scope ? *extern_scope : Scope::null (); }

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

          stop_when->tick (daisy, scope (), msg);
	  if (stop_when->match (daisy, scope (), msg))
	    running = false;

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

  static void prettify_time (const Time& lim, Time& val)
  // Crop the least significant part of val, but stay larger than lim.
  {
    daisy_assert (val > lim);
    Time test = val;
    test.tick_microsecond (-test.microsecond ());
    if (test <= lim)
      return;
    val = test;
    test.tick_second (-test.second ());
    if (test <= lim)
      return;
    val = test;
    test.tick_minute (-test.minute ());
    if (test <= lim)
      return;
    val = test;
    return;
  }

  void tick (Daisy& daisy, Treelog& msg);

  void output (Log& log) const
  {
    output_submodule (time, "time", log);
    output_submodule (previous, "previous", log);
    output_submodule (next_large, "next_large", log);
    output_value (current_dt, "dt", log);
    if (weather.get ())
      output_derived (weather, "weather", log);
    output_object (action, "manager", log);
    output_submodule (*field, "column", log);
    output_ordered (harvest, "harvest", log);

  }

  // Create and Destroy.
  void initialize (Daisy& daisy, Block& block)
  { 
    Treelog& msg = block.msg ();
    if (weather.get ())
      weather->weather_initialize (time, msg);

    {
      Treelog::Open nest (msg, "output");
      output_log->initialize (metalib, msg);
    }
    extern_scope = scopesel->lookup (output_log->scopes (), msg); 
    if (!field->initialize (block, output_log->scopes (), time, 
                            weather.get (), scope ()))
      throw "Initialize failed";
    {                       
      Treelog::Open nest (msg, "manager");
      action->initialize (daisy, scope (), msg);
    }

    initialized = true;
  }

  bool check (const Daisy& daisy, Treelog& msg)
  {
    if (!initialized)
      return false;

    bool ok = true;

    // Check weather.
    {
      if (weather.get () && !weather->weather_check (time, stop, msg))
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
    if (stop != Time::null () && stop <= time)
      msg.warning ("Simulation set to stop at " + stop.print ()
		   + " before it starts at " + time.print ());

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
      message_timestep (al.flag ("message_timestep")),
      timestep (al.check ("timestep") 
                ? submodel_value<Timestep> (al, "timestep")
                : Timestep::hour ()),
      max_dt (timestep.total_hours ()),
      minimal_timestep (al.check ("minimal_timestep")
                        ? submodel_value<Timestep> (al, "minimal_timestep")
                        : Timestep::microsecond ()),
      min_dt (minimal_timestep.total_hours ()),
      small_dt (max_dt),
      current_dt (max_dt),
      time (al.submodel ("time")),
      previous (al.check ("previous")
                ? Time (al.submodel ("previous"))
                : time - timestep),
      next_large (al.check ("next_large")
                  ? Time (al.submodel ("next_large"))
                  : time + timestep),
      stop (al.check ("stop")
            ? Time (al.submodel ("stop")) 
            : Time::null ()),
      duration (al.check ("stop")
                ? Time::fraction_hours_between (time, stop)
                :-1),
      stop_when (Librarian::build_item<Condition> (al, "stop_when")),
      action (Librarian::build_item<Action> (al, "manager")),
      weather (al.check ("weather") 
               ? Librarian::build_item<WSource> (al, "weather")
               : NULL),
      initialized (false),
      running (false),
      field (new Field (al, "column")),
      harvest (map_submodel_const<Harvest> (al, "harvest"))
  { }
};

void 
Daisy::Implementation::tick (Daisy& daisy, Treelog& msg)
{ 
  // Initial logs.
  output_log->initial_logs (daisy, time, msg);

  // Weather and management.

  if (weather.get ())
    weather->weather_tick (time + timestep, msg);

  action->tick (daisy, scope (), msg);
  action->doIt (daisy, scope (), msg);

  // Find sources.
  field->tick_source (scope (), time, msg); 

  // Find next timestep.
  Time next_time = Time::null ();

  if (approximate (min_dt, max_dt))
    // Fixed timesteps.
    {
      next_time = time + timestep;
      next_large = next_time + timestep;
      current_dt = max_dt;
    }
  else
    // Source limited timestep.
    {
      const double weather_dt = weather.get () 
        ? weather->suggest_dt ()
        : 0.0;
      const double T_air = weather.get () 
        ? weather->air_temperature ()
        : 42.42e42;

      double suggested_dt = field->suggest_dt (weather_dt, T_air); 
      if (!std::isnormal (suggested_dt))
        suggested_dt = max_dt;

      if (suggested_dt < min_dt)
        {
	  if (!daisy_full_debug ())
	    {
	      static const double us = 1.0 / (60.0 * 60.0 * 1000000.0);
	      std::ostringstream tmp;
	      tmp << "Suggested timestep too small: " << suggested_dt << " (" ;
	      if (suggested_dt < 1.0 * us)
		tmp << suggested_dt / us << " [us]";
	      else
		tmp  << Timestep::build_hours (suggested_dt).print ();
	      tmp << ") < " << min_dt
		  << " (" << minimal_timestep.print () << ")";
	      msg.warning (tmp.str ());
	    }
          suggested_dt = min_dt;
        }
        

      // Time remaining of current large timestep.
      daisy_assert (next_large > time);
      const Timestep ts_time_left = next_large - time;
      const double ts_hours_left = ts_time_left.total_hours ();
      daisy_assert (ts_hours_left > 0.0);

      // Find next time.
      if (suggested_dt >= ts_hours_left)
        {
          // We can complete the current large timestep in one go.
          current_dt = ts_hours_left;
          next_time = next_large;
          next_large += timestep;
        }
      else if (suggested_dt * 2.0 > ts_hours_left)
        {
          // We split the remaining time in two, and round down.
          next_time = time + ts_time_left / 2;
          prettify_time (time, next_time);
          Timestep ts = next_time - time;
          current_dt = ts.total_hours ();
        }
      else 
        {
          // Use something close to suggested value.
          Timestep approx = Timestep::build_hours (suggested_dt);
          next_time = time + approx;
          prettify_time (time, next_time);
          Timestep ts = next_time - time;
          current_dt = ts.total_hours ();
        }

      if (current_dt < small_dt)
        {
          small_dt = current_dt;
          if (message_timestep)
            {
              std::ostringstream tmp;
              tmp << "Using small timestep: " 
                  << Timestep::build_hours (current_dt).print ();
              msg.message (tmp.str ());
            }
        }
      else if (small_dt < max_dt * 0.99 && approximate (current_dt, max_dt))
        {
          small_dt = max_dt;
          if (message_timestep)
            msg.message ("Back to normal size timesteps");
        }
    }
    
  if (weather.get ())
    weather->weather_tick (next_time, msg);

  field->tick_move (metalib, time, next_time, current_dt, weather.get (),
                    scope (), msg);

  // Update time.
  previous = time;
  time = next_time;
  if (stop != Time::null () && time >= stop)
    running = false;

  // Log values.
  output_log->tick (daisy, time, current_dt, msg);

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
      const double hours_left = Time::fraction_hours_between (time, stop);
      daisy.ui_set_progress ((total_hours - hours_left) / total_hours);
    }
  else if (std::isfinite (duration))
    {
      duration = NAN;           // Magic to call this only once
      daisy.ui_set_progress (-1.0);
    }
}

const FrameModel&
Daisy::frame () const
{ return impl->frame; }

const std::vector<const Scope*>& 
Daisy::scopes () const
{ return impl->output_log->scopes (); }

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

void
Daisy::start ()
{ impl->running = true; }

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
Daisy::initialize (Block& block)
{ impl->initialize (*this, block); }

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
  frame.declare_submodule ("next_large", Attribute::OptionalState, "\
End of next large timestep.", Time::load_syntax);
  frame.declare_boolean ("message_timestep", Attribute::Const, "\
Show messages about timestep modifications.");
  frame.set ("message_timestep", true);
  frame.declare_submodule ("timestep", Attribute::OptionalState, "\
Length of large timestep in simulation.\n\
The default value is 1 hour, anything else is unlikely to work.",
                           Timestep::load_syntax);
  frame.set_check ("timestep", Timestep::positive ());
  frame.declare_submodule ("minimal_timestep", Attribute::OptionalState, "\
Minimum length of timestep in simulation.\n\
By default, this is the same as 'timestep'.",
                           Timestep::load_syntax);
  frame.declare ("dt", "h", Attribute::LogOnly, "\
Current timestep used by simulation.");
  frame.declare_submodule ("stop", Attribute::OptionalConst,
			"Latest time where the simulation stops.\n\
By default, the simulation will run until the manager request it to stop.",
                        Time::load_syntax);
  frame.declare_object ("stop_when", Condition::component,
			"Stop when this is true.");
  frame.set ("stop_when", "false");
  frame.declare_object ("column", Column::component, 
                        Attribute::State, Attribute::Variable,
                        "List of columns to use in this simulation.");
  frame.declare_object ("weather", WSource::component,
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
