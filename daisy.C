// daisy.C -- Main module
//
// Copyright 1996-2001 Per Abrahamsen.
// Copyright 2000-2001 KVL.
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

#include "daisy.h"
#include "weather.h"
#include "groundwater.h"
#include "horizon.h"
#include "log_all.h"
#include "parser.h"
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
#include "tmpstream.h"

using namespace std;

const char *const Daisy::default_description = "\
The Daisy Crop/Soil/Atmosphere Model.";

const vector<Log*> 
Daisy::find_active_logs (const vector<Log*>& logs, LogAll& log_all)
{
  vector<Log*> result;

  for (unsigned int i = 0; i < logs.size (); i++)
    if (!dynamic_cast<LogSelect*> (logs[i]))
      result.push_back (logs[i]);
  
  result.push_back (&log_all);
  
  return result;
}

Daisy::Daisy (const AttributeList& al)
  : syntax (NULL),
    alist (al),
    running (false),
    logging (false),
    logs (map_create<Log> (al.alist_sequence ("output"))),
    log_all (*new LogAll (logs)),
    active_logs (find_active_logs (logs, log_all)),
    activate_output (Librarian<Condition>::create
		     (al.alist ("activate_output"))),
    print_time (Librarian<Condition>::create
		     (al.alist ("print_time"))),
    time (al.alist ("time")),
    stop (al.check ("stop") ? Time (al.alist ("stop")) : Time (9999, 1, 1, 1)),
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
  daisy_assert (syntax);
  bool ok = true;

  // Check weather.
  {
    Treelog::Open nest (err, "weather");
    if (weather && !weather->check (time, stop, err))
      ok = false;
  }

  // Check field.
  {
    Treelog::Open nest (err, "column");
    if (!field.check (weather == NULL, time, stop, err))
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
Daisy::tick_columns (Treelog& out)
{ field.tick (out, time, weather); }

void
Daisy::initial_logs (Treelog& out)
{
  activate_output.tick (*this, out);

  if (activate_output.match (*this))
    {
      if (!logging)
	{
	  out.message ("Start logging");
	  // get initial values for previous day.
	  Time previous (time);
	  previous.tick_hour (-1);
	  for (unsigned int i = 0; i < active_logs.size (); i++)
	    {
	      Log& log = *active_logs[i];
	      if (log.initial_match (*this, out))
		{
		  output_submodule (previous, "time", log);
		  if (weather)
		    output_derived (*weather, "weather", log);
		  output_submodule (field, "column", log);
		  output_vector (harvest, "harvest", log);
		  output_derived (action, "manager", log);
		  log.initial_done (previous);
		}
	    }
	}
      logging = true;
    }
  else if (logging)
    {
      out.message ("End logging");
      logging = false;
    }
}

void
Daisy::tick_logs (Treelog& out)
{
  if (logging)
    {
      for (unsigned int i = 0; i < active_logs.size (); i++)
	{
	  Log& log = *active_logs[i];
	  if (log.match (*this, out))
	    {
	      output_submodule (time, "time", log);
	      if (weather)
		output_derived (*weather, "weather", log);
	      output_submodule (field, "column", log);
	      output_vector (harvest, "harvest", log);
	      output_derived (action, "manager", log);
	      log.done (time);
	    }
	}
    }

  // KLUDGE: Clean up field *after* log.
  // Should be moved to separate function, but consider C interface.
  field.clear ();
}

void
Daisy::tick (Treelog& out)
{ 
  initial_logs (out);
  if (weather)
    weather->tick (time, out);
  action.tick (*this, out);
  action.doIt (*this, out);

  tick_columns (out);
  tick_logs (out);
  time.tick_hour ();
  
  if (time >= stop)
    running = false;
}

void 
Daisy::run (Treelog& out)
{ 
  // Run simulation.
  {
    Treelog::Open nest (out, "Running");

    running = false;

    do
      {
	TmpStream tmp;
	tmp () << time.year () << "-" << time.month () << "-" 
	       << time.mday () << "h" << time.hour ();
	Treelog::Open nest (out, tmp.str ());

	if (!running)
	  {
	    running = true;
	    out.message ("Begin simulation");
	  }

	print_time.tick (*this, out);
	const bool force_print = print_time.match (*this);

	tick (out);

	if (!running)
	  out.message ("End simulation");
	print_time.tick (*this, out);
	if (force_print)
	  out.touch ();
      }
    while (running);
  }
  // Print log file summaries at end of simulation.
  {
    Treelog::Open nest (out, "Summary");
    for (unsigned int i = 0; i < logs.size (); i++)
      logs[i]->summarize (out);
  }
}

void
Daisy::initialize (const Syntax& s, Treelog& err)
{ 
  syntax = &s; 
  if (weather)
    weather->initialize (time, err);
  field.initialize (time, err, weather);
  {
    Treelog::Open nest (err, "output");
    for (unsigned int i = 0; i < logs.size (); i++)
      logs[i]->initialize (err);
  }
}

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
You can use the 'after' condition to avoid logging during an initialization\n\
period.");
  AttributeList true_alist;
  true_alist.add ("type", "true");
  alist.add ("activate_output", true_alist);
  syntax.add ("print_time", Librarian<Condition>::library (),
	      "Print simulation time whenever this condition is true.\n\
The simulation time will also be printed whenever there are any news\n\
to report, like emergence of crop or various management operations.\n\
Good values for this parameter would be hourly, daily or monthly.");
  AttributeList false_alist;
  false_alist.add ("type", "false");
  alist.add ("print_time", false_alist);

  syntax.add ("directory", Syntax::String, Syntax::OptionalConst,
	      "Run simulation in this directory.\n\
This can affect both where input files are found and where log files\n\
are generated.");
  syntax.add ("path", Syntax::String, Syntax::OptionalConst, Syntax::Sequence,
	      "List of directories to search for input files in.\n\
The special value \".\" means the current directory.");
  syntax.add ("input", Librarian<Parser>::library (), Syntax::OptionalConst, 
	      Syntax::Singleton,
	      "Command to add more information about the simulation.");
  syntax.add ("manager", Librarian<Action>::library (), Syntax::State,
	      Syntax::Singleton,
	      "Specify the management operations to perform during\n\
the simulation.");
  syntax.add_submodule ("time", alist, Syntax::State,
			"Current time in the simulation.", Time::load_syntax);
  syntax.add_submodule ("stop", alist, Syntax::OptionalConst,
			"Latest time where the simulation stops.\n\
By default, the simulation will run until the manager request it to stop.",
                        Time::load_syntax);
  syntax.add ("column",
	      Librarian<Column>::library (), 
	      Syntax::State, Syntax::Sequence,
	      "List of columns to use in this simulation.");
  syntax.add ("weather", Librarian<Weather>::library (),
	      Syntax::OptionalState, Syntax::Singleton,
	      "Weather model for providing climate information during\n\
the simulation.  Can be overwritten by column specific weather.");
  syntax.add_submodule_sequence ("harvest", Syntax::State, 
				 "Total list of all crop yields.",
				 Harvest::load_syntax);
  alist.add ("harvest", vector<AttributeList*> ());
}

Daisy::~Daisy ()
{
  sequence_delete (logs.begin (), logs.end ());
  delete &log_all;
  delete &activate_output;
  delete &print_time;
  delete &action;
  if (weather)
    delete weather;
  delete &field;
  sequence_delete (harvest.begin (), harvest.end ());
}

static Submodel::Register 
daisy_submodel ("Daisy", Daisy::load_syntax);
