// output.C -- Handle output from a Daisy simulation.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "output.h"
#include "daisy.h"
#include "log_all.h"
#include "log_extern.h"
#include "treelog.h"
#include "time.h"
#include "timestep.h"
#include "block.h"
#include "syntax.h"
#include "assertion.h"
#include "librarian.h"

static void
operator++ (Time::component_t& val, int)
{ val = Time::component_t (val + 1); }

void
Output::initial_logs (const Daisy& daisy, Treelog& msg)
{
  activate_output->tick (daisy, Scope::null (), msg);

  if (activate_output->match (daisy, Scope::null (), msg))
    {
      if (!logging)
	{
	  msg.message ("Start logging");
	  // get initial values for previous day.
	  const Time previous = daisy.time - daisy.timestep;
	  for (size_t i = 0; i < active_logs.size (); i++)
	    {
	      Log& log = *active_logs[i];
	      if (log.initial_match (daisy, msg))
		{
		  output_submodule (previous, "time", log);
                  daisy.output (log);
                  output_list (logs, "output", log, Log::component);
		  log.initial_done (time_columns, previous, daisy.dt);
		}
	    }
	}
      logging = true;
    }
  else if (logging)
    {
      msg.message ("End logging");
      logging = false;
    }
}

void
Output::tick (const Daisy& daisy, Treelog& msg)
{
  if (logging)
    {
      for (size_t i = 0; i < active_logs.size (); i++)
	{
	  Log& log = *active_logs[i];
	  if (log.match (daisy, msg))
	    {
	      output_submodule (daisy.time, "time", log);
              daisy.output (log);
              output_list (logs, "output", log, Log::component);
	      log.done (time_columns, daisy.time, daisy.dt);
	    }
	}
    }
}

void
Output::summarize (Treelog& msg) const
{
  // Print log file summaries at end of simulation.
  {
    Treelog::Open nest (msg, "Summary");
    for (size_t i = 0; i < logs.size (); i++)
      logs[i]->summarize (msg);
  }
}

size_t
Output::scope_size () const
{ return scopes.size (); }

Scope&
Output::scope (size_t i) const
{ 
  daisy_assert (i < scopes.size ());
  return *scopes[i];
}

bool
Output::check (const Border& field, Treelog& msg)
{
  bool ok = true;

  for (std::vector<Log*>::const_iterator i = logs.begin ();
       i != logs.end ();
       i++)
    {
      if (*i == NULL || !(*i)-> check (field, msg))
        ok = false;
    }

  return ok;
}

void
Output::initialize (const Metalib& metalib, Treelog& msg)
{
  for (size_t i = 0; i < logs.size (); i++)
    logs[i]->initialize_common (metalib, msg);
  log_all->initialize_common (metalib, msg);
}

void 
Output::add_log (Log* log)
{ 
  if (LogSelect* sel = dynamic_cast<LogSelect*> (log))
    log_all->attach_log (sel);
  else
    active_logs.push_back (log); 
}

const std::vector<Log*> 
Output::find_active_logs (const std::vector<Log*>& logs, LogAll& log_all)
{
  std::vector<Log*> result;

  for (size_t i = 0; i < logs.size (); i++)
    if (!dynamic_cast<LogSelect*> (logs[i]))
      result.push_back (logs[i]);
  
  result.push_back (&log_all);
  
  return result;
}

const std::vector<Scope*> 
Output::find_extern_logs (const std::vector<Log*>& logs, 
                          const std::vector<Scope*>& exchanges)
{
  std::vector<Scope*> result = exchanges;

  for (size_t i = 0; i < logs.size (); i++)
    if (LogExtern* log = dynamic_cast<LogExtern*> (logs[i]))
      result.push_back (log);
  
  return result;
}

std::vector<Time::component_t>
Output::find_time_columns (const std::vector<std::string>& names)
{
  std::vector<Time::component_t> result;
  
  for (size_t n = 0; n < names.size (); n++)
    for (Time::component_t c = Time::First; c <= Time::Last; c++)
      if (names[n] == Time::component_name (c))
	result.push_back (c);

  return result;
}

Output::Output (Block& al)
  : logging (false),
    exchanges (Librarian::build_vector<Scope> (al, "exchange")),
    logs (Librarian::build_vector<Log> (al, "output")),
    log_all (new LogAll (logs)),
    active_logs (find_active_logs (logs, *log_all)),
    scopes (find_extern_logs (logs, exchanges)),
    activate_output (Librarian::build_item<Condition> (al, "activate_output")),
    time_columns (find_time_columns (al.name_sequence ("log_time_columns")))
{ }

Output::Output ()
  : logging (false),
    exchanges (std::vector<Scope*> ()),
    logs (std::vector<Log*> ()),
    active_logs (std::vector<Log*> ()),
    scopes (std::vector<Scope*> ())
{ }

Output::~Output ()
{ }

void
Output::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_object ("output", Log::component,
                     Syntax::State, Syntax::Sequence,
                     "List of logs for output during the simulation.");
  syntax.add_object ("activate_output", Condition::component,
                     "Activate output logs when this condition is true.\n\
You can use the 'after' condition to avoid logging during an initialization\n\
period.");
  AttributeList true_alist;
  true_alist.add ("type", "true");
  alist.add ("activate_output", true_alist);

  syntax.add_object ("exchange", Scope::component,
                     Syntax::Const, Syntax::Sequence, "\
List of exchange items for communicating with external models.");
  alist.add ("exchange", std::vector<const AttributeList*> ());

  // The log_time paramater.
  static VCheck::Enum valid_component;
  const bool empty_valid = valid_component.size () < 1;
  std::string log_time_doc = "\
List of default time components to include in log files. Choose between:\n";

  for (Time::component_t i = Time::First; i <= Time::Last; i++)
    {
      const std::string& name = Time::component_name (i);
      const std::string& doc = Time::component_documentation (i);
      log_time_doc += " '" + name + "': " + doc + "\n";
      if (empty_valid)
	valid_component.add (name);
    }
  syntax.add ("log_time_columns",
	      Syntax::String, Syntax::Const, Syntax::Sequence, 
	      log_time_doc);
  syntax.add_check ("log_time_columns", valid_component);
  std::vector<symbol> default_time;
  default_time.push_back (symbol ("year"));
  default_time.push_back (symbol ("month"));
  default_time.push_back (symbol ("mday"));
  default_time.push_back (symbol ("hour"));
#if 0
  default_time.push_back (symbol ("minute"));
  default_time.push_back (symbol ("second"));
#endif
  alist.add ("log_time_columns", default_time);
}

// output.C ends here.
