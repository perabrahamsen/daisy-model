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

#include "output.h"
#include "daisy.h"
#include "log_all.h"
#include "log_extern.h"
#include "treelog.h"
#include "time.h"
#include "timestep.h"

void
Output::initial_logs (const Daisy& daisy, Treelog& msg)
{
  activate_output->tick (daisy, msg);

  if (activate_output->match (daisy, msg))
    {
      if (!logging)
	{
	  msg.message ("Start logging");
	  // get initial values for previous day.
	  const Time previous = daisy.time - *(daisy.timestep);
	  for (size_t i = 0; i < active_logs.size (); i++)
	    {
	      Log& log = *active_logs[i];
	      if (log.initial_match (daisy, msg))
		{
		  output_submodule (previous, "time", log);
                  daisy.output (log);
                  output_list (logs, "output", log, 
                               Librarian<Log>::library ());
		  log.initial_done (previous, daisy.dt);
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
              output_list (logs, "output", log, Librarian<Log>::library ());
	      log.done (daisy.time, daisy.dt);
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

const Scope* 
Output::scope (size_t i) const
{ 
  daisy_assert (i < scopes.size ());
  return scopes[i];
}

const Scope*
Output::scope (const symbol target) const
{
  for (size_t i = 0; i < scopes.size (); i++)
    {
      const LogExtern* scope = scopes[i];
      const symbol name = scope->alist.check ("where") 
        ? scope->alist.identifier ("where") 
        : scope->alist.identifier ("type");

      if (name == target)
        return scope;
    }
  return NULL;
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
Output::initialize (Treelog& msg)
{
  for (size_t i = 0; i < logs.size (); i++)
    logs[i]->initialize (msg);
}

const std::vector<Log*> 
/**/ Output::find_active_logs (const std::vector<Log*>& logs, LogAll& log_all)
{
  std::vector<Log*> result;

  for (size_t i = 0; i < logs.size (); i++)
    if (!dynamic_cast<LogSelect*> (logs[i]))
      result.push_back (logs[i]);
  
  result.push_back (&log_all);
  
  return result;
}

const std::vector<const LogExtern*> 
/**/ Output::find_extern_logs (const std::vector<Log*>& logs)
{
  std::vector<const LogExtern*> result;

  for (size_t i = 0; i < logs.size (); i++)
    if (const LogExtern* log = dynamic_cast<const LogExtern*> (logs[i]))
      result.push_back (log);
  
  return result;
}

Output::Output (Block& al)
  : logging (false),
    logs (Librarian<Log>::build_vector (al, "output")),
    log_all (new LogAll (logs)),
    active_logs (find_active_logs (logs, *log_all)),
    scopes (find_extern_logs (logs)),
    activate_output (Librarian<Condition>::build_item (al, "activate_output"))
{ }

Output::~Output ()
{ }
