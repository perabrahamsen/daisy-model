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
#include "syntax.h"
#include "assertion.h"

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

const std::vector<Scope*> 
/**/ Output::find_extern_logs (const std::vector<Log*>& logs, 
                               const std::vector<Scope*>& exchanges)
{
  std::vector<Scope*> result = exchanges;

  for (size_t i = 0; i < logs.size (); i++)
    if (LogExtern* log = dynamic_cast<LogExtern*> (logs[i]))
      result.push_back (log);
  
  return result;
}

Output::Output (Block& al)
  : logging (false),
    exchanges (Librarian<Scope>::build_vector (al, "exchange")),
    logs (Librarian<Log>::build_vector (al, "output")),
    log_all (new LogAll (logs)),
    active_logs (find_active_logs (logs, *log_all)),
    scopes (find_extern_logs (logs, exchanges)),
    activate_output (Librarian<Condition>::build_item (al, "activate_output"))
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
  syntax.add_object ("output", Librarian<Log>::library (),
                     Syntax::State, Syntax::Sequence,
                     "List of logs for output during the simulation.");
  syntax.add_object ("activate_output", Librarian<Condition>::library (),
                     "Activate output logs when this condition is true.\n\
You can use the 'after' condition to avoid logging during an initialization\n\
period.");
  AttributeList true_alist;
  true_alist.add ("type", "true");
  alist.add ("activate_output", true_alist);

  syntax.add_object ("exchange", Librarian<Scope>::library (),
                     Syntax::Const, Syntax::Sequence, "\
List of exchange items for communicating with external models.");
  alist.add ("exchange", std::vector<AttributeList*> ());
}
