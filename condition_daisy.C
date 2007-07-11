// condition_daisy.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
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
//
// Checking daisy state.

#define BUILD_DLL

#include "condition.h"
#include "syntax.h"
#include "alist.h"
#include "daisy.h"
#include "librarian.h"

struct ConditionRunning : public Condition
{
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return daisy.running; }
  void output (Log&) const
  { }
  ConditionRunning (Block& al)
    : Condition (al)
  { }
};

struct ConditionFinished : public Condition
{
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return !daisy.running; }
  void output (Log&) const
  { }
  ConditionFinished (Block& al)
    : Condition (al)
  { }
};

static struct ConditionDaisySyntax
{
  static Model& make_running (Block& al)
  { return *new ConditionRunning (al); }
  
  static Model& make_finished (Block& al)
  { return *new ConditionFinished (al); }

  ConditionDaisySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist_running = *new AttributeList ();
    alist_running.add ("description", 
                       "True iff the simulation is still running.");
    AttributeList& alist_finished = *new AttributeList ();
    alist_finished.add ("description", 
                        "True iff the simulation has finished.");
    Librarian::add_type (Condition::component, "running",
                                    alist_running, syntax, &make_running);
    Librarian::add_type (Condition::component, "finished",
                                    alist_finished, syntax, &make_finished);
  }
} ConditionDaisy_syntax;
