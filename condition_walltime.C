// condition_walltime.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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
#include "condition.h"
#include "block.h"
#include "alist.h"
#include "librarian.h"
#include "frame.h"

struct ConditionPeriodic : public Condition
{
  const int period;
  mutable time_t last;
public:
  bool match (const Daisy&, const Scope&, Treelog&) const
  {
    const time_t next = time (NULL);
    if (next - last >= period)
      {
        last = next;
        return true;
      }
    return false;
  }
  void output (Log&) const
  { }
  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionPeriodic (Block& al)
    : Condition (al),
      period (al.integer ("period")),
      last (0)
  { }
};

static struct ConditionPeriodicSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionPeriodic (al); }
  ConditionPeriodicSyntax ()
    : DeclareModel (Condition::component, "periodic", "\
True if move than a specified walltime has passed since last time\n\
it was true.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add ("period", Value::Integer, Value::Const, 
		"Number of walltime seconds between success.");
    frame.add ("period", 1);
    frame.order ("period");
  }
} ConditionPeriodic_syntax;

// condition_walltime.C ends here.
