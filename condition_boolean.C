// condition_boolean.C
// 
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
//
// Interface to generic boolean library.

#define BUILD_DLL

#include "condition.h"
#include "boolean.h"
#include "scope.h"
#include "librarian.h"
#include "assertion.h"
#include "daisy.h"
#include "treelog.h"
#include "frame.h"
#include "block_model.h"
#include <memory>

struct ConditionBoolean : public Condition
{
  std::unique_ptr<Boolean> expr;
  
  // State.
  mutable enum { isfalse, istrue, missing, uninitialized, error } state;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { expr->tick (daisy.units (), scope, msg); }

  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);

    if (state != error)
      {
        expr->tick (daisy.units (), scope, msg);
        if (expr->missing (scope))
          state = missing;
        else
          state = expr->value (scope) ? istrue : isfalse;
      }
    
    return state == istrue ? true : false; 
  }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    daisy_assert (state == uninitialized);
    
    if (!expr->initialize (daisy.units (), scope, msg))
      state = error;
    else
      state = missing;
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { 
    if (state == error)
      return false;
  
    daisy_assert (state == missing);

    if (expr->check (daisy.units (), scope, msg))
      return true;

    state = error;
    return false;
  }

  ConditionBoolean (const BlockModel& al)
    : Condition (al),
      expr (Librarian::build_item<Boolean> (al, "expr")),
      state (uninitialized)
  { }
};

static struct ConditionBooleanSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionBoolean (al); }

  ConditionBooleanSyntax ()
    : DeclareModel (Condition::component, "check", "\
Test if a boolean expression is true.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("expr", Boolean::component, "\
Expression to evaluate.");
    frame.order ("expr");
  }
} ConditionBoolean_syntax;
