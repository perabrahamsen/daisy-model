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
#include "syntax.h"
#include "alist.h"
#include "boolean.h"
#include "scope.h"
#include "librarian.h"
#include "assertion.h"
#include <memory>

struct ConditionBoolean : public Condition
{
  std::auto_ptr<Boolean> expr;
  
  // State.
  mutable enum { isfalse, istrue, missing, uninitialized, error } state;

  void tick (const Daisy&, const Scope& scope, Treelog& msg)
  { expr->tick (scope, msg); }

  bool match (const Daisy&, const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);

    if (state != error)
      {
        expr->tick (scope, msg);
        if (expr->missing (scope))
          state = missing;
        else
          state = expr->value (scope) ? istrue : isfalse;
      }
    
    return state == istrue ? true : false; 
  }

  void output (Log&) const
  { }

  void initialize (const Daisy&, const Scope&, Treelog& msg)
  {
    daisy_assert (state == uninitialized);
    
    if (!expr->initialize (msg))
      state = error;
    else
      state = missing;
  }

  bool check (const Daisy&, const Scope& scope, Treelog& msg) const
  { 
    if (state == error)
      return false;
  
    daisy_assert (state == missing);

    if (expr->check (scope, msg))
      return true;

    state = error;
    return false;
  }

  ConditionBoolean (Block& al)
    : Condition (al),
      expr (Librarian::build_item<Boolean> (al, "extern")),
      state (uninitialized)
  { }
};

static struct ConditionBooleanSyntax
{
  static Model& make (Block& al)
  { return *new ConditionBoolean (al); }

  ConditionBooleanSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Test if a boolean expression is true.");
      syntax.add_object ("expr", Boolean::component, "\
Expression to evaluate.");
      syntax.order ("expr");
      Librarian::add_type (Condition::component, "check",
				      alist, syntax, &make);
    }
  }
} ConditionBoolean_syntax;
