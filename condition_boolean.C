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

#include "condition.h"
#include "boolean.h"
#include "scope.h"
#include <memory>

struct ConditionBoolean : public Condition
{
  std::auto_ptr<Boolean> expr;
  
  // State.
  mutable enum { isfalse, istrue, missing, uninitialized, error } state;

  void tick (const Daisy&, Treelog&)
  { }

  bool match (const Daisy&, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);

    if (state == uninitialized
        && (!expr->initialize (msg)
            || !expr->check (Scope::null (), msg)))
      state = error;

    if (state != error)
      {
        expr->tick (Scope::null (), msg);
        if (expr->missing (Scope::null ()))
          state = missing;
        else
          state = expr->value (Scope::null ()) ? istrue : isfalse;
      }
    
    return state == istrue ? true : false; 
  }

  void output (Log&) const
  { }

  ConditionBoolean (Block& al)
    : Condition (al),
      expr (Librarian<Boolean>::build_item (al, "extern")),
      state (uninitialized)
  { }
};

static struct ConditionBooleanSyntax
{
  static Condition& make (Block& al)
  { return *new ConditionBoolean (al); }

  ConditionBooleanSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Test if a boolean expression is true.");
    syntax.add ("expr", Librarian<Boolean>::library (), "\
Expression to evaluate.");
      syntax.order ("expr");
      Librarian<Condition>::add_type ("check",
				      alist, syntax, &make);
    }
  }
} ConditionBoolean_syntax;
