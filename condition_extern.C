// condition_extern.C
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
#include "log_extern.h"
#include <memory>

struct ConditionExtern : public Condition
{
  const symbol extern_name;
  const Scope* extern_scope;
  std::auto_ptr<Boolean> expr;
  
  // State.
  enum { isfalse, istrue, missing, uninitialized, error } state;

  void tick (const Daisy&, Treelog& msg)
  {
    Treelog::Open nest (msg, name);

    if (state == uninitialized)
      {
        extern_scope = find_extern_scope (extern_name);
        if (!expr->initialize (msg)
            || !extern_scope
            || !expr->check (*extern_scope, msg))
          {
            state = error;
            msg.error ("Initialize failed, condition will always be false");
          }
      }
    if (state != error)
      if (expr->missing (*extern_scope))
        state = missing;
      else
        state = expr->value (*extern_scope) ? istrue : isfalse;
  }

  bool match (const Daisy&) const
  { return state == istrue ? true : false; }

  void output (Log&) const
  { }

  ConditionExtern (Block& al)
    : Condition (al),
      extern_name (al.identifier ("name")),
      extern_scope (NULL),
      expr (Librarian<Boolean>::build_item (al, "extern")),
      state (uninitialized)
  { }
};

static struct ConditionExternSyntax
{
  static Condition& make (Block& al)
  { return *new ConditionExtern (al); }

  ConditionExternSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Test if a boolean expression is true,using extern log.");
      syntax.add ("name", Syntax::String, Syntax::Const, "\
Name of extern log to fecth values from.");
      syntax.add ("expr", Librarian<Boolean>::library (), "\
Expression to evaluate.");
      syntax.order ("name", "expr");
      Librarian<Condition>::add_type ("extern",
				      alist, syntax, &make);
    }
  }
} ConditionExtern_syntax;
