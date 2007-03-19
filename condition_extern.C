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
#include "daisy.h"
#include "block.h"
#include "alist.h"
#include "boolean.h"
#include "output.h"
#include "scope.h"
#include "scopesel.h"
#include <memory>

struct ConditionExtern : public Condition
{
  const std::auto_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;
  std::auto_ptr<Boolean> expr;
  
  // State.
  mutable enum { isfalse, istrue, missing, uninitialized, error } state;

  void tick (const Daisy&, Treelog&)
  { }

  bool match (const Daisy& daisy, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);

    if (state == uninitialized)
      {
        extern_scope = scopesel->lookup (*daisy.output_log, msg);
        if (!expr->initialize (msg)
            || !extern_scope
            || !expr->check (*extern_scope, msg))
          {
            state = error;
            msg.error ("Initialize failed, condition will always be false");
          }
      }
    if (state != error)
      {
        expr->tick (*extern_scope, msg);
        if (expr->missing (*extern_scope))
          state = missing;
        else
          state = expr->value (*extern_scope) ? istrue : isfalse;
      }
    return state == istrue ? true : false; 
  }

  void output (Log&) const
  { }

  ConditionExtern (Block& al)
    : Condition (al),
      scopesel (Librarian<Scopesel>::build_item (al, "scope")),
      extern_scope (NULL),
      expr (Librarian<Boolean>::build_item (al, "expr")),
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
      syntax.add ("scope", Librarian<Scopesel>::library (), 
                  Syntax::Const, Syntax::Singleton, "\
Scope to evaluate expession in.");
      syntax.add ("expr", Librarian<Boolean>::library (), "\
Expression to evaluate.");
      syntax.order ("scope", "expr");
      Librarian<Condition>::add_type ("extern",
				      alist, syntax, &make);
    }
  }
} ConditionExtern_syntax;

// condition_extern.C ends here.
