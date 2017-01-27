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

#define BUILD_DLL

#include "condition.h"
#include "daisy.h"
#include "block_model.h"
#include "boolean.h"
#include "output.h"
#include "scope_multi.h"
#include "scopesel.h"
#include "librarian.h"
#include "assertion.h"
#include "treelog.h"
#include "frame.h"
#include <memory>

struct ConditionExtern : public Condition
{
  bool initialized_ok;
  const std::unique_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;
  std::unique_ptr<Boolean> expr;
  
  void tick (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  {
    daisy_assert (initialized_ok);
    daisy_assert (extern_scope);
    ScopeMulti multi (*extern_scope, parent_scope);
    expr->tick (daisy.units (), multi, msg);  
  }

  bool match (const Daisy&, const Scope& parent_scope, Treelog& msg) const
  { 
    daisy_assert (initialized_ok);
    daisy_assert (extern_scope);

    TREELOG_MODEL (msg);
    ScopeMulti multi (*extern_scope, parent_scope);

    if (expr->missing (multi))
      return false;

    return expr->value (multi);
  }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  {
    initialized_ok = true;
    daisy_assert (!extern_scope);
    extern_scope = scopesel->lookup (daisy.scopes (), msg); 
    if (!extern_scope)
      return;
    ScopeMulti multi (*extern_scope, parent_scope);
    if (!expr->initialize (daisy.units (), multi, msg))
      initialized_ok = false;
  }

  bool check (const Daisy& daisy, const Scope& parent_scope, 
              Treelog& msg) const
  {
    if (!initialized_ok)
      {
        msg.error ("Initialized failed");
        return false;
      }
    if (!extern_scope)
      {
        msg.error ("Extern scope not found");
        return false;
      }

    ScopeMulti multi (*extern_scope, parent_scope);
    return expr->check (daisy.units (), multi, msg);
  }

  ConditionExtern (const BlockModel& al)
    : Condition (al),
      initialized_ok (false),
      scopesel (Librarian::build_item<Scopesel> (al, "scope")),
      extern_scope (NULL),
      expr (Librarian::build_item<Boolean> (al, "expr"))
  { }
};

static struct ConditionExternSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionExtern (al); }

  ConditionExternSyntax ()
    : DeclareModel (Condition::component, "extern", "\
Test if a boolean expression is true,using extern log.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.declare_object ("scope", Scopesel::component, 
                         Attribute::Const, Attribute::Singleton, "\
Scope to evaluate expession in.");
      frame.declare_object ("expr", Boolean::component, "\
Expression to evaluate.");
      frame.order ("scope", "expr");
  }
} ConditionExtern_syntax;

// condition_extern.C ends here.
