// action_extern.C -- Make external information available.
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

#define BUILD_DLL

#include "action.h"
#include "scope_multi.h"
#include "scopesel.h"
#include "daisy.h"
#include "log.h"
#include "treelog.h"
#include "librarian.h"
#include "syntax.h"
#include <memory>

struct ActionExtern : public Action
{
  const std::auto_ptr<Scopesel> scopesel;
  mutable const Scope* extern_scope;
  const std::auto_ptr<Action> child;

  void tick (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  {
    ScopeMulti multi (*extern_scope, parent_scope);
    child->tick (daisy, multi, msg);  
  }

  void doIt (Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    ScopeMulti multi (*extern_scope, parent_scope);
    child->doIt (daisy, multi, msg);
  }

  bool done (const Daisy& daisy, const Scope& parent_scope, Treelog& msg) const
  { 
    ScopeMulti multi (*extern_scope, parent_scope);
    return child->done (daisy, multi, msg); 
  }

  void output (Log& log) const
  { output_derived (child, "action", log); }

  void initialize (const Daisy& daisy, const Scope& parent_scope, Treelog& msg)
  { 
    extern_scope = scopesel->lookup (*daisy.output_log, msg); 
    if (extern_scope)
      {
        ScopeMulti multi (*extern_scope, parent_scope);
        child->initialize (daisy, multi, msg);
      }
  }

  bool check (const Daisy& daisy, const Scope& parent_scope, 
              Treelog& msg) const
  { 
    bool ok = true; 

    if (!extern_scope)
      {
        msg.error ("Extern scope not found");
        ok = false;
      }
    else
      {
        ScopeMulti multi (*extern_scope, parent_scope);
        if (!child->check (daisy, multi, msg))
          ok = false;
      }
    return ok;
  }

  ActionExtern (Block& al)
    : Action (al),
      scopesel (Librarian::build_item<Scopesel> (al, "scope")),
      extern_scope (NULL),
      child (Librarian::build_item<Action> (al, "action"))
  { }
  ~ActionExtern ()
  { }
};

static struct ActionExternSyntax
{
  static Model& make (Block& al)
  { return *new ActionExtern (al); }
  ActionExternSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Select an external scope, and perform action.");

    syntax.add_object ("scope", Scopesel::component, 
                       Syntax::Const, Syntax::Singleton, "\
Scope to evaluate expession in.");
    syntax.add_object ("action", Action::component, 
                       "Action to perform if the condition is false.");
    syntax.order ("scope", "action");

    Librarian::add_type (Action::component, "extern", alist, syntax, &make);
  }
} ActionExtern_syntax;

// action_extern.C ends here.
