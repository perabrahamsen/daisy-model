// number_extern.C -- Extract a single number from an extern log.
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


#include "number.h"
#include "block.h"
#include "alist.h"
#include "log_extern.h"
#include "scope_multi.h"
#include <memory>

struct NumberExtern : public Number
{
  const symbol extern_name;
  const Scope* extern_scope;
  std::auto_ptr<Number> expr;

  const std::string& title () const
  { return extern_name.name (); }
  void tick (const Scope& scope, Treelog& msg)
  { expr->tick (scope, msg); }
  bool missing (const Scope& inherit_scope) const 
  { 
    daisy_assert (extern_scope);
    ScopeMulti scope (*extern_scope, inherit_scope);
    return expr->missing (scope);
  }
  double value (const Scope& inherit_scope) const
  { 
    daisy_assert (extern_scope);
    ScopeMulti scope (*extern_scope, inherit_scope);
    return expr->value (scope);
  }
  symbol dimension (const Scope& inherit_scope) const 
  {     
    daisy_assert (extern_scope);
    ScopeMulti scope (*extern_scope, inherit_scope);
    return expr->dimension (scope);
  }

  // Create.
  bool initialize (Treelog& msg)
  {
    Treelog::Open nest (msg, name + ": " + extern_name);
    extern_scope = find_extern_scope (extern_name);
    return expr->initialize (msg);
  }
  bool check (const Scope& inherit_scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name + ": " + extern_name);
    if (!extern_scope)
      {
        msg.error ("No extern log named '" + extern_name + "' found");
        return false;
      }
    ScopeMulti scope (*extern_scope, inherit_scope);
    return expr->check (scope, msg);
  }

  NumberExtern (Block& al)
    : Number (al),
      extern_name (al.identifier ("name")),
      extern_scope (NULL),
      expr (Librarian<Number>::build_item (al, "expr"))
  { }
};

static struct NumberExternSyntax
{
  static Number& make (Block& al)
  { return *new NumberExtern (al); }
  NumberExternSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Evaluate an expression with access to recent values from an 'extern' log.");
    syntax.add ("expr", Librarian<Number>::library (), "\
Expression to evaluate.");
    syntax.add ("name", Syntax::String, Syntax::Const, "\
Name of extern log to fecth values from.");
    syntax.order ("name", "expr");
    Librarian<Number>::add_type ("extern", alist, syntax, &make);
  }
} NumberExtern_syntax;

// number_extern.C ends here
