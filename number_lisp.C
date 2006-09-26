// number_lisp.C -- Lisp like constructs with numbers.
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
#include "scope_multi.h"
#include "submodeler.h"
#include "memutils.h"
#include <sstream>
#include <memory>

struct NumberLet : public Number
{
  mutable struct ScopeClause : public Scope
  {
    // Content.
    struct Clause
    {
      symbol id;
      std::auto_ptr<Number> expr;

      static void load_syntax (Syntax& syntax, AttributeList& alist)
      {
        alist.add ("description", "Bind an identifier to an expression.");
        syntax.add ("identifier", Syntax::String, Syntax::Const, 
                    "Identifier to bind.");
        syntax.add ("expr", Librarian<Number>::library (), 
                    " Value to give it.");
        syntax.order ("identifier", "expr");
      }
      Clause (Block& al)
        : id (al.identifier ("identifier")),
          expr (Librarian<Number>::build_item (al, "expr"))
      { }
    }; 
    std::vector<Clause*> clause;

    typedef std::map<symbol, double> number_map;
    number_map numbers;
    typedef std::map<symbol, symbol> symbol_map;
    symbol_map dimensions;

    // Scope.
    void tick (const Scope& scope, Treelog& msg)
    {
      numbers.clear ();
      dimensions.clear ();
      for (size_t i = 0; i < clause.size (); i++)
        {
          const symbol id = clause[i]->id;
          std::ostringstream tmp;
          tmp << "clause[" << i << "]: " << id;
          // Treelog::Open nest (msg, tmp.str ());
          Number& expr = *(clause[i]->expr);
          expr.tick (scope, msg);
          if (!expr.missing (scope))
            {
              numbers[id] = expr.value (scope);
              dimensions[id] = expr.dimension (scope);
            }
        }
    }
    bool has_number (symbol id) const
    {
      const number_map::const_iterator i = numbers.find (id);
      return i != numbers.end ();
    }
    double number (symbol id) const
    { 
      const number_map::const_iterator i = numbers.find (id);
      daisy_assert (i != numbers.end ());
      return (*i).second;
    }
    symbol dimension (symbol id) const
    {
      const symbol_map::const_iterator i = dimensions.find (id);
      daisy_assert (i != dimensions.end ());
      return (*i).second;
    }

    // Create and Destroy.
    bool initialize (Treelog& msg)
    {
      bool ok = true;
      for (size_t i = 0; i < clause.size (); i++)
        {
          std::ostringstream tmp;
          tmp << "clauses[" << i << "]";
          Treelog::Open nest (msg, tmp.str ());
          if (!clause[i]->expr->initialize (msg))
            ok = false;
        }
      return ok;
    }
    bool check (const Scope& scope, Treelog& msg) const
    {
      bool ok = true;
      for (size_t i = 0; i < clause.size (); i++)
        {
          std::ostringstream tmp;
          tmp << "clauses[" << i << "]";
          Treelog::Open nest (msg, tmp.str ());
          if (!clause[i]->expr->check (scope, msg))
            ok = false;
        }
      return ok;
    }
    static void load_syntax (Syntax& syntax, AttributeList&)
    {
      syntax.add_submodule_sequence ("clauses", Syntax::Const, "\
List of identifiers and values to bind in this scope.", Clause::load_syntax);
    }
    ScopeClause (Block& al)
      : clause (map_submodel<Clause> (al, "clauses"))
    { }
    ~ScopeClause ()
    { sequence_delete (clause.begin (), clause.end ()); }
  } scope_clause;
  std::auto_ptr<Number> expr;

  bool missing (const Scope& inherit_scope) const 
  { 
    ScopeMulti scope (scope_clause, inherit_scope);
    return expr->missing (scope);
  }
  double value (const Scope& inherit_scope) const
  { 
    ScopeMulti scope (scope_clause, inherit_scope);
    return expr->value (scope);
  }
  symbol dimension (const Scope& inherit_scope) const 
  {     
    ScopeMulti scope (scope_clause, inherit_scope);
    return expr->dimension (scope);
  }

  // Create.
  void tick (const Scope& inherit_scope, Treelog& msg)
  { 
    scope_clause.tick (inherit_scope, msg);
    expr->tick (inherit_scope, msg);
  }
  bool initialize (Treelog& msg)
  {
    Treelog::Open nest (msg, name);
    scope_clause.initialize (msg);
    return expr->initialize (msg);
  }
  bool check (const Scope& inherit_scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    if (!scope_clause.check (inherit_scope, msg))
      return false;
    scope_clause.tick (inherit_scope, msg);
    ScopeMulti scope (scope_clause, inherit_scope);
    return expr->check (scope, msg);
  }
  NumberLet (Block& al)
    : Number (al),
      scope_clause (al),
      expr (Librarian<Number>::build_item (al, "expr"))
  { }
};

static struct NumberLetSyntax
{
  static Number& make (Block& al)
  { return *new NumberLet (al); }
  NumberLetSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Bind symbols in 'clauses' in a new scope, and evaluate 'expr' in that scope.");
    NumberLet::ScopeClause::load_syntax (syntax, alist);
    syntax.add ("expr", Librarian<Number>::library (), "\
Expression to evaluate.");
    syntax.order ("clauses", "expr");
    Librarian<Number>::add_type ("let", alist, syntax, &make);
  }
} NumberLet_syntax;

// number_extern.C ends here
