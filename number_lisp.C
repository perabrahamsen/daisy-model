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

#define BUILD_DLL

#include "number.h"
#include "boolean.h"
#include "scope_multi.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include "assertion.h"
#include "frame.h"
#include "block_model.h"
#include <sstream>
#include <memory>
#include <map>

// The 'let' model.

struct NumberLet : public Number
{
  mutable struct ScopeClause : public Scope
  {
    // Content.
    struct Clause
    {
      symbol id;
      std::unique_ptr<Number> expr;

      static void load_syntax (Frame& frame)
      {
        // Bind an identifier to an expression.
        frame.declare_string ("identifier", Attribute::Const, 
                    "Identifier to bind.");
        frame.declare_object ("expr", Number::component, 
                           " Value to give it.");
        frame.order ("identifier", "expr");
      }
      Clause (const Block& al)
        : id (al.name ("identifier")),
          expr (Librarian::build_item<Number> (al, "expr"))
      { }
    }; 
    std::vector<Clause*> clause;

    typedef std::map<symbol, double> number_map;
    number_map numbers;
    typedef std::map<symbol, symbol> symbol_map;
    symbol_map dimensions;

    // Scope.
    void tick (const Units& units, const Scope& scope, Treelog& msg)
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
          expr.tick (units, scope, msg);
          if (!expr.missing (scope))
            {
              numbers[id] = expr.value (scope);
              dimensions[id] = expr.dimension (scope);
            }
        }
    }
    void entries (std::set<symbol>& all) const
    {
      for (number_map::const_iterator i = numbers.begin ();
           i != numbers.end ();
           i++)
        all.insert ((*i).first);
    }

    Attribute::type lookup (const symbol id) const
    { return check (id) ? Attribute::Number : Attribute::Error; }

    bool check (symbol id) const
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
    symbol description (symbol) const
    { return symbol ("Descriptions not implemented yet"); }

    // Create and Destroy.
    bool initialize (const Units& units, const Scope& scope, Treelog& msg)
    {
      bool ok = true;
      for (size_t i = 0; i < clause.size (); i++)
        {
          std::ostringstream tmp;
          tmp << "clauses[" << i << "]";
          Treelog::Open nest (msg, tmp.str ());
          if (!clause[i]->expr->initialize (units, scope, msg))
            ok = false;
        }
      if (ok)
	tick (units, scope, msg);
      return ok;
    }
    using Scope::check;
    bool check (const Units& units, const Scope& scope, Treelog& msg) const
    {
      bool ok = true;
      for (size_t i = 0; i < clause.size (); i++)
        {
          std::ostringstream tmp;
          tmp << "clauses[" << i << "]";
          Treelog::Open nest (msg, tmp.str ());
          if (!clause[i]->expr->check (units, scope, msg))
            ok = false;
        }
      return ok;
    }
    static void load_syntax (Frame& frame)
    {
      frame.declare_submodule_sequence ("clauses", Attribute::Const, "\
List of identifiers and values to bind in this scope.", Clause::load_syntax);
    }
    ScopeClause (const BlockModel& al)
      : clause (map_submodel<Clause> (al, "clauses"))
    { }
    ~ScopeClause ()
    { sequence_delete (clause.begin (), clause.end ()); }
  } scope_clause;
  std::unique_ptr<Number> expr;

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
  void tick (const Units& units, const Scope& inherit_scope, Treelog& msg)
  { 
    scope_clause.tick (units, inherit_scope, msg);
    expr->tick (units, inherit_scope, msg);
  }
  bool initialize (const Units& units,
                   const Scope& inherit_scope, Treelog& msg)
  {
    TREELOG_MODEL (msg);
    scope_clause.initialize (units, inherit_scope, msg);
    ScopeMulti scope (scope_clause, inherit_scope);
    return expr->initialize (units, scope, msg);
  }
  bool check (const Units& units,
              const Scope& inherit_scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    if (!scope_clause.check (units, inherit_scope, msg))
      return false;
    scope_clause.tick (units, inherit_scope, msg);
    ScopeMulti scope (scope_clause, inherit_scope);
    return expr->check (units, scope, msg);
  }
  NumberLet (const BlockModel& al)
    : Number (al),
      scope_clause (al),
      expr (Librarian::build_item<Number> (al, "expr"))
  { }
};

static struct NumberLetSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberLet (al); }
  NumberLetSyntax ()
    : DeclareModel (Number::component, "let", "\
Bind symbols in 'clauses' in a new scope, and evaluate 'expr' in that scope.")
  { }
  void load_frame (Frame& frame) const
  {
    NumberLet::ScopeClause::load_syntax (frame);
    frame.declare_object ("expr", Number::component, "\
Expression to evaluate.");
    frame.order ("clauses", "expr");
  }
} NumberLet_syntax;

// The 'if' model.

struct NumberIf : public Number
{
  std::unique_ptr<Boolean> if_b;
  std::unique_ptr<Number> then_n;
  std::unique_ptr<Number> else_n;

  bool missing (const Scope& scope) const 
  { 
    return if_b->missing (scope)
      || then_n->missing (scope)
      || else_n->missing (scope);
  }
  double value (const Scope& scope) const
  { 
    return if_b->value (scope)
      ? then_n->value (scope)
      : else_n->value (scope);
  }
  symbol dimension (const Scope& scope) const 
  {     
    const symbol then_dim = then_n->dimension (scope);
    const symbol else_dim = else_n->dimension (scope);
    if (then_dim == else_dim)
      return then_dim;
    
    return Attribute::Unknown ();
  }

  // Create.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    if_b->tick (units, scope, msg);
    then_n->tick (units, scope, msg);
    else_n->tick (units, scope, msg);
  }
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  {
    TREELOG_MODEL (msg);
    bool ok = true;
    if (!if_b->initialize (units, scope, msg))
      ok = false;
    if (!then_n->initialize (units, scope, msg))
      ok = false;
    if (!else_n->initialize (units, scope, msg))
      ok = false;
    return ok;
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    bool ok = true;
    if (!if_b->check (units, scope, msg))
      ok = false;
    if (!then_n->check (units, scope, msg))
      ok = false;
    if (!else_n->check (units, scope, msg))
      ok = false;
    return ok;
  }
  NumberIf (const BlockModel& al)
    : Number (al),
      if_b (Librarian::build_item<Boolean> (al, "if")),
      then_n (Librarian::build_item<Number> (al, "then")),
      else_n (Librarian::build_item<Number> (al, "else"))
  { }
};

static struct NumberIfSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberIf (al); }
  NumberIfSyntax ()
    : DeclareModel (Number::component, "if", "\
Select between two numbers depending on a boolean expression.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("if", Boolean::component, 
                          "Select which number to use.");
    frame.declare_object ("then", Number::component, 
                          "Use this if true.");
    frame.declare_object ("else", Number::component, 
                          "Use this if false.");
    frame.order ("if", "then", "else");
  }
} NumberIf_syntax;

// number_lisp.C ends here
