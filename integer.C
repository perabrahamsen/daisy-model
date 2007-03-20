// integer.C --- Integers in Daisy.
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


#include "integer.h"
#include "boolean.h"
#include "submodeler.h"
#include "block.h"
#include "alist.h"
#include "memutils.h"
#include <sstream>

template<>
Librarian<Integer>::Content* Librarian<Integer>::content = NULL;

const char *const Integer::description = "\
Generic representation of integers.";

const std::string& 
Integer::title () const
{ return name.name (); }

Integer::Integer (Block& al)
  : name (al.identifier ("type"))
{ }

Integer::~Integer ()
{ }

struct IntegerConst : public Integer
{
  // Parameters.
  const int val;

  // Simulation.
  bool missing (const Scope&) const
  { return false; }
  int value (const Scope&) const
  { return val; }

  // Create.
  bool initialize (Treelog&)
  { return true; }
  bool check (const Scope&, Treelog&) const
  { return true; }
  IntegerConst (Block& al)
    : Integer (al),
      val (al.integer ("value"))
  { }
};

static struct IntegerConstSyntax
{
  static Model& make (Block& al)
  { return *new IntegerConst (al); }
  IntegerConstSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Always give the specified value.");
    syntax.add ("value", Syntax::Integer, Syntax::Const,
		"Fixed value for this integer.");
    syntax.order ("value");
    Librarian<Integer>::add_type ("const", alist, syntax, &make);
  }
} IntegerConst_syntax;

struct IntegerCond : public Integer
{
  // Parameters.
  struct Clause
  {
    const std::auto_ptr<Boolean> condition;
    const int value;
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    {
      alist.add ("description", "\
If condition is true, return value.");
      syntax.add ("condition", Librarian<Boolean>::library (), "\
Condition to test for.");
      syntax.add ("value", Syntax::Integer, Syntax::Const, "\
Value to return.");
      syntax.order ("condition", "value");
    }
    Clause (Block& al)
      : condition (Librarian<Boolean>::build_item (al, "condition")),
        value (al.integer ("value"))
    { }
  };
  std::vector<const Clause*> clauses;

  // Simulation.
  bool missing (const Scope&) const
  { return false; }
  int value (const Scope& scope) const
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      if (clauses[i]->condition->value (scope))
        return clauses[i]->value;
    throw "No matching conditions";
  }

  // Create.
  bool initialize (Treelog& msg)
  {
    bool ok = true;
    for (size_t i = 0; i < clauses.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (msg, tmp.str ());
        if (!clauses[i]->condition->initialize (msg))
          ok = false;
      }
    return ok;
  }
  bool check (const Scope& scope, Treelog& msg) const
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      if (clauses[i]->condition->value (scope))
        return true;
    msg.error ("No clause matches");
    return false; 
  }
  IntegerCond (Block& al)
    : Integer (al),
      clauses (map_submodel_const<Clause> (al, "clauses"))
  { }
  ~IntegerCond ()
  { sequence_delete (clauses.begin (), clauses.end ()); }
};

static struct IntegerCondSyntax
{
  static Model& make (Block& al)
  { return *new IntegerCond (al); }
  IntegerCondSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "\
Return the value of the first clause whose condition is true.");
    syntax.add_submodule_sequence ("clauses", Syntax::Const, "\
List of clauses to match for.",
                                   IntegerCond::Clause::load_syntax);
    syntax.order ("clauses");
    Librarian<Integer>::add_type ("cond", alist, syntax, &make);
  }
} IntegerCond_syntax;

