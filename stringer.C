// stringer.C --- Strings in Daisy.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#include "stringer.h"

template<>
Librarian<Stringer>::Content* Librarian<Stringer>::content = NULL;

const char *const Stringer::description = "\
Generic representation of strings.";

const std::string& 
Stringer::title () const
{ return name.name (); }

Stringer::Stringer (const Block& al)
  : name (al.identifier ("type"))
{ }

Stringer::~Stringer ()
{ }

struct StringerCond : public Stringer
{
  // Parameters.
  struct Clause
  {
    const auto_ptr<const Boolean> condition;
    const std::string value;
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    {
      alist.add ("description", "\
If condition is true, return value.");
      syntax.add ("condition", Librarian<Boolean>::library (), "\
Condition to test for.");
      syntax.add ("value", Syntax::String, Syntax::Const, "\
Value to return.");
      syntax.order ("condition", "value");
    }
    Clause (const Block& al)
      : condition (Librarian<Boolean>::build_item (al, "condition")),
        value (al.name ("value"))
    { }
  }
  std::vector<Clause*> clauses;

  // Simulation.
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope& scope) const
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      if (clauses[i]->condition (scope))
        return clauses[i]->value;
    throw "No matching conditions";
  }

  // Create.
  bool check (const Scope& scope, Treelog& msg) const
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      if (clauses[i]->condition (scope))
        return true;
    msg.error ("No clause matches");
    return false; 
  }
  StringerCond (const Block& al)
    : Stringer (al),
      clauses (map_submodel<Clause> (al, "clauses"))
  { }
  ~StringerCond ()
  { sequence_delete (clauses.begin (), clauses.end ()); }
};

static struct StringerCondSyntax
{
  static Stringer& make (const Block& al)
  { return *new StringerCond (al); }
  StringerCondSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "\
Return the value of the first clause whose condition is true.");
    syntax.add_submodule_sequence ("clauses", Syntax::Const, "\
List of clauses to match for.",
                                   StringerCond::Clause::load_syntax);
    syntax.order ("clauses");
    Librarian<Stringer>::add_type ("cond", alist, syntax, &make);
  }
} StringerStringEqual_syntax;
