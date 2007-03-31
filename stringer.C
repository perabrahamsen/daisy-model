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
#include "boolean.h"
#include "number.h"
#include "submodeler.h"
#include "alist.h"
#include "memutils.h"
#include <sstream>
#include <vector>
#include <memory>

const char *const Stringer::description = "\
Generic representation of strings.";

const char *const Stringer::component = "string";

const std::string& 
Stringer::title () const
{ return name.name (); }

Stringer::Stringer (Block& al)
  : name (al.identifier ("type"))
{ }

Stringer::~Stringer ()
{ }

struct StringerCond : public Stringer
{
  // Parameters.
  struct Clause
  {
    const std::auto_ptr<Boolean> condition;
    const std::string value;
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    {
      alist.add ("description", "\
If condition is true, return value.");
      syntax.add_object ("condition", Boolean::component, "\
Condition to test for.");
      syntax.add ("value", Syntax::String, Syntax::Const, "\
Value to return.");
      syntax.order ("condition", "value");
    }
    Clause (Block& al)
      : condition (Librarian<Boolean>::build_item (al, "condition")),
        value (al.name ("value"))
    { }
  };
  std::vector<const Clause*> clauses;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      clauses[i]->condition->tick (scope, msg);
  }
  bool missing (const Scope&) const
  { return false; }
  std::string value (const Scope& scope) const
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
  StringerCond (Block& al)
    : Stringer (al),
      clauses (map_submodel_const<Clause> (al, "clauses"))
  { }
  ~StringerCond ()
  { sequence_delete (clauses.begin (), clauses.end ()); }
};

static struct StringerCondSyntax
{
  static Model& make (Block& al)
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
    BuildBase::add_type (Stringer::component, "cond", alist, syntax, &make);
  }
} StringerCond_syntax;

struct StringerNumber : public Stringer
{
  // Parameters.
  std::auto_ptr<Number> number;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { number->tick (scope, msg); }
  bool missing (const Scope& scope) const
  { return number->missing (scope); }

  // Create.
  static void load_syntax (Syntax& syntax, AttributeList&)
  { 
    syntax.add_object ("number", Number::component, "\
Number to manipulate."); 
  }
  bool initialize (Treelog& msg)
  { 
    Treelog::Open nest (msg, name);
    return number->initialize (msg); 
  }
  bool check (const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    return number->check (scope, msg); 
  }
  StringerNumber (Block& al)
    : Stringer (al),
      number (Librarian<Number>::build_item (al, "number"))
  { }
  ~StringerNumber ()
  { }
};

struct StringerValue : public StringerNumber
{
  const int precision;

  std::string value (const Scope& scope) const
  { 
    std::ostringstream tmp;
    if (precision >= 0)
      {
        tmp.precision (precision);
        tmp.flags (std::ios::right | std::ios::fixed);
      }
    tmp << number->value (scope);
    return tmp.str ();
  }

  StringerValue (Block& al)
    : StringerNumber (al),
      precision (al.integer ("precision", -1))
  { }
};

static struct StringerValueSyntax
{
  static Model& make (Block& al)
  { return *new StringerValue (al); }
  StringerValueSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    StringerNumber::load_syntax (syntax, alist);
    alist.add ("description", "\
Extract the value of a number as a string.");
    syntax.add ("precision", Syntax::Integer, Syntax::OptionalConst, "\
Number of decimals after point.  By default, use a floating format.");
    BuildBase::add_type (Stringer::component, "value", alist, syntax, &make);
  }
} StringerValue_syntax;

struct StringerDimension : public StringerNumber
{
  std::string value (const Scope& scope) const
  { return number->dimension (scope).name (); }

  StringerDimension (Block& al)
    : StringerNumber (al)
  { }
};

static struct StringerDimensionSyntax
{
  static Model& make (Block& al)
  { return *new StringerDimension (al); }
  StringerDimensionSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    StringerNumber::load_syntax (syntax, alist);
    alist.add ("description", "\
Extract the dimension of a number as a string.");
    BuildBase::add_type (Stringer::component, "dimension", alist, syntax, &make);
  }
} StringerDimension_syntax;

struct StringerIdentity : public Stringer
{
  const std::string val;

  // Simulation.
  void tick (const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const
  { return false; }
  std::string value (const Scope&) const
  { return val; }

  // Create.
  bool initialize (Treelog&)
  { return true; }

  bool check (const Scope&, Treelog&) const
  { return true; }
  StringerIdentity (Block& al)
    : Stringer (al),
      val (al.name ("value"))
  { }
  ~StringerIdentity ()
  { }
};

static struct StringerIdentitySyntax
{
  static Model& make (Block& al)
  { return *new StringerIdentity (al); }
  StringerIdentitySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "\
Return the specified value.");
    syntax.add ("value", Syntax::String, Syntax::Const, "\
Constant value.");
    BuildBase::add_type (Stringer::component, "identity", alist, syntax, &make);
  }
} StringerIdentity_syntax;

static BuildBase Stringer_init (Stringer::component, Stringer::description);

