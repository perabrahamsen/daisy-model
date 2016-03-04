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

#define BUILD_DLL

#include "stringer.h"
#include "boolean.h"
#include "number.h"
#include "submodeler.h"
#include "frame.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include "block_model.h"
#include <sstream>
#include <vector>
#include <memory>

const char *const Stringer::component = "string";

symbol
Stringer::library_id () const
{
  static const symbol id (component);
  return id;
}

const std::string& 
Stringer::title () const
{ return name.name (); }

Stringer::Stringer (const BlockModel& al)
  : name (al.type_name ())
{ }

Stringer::~Stringer ()
{ }

struct StringerCond : public Stringer
{
  // Parameters.
  struct Clause
  {
    const std::unique_ptr<Boolean> condition;
    const symbol  value;
    static void load_syntax (Frame& frame)
    {
      frame.declare_object ("condition", Boolean::component, "\
Condition to test for.");
      frame.declare_string ("value", Attribute::Const, "\
Value to return.");
      frame.order ("condition", "value");
    }
    Clause (const Block& al)
      : condition (Librarian::build_item<Boolean> (al, "condition")),
        value (al.name ("value"))
    { }
  };
  std::vector<const Clause*> clauses;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      clauses[i]->condition->tick (units, scope, msg);
  }
  bool missing (const Scope&) const
  { return false; }
  symbol value (const Scope& scope) const
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      if (clauses[i]->condition->value (scope))
        return clauses[i]->value;
    throw "No matching conditions";
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  {
    bool ok = true;
    for (size_t i = 0; i < clauses.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (msg, tmp.str ());
        if (!clauses[i]->condition->initialize (units, scope, msg))
          ok = false;
      }
    return ok;
  }
  bool check (const Units&, const Scope& scope, Treelog& msg) const
  { 
    for (size_t i = 0; i < clauses.size (); i++)
      if (clauses[i]->condition->value (scope))
        return true;
    msg.error ("No clause matches");
    return false; 
  }
  StringerCond (const BlockModel& al)
    : Stringer (al),
      clauses (map_submodel_const<Clause> (al, "clauses"))
  { }
  ~StringerCond ()
  { sequence_delete (clauses.begin (), clauses.end ()); }
};

static DeclareSubmodel 
stringer_cond_clause_submodel (StringerCond::Clause::load_syntax,
                              "StringerCondClause", "\
If condition is true, return value.");

static struct StringerCondSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StringerCond (al); }
  StringerCondSyntax ()
    : DeclareModel (Stringer::component, "cond", "\
Return the value of the first clause whose condition is true.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_submodule_sequence ("clauses", Attribute::Const, "\
List of clauses to match for.",
                                   StringerCond::Clause::load_syntax);
    frame.order ("clauses");
  }
} StringerCond_syntax;

struct StringerNumber : public Stringer
{
  // Parameters.
  std::unique_ptr<Number> number;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { number->tick (units, scope, msg); }
  bool missing (const Scope& scope) const
  { return number->missing (scope); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    Treelog::Open nest (msg, name);
    return number->initialize (units, scope, msg); 
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    return number->check (units, scope, msg); 
  }
  StringerNumber (const BlockModel& al)
    : Stringer (al),
      number (Librarian::build_item<Number> (al, "number"))
  { }
  ~StringerNumber ()
  { }
};

static struct StringerNumberSyntax : public DeclareBase
{
  StringerNumberSyntax ()
    : DeclareBase (Stringer::component, "number", "\
Extract the value of a number.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("number", Number::component, "\
Number to manipulate."); 
  }
} StringerNumber_syntax;

struct StringerValue : public StringerNumber
{
  const int precision;

  symbol value (const Scope& scope) const
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

  StringerValue (const BlockModel& al)
    : StringerNumber (al),
      precision (al.integer ("precision", -1))
  { }
};

static struct StringerValueSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StringerValue (al); }
  StringerValueSyntax ()
    : DeclareModel (Stringer::component, "value", "number", "\
Extract the value of a number as a string.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_integer ("precision", Attribute::OptionalConst, "\
Number of decimals after point.  By default, use a floating format.");
  }
} StringerValue_syntax;

struct StringerDimension : public StringerNumber
{
  symbol value (const Scope& scope) const
  { return number->dimension (scope); }

  StringerDimension (const BlockModel& al)
    : StringerNumber (al)
  { }
};

static struct StringerDimensionSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StringerDimension (al); }
  StringerDimensionSyntax ()
    : DeclareModel (Stringer::component, "dimension", "number", "\
Extract the dimension of a number as a string.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} StringerDimension_syntax;

struct StringerIdentity : public Stringer
{
  const symbol val;

  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const
  { return false; }
  symbol value (const Scope&) const
  { return val; }

  // Create.
  bool initialize (const Units&, const Scope&, Treelog&)
  { return true; }

  bool check (const Units&, const Scope&, Treelog&) const
  { return true; }
  StringerIdentity (const BlockModel& al)
    : Stringer (al),
      val (al.name ("value"))
  { }
  ~StringerIdentity ()
  { }
};

static struct StringerIdentitySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new StringerIdentity (al); }
  StringerIdentitySyntax ()
    : DeclareModel (Stringer::component, "identity", "\
Return the specified value.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_string ("value", Attribute::Const, "\
Constant value.");
  }
} StringerIdentity_syntax;

static struct StringerInit : public DeclareComponent 
{
  StringerInit ()
    : DeclareComponent (Stringer::component, "\
Generic representation of strings.")
  { }
} Stringer_init;

