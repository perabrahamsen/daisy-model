// boolean.C --- Booleans in Daisy.
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


#include "boolean.h"
#include "block_model.h"
#include "frame.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include <sstream>
#include <vector>

const char *const Boolean::component = "boolean";

symbol
Boolean::library_id () const
{
  static const symbol id (component);
  return id;
}

const std::string& 
Boolean::title () const
{ return name.name (); }

Boolean::Boolean (const BlockModel& al)
  : name (al.type_name ())
{ }

Boolean::~Boolean ()
{ }

struct BooleanTrue : public Boolean
{
  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope&) const
  { return true; }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog&)
  { return true; }
  bool check (const Units&, const Scope&, Treelog&) const
  { return true; }
  BooleanTrue (const BlockModel& al)
    : Boolean (al)
  { }
};

static struct BooleanTrueSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanTrue (al); }
  BooleanTrueSyntax ()
    : DeclareModel (Boolean::component, "true", 
                    "Always true.")
  { }
  void load_frame (Frame&) const
  { }
} BooleanTrue_syntax;


struct BooleanFalse : public Boolean
{
  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope&) const
  { return false; }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog&)
  { return true; }
  bool check (const Units&, const Scope&, Treelog&) const
  { return true; }
  BooleanFalse (const BlockModel& al)
    : Boolean (al)
  { }
};

static struct BooleanFalseSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanFalse (al); }
  BooleanFalseSyntax ()
    : DeclareModel (Boolean::component, "false", 
                    "Always false.")
  { }
  void load_frame (Frame&) const
  {  }
} BooleanFalse_syntax;

struct BooleanOperands : public Boolean
{
  const std::vector<Boolean*> operand;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { 
    for (size_t i = 0; i < operand.size (); i++)
      operand[i]->tick (units, scope, msg);
  }
  bool missing (const Scope& scope) const
  { 
    for (size_t i = 0; i < operand.size (); i++)
      if (operand[i]->missing (scope))
        return true;
    
    return false;
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    bool ok = true;

    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->initialize (units, scope, msg))
        {
          std::ostringstream tmp;
          tmp << name << "[" << i << "]";
          Treelog::Open nest (msg, tmp.str ());
          ok = false;
        }
    return ok;
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    bool ok = true;

    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->check (units, scope, msg))
        ok = false;

    return ok;
  }
  BooleanOperands (const BlockModel& al)
    : Boolean (al),
      operand (Librarian::build_vector<Boolean> (al, "operands"))
  { }
  ~BooleanOperands ()
  { sequence_delete (operand.begin (), operand.end ()); }
};

static struct BooleanOperandsSyntax : public DeclareBase
{
  BooleanOperandsSyntax ()
    : DeclareBase (Boolean::component, "operands", "\
Base class for boolean expressions involving multiple boolean operands.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("operands", Boolean::component, 
                       Attribute::Const, Attribute::Variable, "\
List of operands to compare.");
    frame.order ("operands");
  }
} BooleanOperands_syntax;

struct BooleanAnd : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->value (scope))
        return false;
    return true;
  }
  BooleanAnd (const BlockModel& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanAndSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanAnd (al); }
  BooleanAndSyntax ()
    : DeclareModel (Boolean::component, "and", "operands",
                    "True if and only if all operands are true.")
  { }
  void load_frame (Frame&) const
  { }
} BooleanAnd_syntax;


struct BooleanOr : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    for (size_t i = 0; i < operand.size (); i++)
      if (operand[i]->value (scope))
        return true;
    return false;
  }
  BooleanOr (const BlockModel& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanOrSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanOr (al); }
  BooleanOrSyntax ()
    : DeclareModel (Boolean::component, "or", "operands",
                    "True if and only if any operand is true.")
  { }
  void load_frame (Frame&) const
  { }
} BooleanOr_syntax;

struct BooleanXOr : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    daisy_assert (operand.size () == 2);
    return operand[0]->value (scope) != operand[1]->value (scope);
  }
  BooleanXOr (const BlockModel& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanXOrSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanXOr (al); }
  BooleanXOrSyntax ()
    : DeclareModel (Boolean::component, "xor", 
                    "True if and only if one operand is true, and one false.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("operands", Boolean::component, 
                       Attribute::Const, 2, "\
The two operands to compare.");
    frame.order ("operands");
  }
} BooleanXOr_syntax;

struct BooleanNot : public BooleanOperands 
{
  bool value (const Scope& scope) const
  { 
    daisy_assert (operand.size () == 1);
    return !operand[0]->value (scope);
  }
  BooleanNot (const BlockModel& al)
    : BooleanOperands (al)
  { }
};

static struct BooleanNotSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanNot (al); }
  BooleanNotSyntax ()
    : DeclareModel (Boolean::component, "not", 
                    "True if and only if the operand is not true.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("operands", Boolean::component, 
                       Attribute::Const, 1, "\
The operand to check.");
    frame.order ("operands");
  }
} BooleanNot_syntax;

static struct BooleanInit : public DeclareComponent 
{
  BooleanInit ()
    : DeclareComponent (Boolean::component, "\
Generic representation of booleans.")
  { }
} Boolean_init;

// boolean.C ends here.
