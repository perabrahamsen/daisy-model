// boolean_number.C --- Boolean operations on numbers.
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

#include "boolean.h"
#include "frame.h"
#include "number.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include "block_model.h"
#include <sstream>
#include <vector>

struct BooleanNumbers : public Boolean
{
  const std::vector<Number*> operand;

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

    symbol dim = Attribute::Unknown ();
    for (size_t i = 0; i < operand.size (); i++)
      if (!operand[i]->check (units, scope, msg))
        ok = false;
      else 
        {
          static const symbol blank ("");
          symbol new_dim = operand[i]->dimension (scope);
          if (new_dim == Attribute::None ()
              || new_dim == Attribute::Fraction ())
            new_dim = blank;
          if (new_dim != dim)
            {
              if (dim == Attribute::Unknown ())
                dim = new_dim;
              else if (new_dim != Attribute::Unknown ())
                {
                  msg.error ("I don't know how to compare [" + dim + "] with ["
                             + new_dim + "]");
                  dim = new_dim;
                  ok = false;
                }
            }
        }
    return ok;
  }
  BooleanNumbers (const BlockModel& al)
    : Boolean (al),
      operand (Librarian::build_vector<Number> (al, "operands"))
  { }
  ~BooleanNumbers ()
  { sequence_delete (operand.begin (), operand.end ()); }
};

struct BooleanNumbersSyntax : public DeclareBase
{
  BooleanNumbersSyntax ()
    : DeclareBase (Boolean::component, "numbers", "\
Base class for boolean expressions involving numbers.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("operands", Number::component, 
                       Attribute::Const, Attribute::Variable, "\
List of operands to compare.");
    frame.order ("operands");
  }
} BooleanNumbers_syntax;

struct BooleanNumGT : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev > next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumGT (const BlockModel& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumGTSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanNumGT (al); }
  BooleanNumGTSyntax ()
    : DeclareModel (Boolean::component, ">", "numbers",
                    "True iff each operand is larger than the next.")
  { }
  void load_frame (Frame&) const
  { }
} BooleanNumGT_syntax;

struct BooleanNumGTE : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev >= next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumGTE (const BlockModel& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumGTESyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanNumGTE (al); }
  BooleanNumGTESyntax ()
    : DeclareModel (Boolean::component, ">=", "numbers", "\
True iff each operand is at least as large as the next.")
  { }
  void load_frame (Frame&) const
  { }
} BooleanNumGTE_syntax;

struct BooleanNumLT : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev < next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumLT (const BlockModel& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumLTSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanNumLT (al); }
  BooleanNumLTSyntax ()
    : DeclareModel (Boolean::component, "<", "numbers",
                    "True iff each operand is smaller than the next.")
  { }
  void load_frame (Frame& frame) const
  { }
} BooleanNumLT_syntax;

struct BooleanNumLTE : public BooleanNumbers
{
  bool value (const Scope& scope) const
  { 
    const size_t size = operand.size ();
    if (size < 1)
      return true;
    double prev = operand[0]->value (scope);
    for (size_t i = 1; i < size; i++)
      {
        const double next = operand[i]->value (scope);
        if (!(prev <= next))
          return false;
        prev = next;
      }
    return true;
  }
  BooleanNumLTE (const BlockModel& al)
    : BooleanNumbers (al)
  { }
};

static struct BooleanNumLTESyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BooleanNumLTE (al); }
  BooleanNumLTESyntax ()
    : DeclareModel (Boolean::component, "<=", "numbers", "\
True iff each operand is smaller than or equal to the next.")
  { }
  void load_frame (Frame&) const
  { }
} BooleanNumLTE_syntax;

// boolean_number.C ends here.
