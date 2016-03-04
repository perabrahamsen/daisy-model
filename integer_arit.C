// integer_arit.C -- Arithmetics on integers.
// 
// Copyright 2004, 2005 Per Abrahamsen and KVL.
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

#include "integer.h"
#include "vcheck.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include "block_model.h"
#include <sstream>
#include <memory>

struct IntegerOperand : public Integer
{
  // Parameters.
  const std::unique_ptr<Integer> operand;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return operand->missing (scope); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& err)
  { 
    Treelog::Open nest (err, name);
    return operand->initialize (units, scope, err); 
  }
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    return operand->check (scope, err); 
  }
  IntegerOperand (const BlockModel& al)
    : Integer (al),
      operand (Librarian::build_item<Integer> (al, "operand"))
  { }
};

struct IntegerSqr : public IntegerOperand
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    const int v = operand->value (scope);
    return v * v; 
  }

  // Create.
  IntegerSqr (const BlockModel& al)
    : IntegerOperand (al)
  { }
};

static struct IntegerSqrSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerSqr (al); }
  IntegerSqrSyntax ()
    : DeclareModel (Integer::component, "sqr", 
	       "Take the square of its argument.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operand", Integer::component,
                       "Operand for this function.");
    frame.order ("operand");
  }
} IntegerSqr_syntax;

struct IntegerOperands : public Integer
{
  // Parameters.
  const std::vector<Integer*> operands;

  // Use.
  bool missing (const Scope& scope) const 
  { 
    for (size_t i = 0; i < operands.size (); i++)
      if (operands[i]->missing (scope))
        return true;
    return false;
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& err)
  { 
    bool ok = true;
    for (size_t i = 0; i < operands.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (err, tmp.str ());
        
        if (!operands[i]->initialize (units, scope, err))
          ok = false;
      }
    return ok;
  }

  bool check (const Scope& scope, Treelog& err) const
  { 
    bool ok = true;
    for (size_t i = 0; i < operands.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (err, tmp.str ());
        
        if (!operands[i]->check (scope, err))
          ok = false;
      }
    return ok;
  }
  IntegerOperands (const BlockModel& al)
    : Integer (al),
      operands (Librarian::build_vector<Integer> (al, "operands"))
  { }
  ~IntegerOperands ()
  { sequence_delete (operands.begin (), operands.end ()); }
};

struct IntegerMax : public IntegerOperands
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    daisy_assert (operands.size () > 0);
    int max = operands[0]->value (scope);
    for (size_t i = 1; i < operands.size (); i++)
      {
        const int value = operands[i]->value (scope);
        if (value > max)
          max = value;
      }
    return max;
  }
  // Create.
  IntegerMax (const BlockModel& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerMaxSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerMax (al); }
  IntegerMaxSyntax ()
    : DeclareModel (Integer::component, "max", 
	       "Use the largest value of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Integer::component,
                       Attribute::Const, Attribute::Variable,
                       "The operands for this function.");
    frame.set_check ("operands", VCheck::min_size_1 ());
    frame.order ("operands");
  }
} IntegerMax_syntax;

struct IntegerMin : public IntegerOperands
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    daisy_assert (operands.size () > 0);
    int min = operands[0]->value (scope);
    for (size_t i = 1; i < operands.size (); i++)
      {
        const int value = operands[i]->value (scope);
        if (value < min)
          min = value;
      }
    return min;
  }

  // Create.
  IntegerMin (const BlockModel& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerMinSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerMin (al); }
  IntegerMinSyntax ()
    : DeclareModel (Integer::component, "min", 
	       "Use the smallest value of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Integer::component,
                       Attribute::Const, Attribute::Variable,
                       "The operands for this function.");
    frame.set_check ("operands", VCheck::min_size_1 ());
    frame.order ("operands");
  }
} IntegerMin_syntax;

struct IntegerProduct : public IntegerOperands
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    int product = 1;
    for (size_t i = 0; i < operands.size (); i++)
      product *= operands[i]->value (scope);
    return product;
  }

  // Create.
  IntegerProduct (const BlockModel& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerProductSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerProduct (al); }
  IntegerProductSyntax ()
    : DeclareModel (Integer::component, "*", 
	       "Use the product of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Integer::component,
                       Attribute::Const, Attribute::Variable,
                       "The operands for this function.");
    frame.order ("operands");
  }
} IntegerProduct_syntax;

struct IntegerSum : public IntegerOperands
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    int sum = 0;
    for (size_t i = 0; i < operands.size (); i++)
      sum += operands[i]->value (scope);
    return sum;
  }

  // Create.
  IntegerSum (const BlockModel& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerSumSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerSum (al); }
  IntegerSumSyntax ()
    : DeclareModel (Integer::component, "+", 
	       "Use the sum of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Integer::component,
                       Attribute::Const, Attribute::Variable,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    frame.set_check ("operands", IntegerOperands::unique);
#endif // CHECK_OPERANDS_DIM
    frame.order ("operands");
  }
} IntegerSum_syntax;

struct IntegerSubtract : public IntegerOperands
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    daisy_assert (operands.size () > 0);
    int val = operands[0]->value (scope);
    if (operands.size () == 1)
      return -val; 
    for (size_t i = 1; i < operands.size (); i++)
      val -= operands[i]->value (scope);
    return val;
  }

  // Create.
  IntegerSubtract (const BlockModel& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerSubtractSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerSubtract (al); }
  IntegerSubtractSyntax ()
    : DeclareModel (Integer::component, "-", 
	       "Negate integer or subtract integers.\n\
With one operand, negates it.  With more than one operand,\n\
subtracts all but the first from the first.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Integer::component,
                       Attribute::Const, Attribute::Variable,
                       "The operands for this function.");
    frame.order ("operands");
  }
} IntegerSubtract_syntax;

struct IntegerDivide : public IntegerOperands
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    daisy_assert (operands.size () == 2);
    int v1 = operands[0]->value (scope);
    int v2 = operands[1]->value (scope);
    if (v2 == 0)
      throw ("Divide by zero");
    return v1 / v2;
  }

  // Create.
  bool check (const Scope& scope, Treelog& err) const
  { 
    bool ok = true;
    for (size_t i = 0; i < operands.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (err, tmp.str ());
        
        if (!operands[i]->check (scope, err))
          ok = false;
        if (i > 0 && operands[i]->value (scope) == 0)
          {
            err.error ("Divide by zero");
            ok = false;
          }
      }
    return ok;
  }
  IntegerDivide (const BlockModel& al)
    : IntegerOperands (al)
  { }
};

struct IntegerModulo : public IntegerDivide
{
  // Simulation.
  int value (const Scope& scope) const
  { 
    daisy_assert (operands.size () == 2);
    int v1 = operands[0]->value (scope);
    int v2 = operands[1]->value (scope);
    if (v2 == 0)
      throw ("Modulo by zero");
    return v1 % v2;
  }

  // Create.
  IntegerModulo (const BlockModel& al)
    : IntegerDivide (al)
  { }
};

static struct IntegerModuloSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerModulo (al); }
  IntegerModuloSyntax ()
    : DeclareModel (Integer::component, "mod", 
                 "Modulo the first operand by the rest.")
  { }
  void load_frame (Frame& frame) const
  {

      frame.declare_object ("operands", Integer::component,
                         Attribute::Const, 2,
                         "The operands for this function.");
      frame.order ("operands");
  }
} IntegerModulo_syntax;

static struct IntegerDivideSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new IntegerDivide (al); }
  IntegerDivideSyntax ()
    : DeclareModel (Integer::component, "div", 
                 "Divide the first operand by the rest.")
  { }
  void load_frame (Frame& frame) const
  {

      frame.declare_object ("operands", Integer::component,
                         Attribute::Const, 2,
                         "The operands for this function.");
      frame.order ("operands");
  }
} IntegerDivide_syntax;

