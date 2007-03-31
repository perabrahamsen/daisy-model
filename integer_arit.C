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


#include "integer.h"
#include "syntax.h"
#include "alist.h"
#include "vcheck.h"
#include "assertion.h"
#include "memutils.h"
#include <sstream>
#include <memory>

using namespace std;

struct IntegerOperand : public Integer
{
  // Parameters.
  const auto_ptr<Integer> operand;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return operand->missing (scope); }

  // Create.
  bool initialize (Treelog& err)
  { 
    Treelog::Open nest (err, name);
    return operand->initialize (err); 
  }
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    return operand->check (scope, err); 
  }
  IntegerOperand (Block& al)
    : Integer (al),
      operand (BuildBase::build_item<Integer> (al, "operand"))
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
  IntegerSqr (Block& al)
    : IntegerOperand (al)
  { }
};

static struct IntegerSqrSyntax
{
  static Model& make (Block& al)
  { return *new IntegerSqr (al); }
  IntegerSqrSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the square of its argument.");
    syntax.add_object ("operand", Integer::component,
                       "Operand for this function.");
    syntax.order ("operand");
    BuildBase::add_type (Integer::component, "sqr", alist, syntax, &make);
  }
} IntegerSqr_syntax;

struct IntegerOperands : public Integer
{
  // Parameters.
  const vector<Integer*> operands;

  // Use.
  bool missing (const Scope& scope) const 
  { 
    for (size_t i = 0; i < operands.size (); i++)
      if (operands[i]->missing (scope))
        return true;
    return false;
  }

  // Create.
  bool initialize (Treelog& err)
  { 
    bool ok = true;
    for (size_t i = 0; i < operands.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (err, tmp.str ());
        
        if (!operands[i]->initialize (err))
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
  IntegerOperands (Block& al)
    : Integer (al),
      operands (BuildBase::build_vector<Integer> (al, "operands"))
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
  IntegerMax (Block& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerMaxSyntax
{
  static Model& make (Block& al)
  { return *new IntegerMax (al); }
  IntegerMaxSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the largest value of its operands.");
    syntax.add_object ("operands", Integer::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
    syntax.add_check ("operands", VCheck::min_size_1 ());
    syntax.order ("operands");
    BuildBase::add_type (Integer::component, "max", alist, syntax, &make);
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
  IntegerMin (Block& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerMinSyntax
{
  static Model& make (Block& al)
  { return *new IntegerMin (al); }
  IntegerMinSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the smallest value of its operands.");
    syntax.add_object ("operands", Integer::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
    syntax.add_check ("operands", VCheck::min_size_1 ());
    syntax.order ("operands");
    BuildBase::add_type (Integer::component, "min", alist, syntax, &make);
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
  IntegerProduct (Block& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerProductSyntax
{
  static Model& make (Block& al)
  { return *new IntegerProduct (al); }
  IntegerProductSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the product of its operands.");
    syntax.add_object ("operands", Integer::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
    syntax.order ("operands");
    BuildBase::add_type (Integer::component, "*", alist, syntax, &make);
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
  IntegerSum (Block& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerSumSyntax
{
  static Model& make (Block& al)
  { return *new IntegerSum (al); }
  IntegerSumSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the sum of its operands.");
    syntax.add_object ("operands", Integer::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    syntax.add_check ("operands", IntegerOperands::unique);
#endif // CHECK_OPERANDS_DIM
    syntax.order ("operands");
    BuildBase::add_type (Integer::component, "+", alist, syntax, &make);
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
  IntegerSubtract (Block& al)
    : IntegerOperands (al)
  { }
};

static struct IntegerSubtractSyntax
{
  static Model& make (Block& al)
  { return *new IntegerSubtract (al); }
  IntegerSubtractSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Negate integer or subtract integers.\n\
With one operand, negates it.  With more than one operand,\n\
subtracts all but the first from the first.");
    syntax.add_object ("operands", Integer::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
    syntax.order ("operands");
    BuildBase::add_type (Integer::component, "-", alist, syntax, &make);
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
  IntegerDivide (Block& al)
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
  IntegerModulo (Block& al)
    : IntegerDivide (al)
  { }
};

static struct IntegerModuloSyntax
{
  static Model& make_mod (Block& al)
  { return *new IntegerModulo (al); }
  static Model& make_div (Block& al)
  { return *new IntegerDivide (al); }
  IntegerModuloSyntax ()
  {
    // div
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();

      alist.add ("description", 
                 "Modulo the first operand by the rest.");
      syntax.add_object ("operands", Integer::component,
                         Syntax::Const, 2,
                         "The operands for this function.");
      syntax.order ("operands");
      BuildBase::add_type (Integer::component, "mod", alist, syntax, &make_div);
    }
    // mod
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();

      alist.add ("description", 
                 "Divide the first operand by the rest.");
      syntax.add_object ("operands", Integer::component,
                         Syntax::Const, 2,
                         "The operands for this function.");
      syntax.order ("operands");
      BuildBase::add_type (Integer::component, "div", alist, syntax, &make_mod);
    }
  }
} IntegerDivide_syntax;

