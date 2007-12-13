// number_arit.C -- Arithmetics on numbers.
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

#include "number.h"
#include "syntax.h"
#include "alist.h"
#include "units.h"
#include "vcheck.h"
#include "mathlib.h"
#include "memutils.h"
#include "block.h"
#include "librarian.h"
#include "submodeler.h"
#include <sstream>
#include <memory>

struct NumberOperand : public Number
{
  // Parameters.
  const std::auto_ptr<Number> operand;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { operand->tick (scope, msg); }
  bool missing (const Scope& scope) const 
  { return operand->missing (scope); }
  symbol dimension (const Scope& scope) const
  {
    if (operand->dimension (scope) == Syntax::none ())
      return Syntax::none ();

    return Syntax::unknown (); 
  }

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
  NumberOperand (Block& al)
    : Number (al),
      operand (Librarian::build_item<Number> (al, "operand"))
  { }
};

struct NumberLog10 : public NumberOperand
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double v = operand->value (scope);
    daisy_assert (v > 0.0);
    return log10 (v); 
  }

  // Create.
  NumberLog10 (Block& al)
    : NumberOperand (al)
  { }
};

static struct NumberLog10Syntax
{
  static Model& make (Block& al)
  { return *new NumberLog10 (al); }
  NumberLog10Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the base 10 logarithm of its argument.");
    syntax.add_object ("operand", Number::component,
                       "Operand for this function.");
    syntax.order ("operand");
    Librarian::add_type (Number::component, "log10", alist, syntax, &make);
  }
} NumberLog10_syntax;

struct NumberLn : public NumberOperand
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double v = operand->value (scope);
    daisy_assert (v > 0.0);
    return log (v); 
  }

  // Create.
  NumberLn (Block& al)
    : NumberOperand (al)
  { }
};

static struct NumberLnSyntax
{
  static Model& make (Block& al)
  { return *new NumberLn (al); }
  NumberLnSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the natural logarithm of its argument.");
    syntax.add_object ("operand", Number::component,
                       "Operand for this function.");
    syntax.order ("operand");
    Librarian::add_type (Number::component, "ln", alist, syntax, &make);
  }
} NumberLn_syntax;

struct NumberExp : public NumberOperand
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double v = operand->value (scope);
    return exp (v); 
  }

  // Create.
  NumberExp (Block& al)
    : NumberOperand (al)
  { }
};

static struct NumberExpSyntax
{
  static Model& make (Block& al)
  { return *new NumberExp (al); }
  NumberExpSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the exponential of its argument.");
    syntax.add_object ("operand", Number::component,
                       "Operand for this function.");
    syntax.order ("operand");
    Librarian::add_type (Number::component, "exp", alist, syntax, &make);
  }
} NumberExp_syntax;

struct NumberSqrt : public NumberOperand
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double v = operand->value (scope);
    daisy_assert (v >= 0.0);
    return sqrt (v); 
  }

  // Create.
  NumberSqrt (Block& al)
    : NumberOperand (al)
  { }
};

static struct NumberSqrtSyntax
{
  static Model& make (Block& al)
  { return *new NumberSqrt (al); }
  NumberSqrtSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the square root of its argument.");
    syntax.add_object ("operand", Number::component,
                       "Operand for this function.");
    syntax.order ("operand");
    Librarian::add_type (Number::component, "sqrt", alist, syntax, &make);
  }
} NumberSqrt_syntax;

struct NumberSqr : public NumberOperand
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double v = operand->value (scope);
    return v * v; 
  }
  symbol dimension (const Scope& scope) const
  { 
    const symbol opdim = operand->dimension (scope);
    return Units::multiply (opdim, opdim);
  }

  // Create.
  NumberSqr (Block& al)
    : NumberOperand (al)
  { }
};

static struct NumberSqrSyntax
{
  static Model& make (Block& al)
  { return *new NumberSqr (al); }
  NumberSqrSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the square of its argument.");
    syntax.add_object ("operand", Number::component,
                       "Operand for this function.");
    syntax.order ("operand");
    Librarian::add_type (Number::component, "sqr", alist, syntax, &make);
  }
} NumberSqr_syntax;

struct NumberPow : public Number
{
  // Parameters.
  const std::auto_ptr<Number> base;
  const std::auto_ptr<Number> exponent;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { 
    base->tick (scope, msg);
    exponent->tick (scope, msg);
  }
  bool missing (const Scope& scope) const 
  { return base->missing (scope) || exponent->missing (scope); }
  double value (const Scope& scope) const
  { 
    const double x = base->value (scope);
    const double y = exponent->value (scope);
    daisy_assert (x >= 0.0);
    return pow (x, y); 
  }
  symbol dimension (const Scope&) const 
  { return Syntax::unknown (); }

  // Create.
  bool initialize (Treelog& err)
  { 
    Treelog::Open nest (err, name);
    bool ok = true;
    if (!base->initialize (err))
      ok = false;
    if (!exponent->initialize (err))
      ok = false;
    return ok;
  }
  bool check (const Scope& scope, Treelog& err) const
  {
    Treelog::Open nest (err, name);
    bool ok = true;
    if (!base->check (scope, err))
      ok = false;
    if (!exponent->check (scope, err))
      ok = false;
    return ok;
  }
  NumberPow (Block& al)
    : Number (al),
      base (Librarian::build_item<Number> (al, "base")),
      exponent (Librarian::build_item<Number> (al, "exponent"))
  { }
};

static struct NumberPowSyntax
{
  static Model& make (Block& al)
  { return *new NumberPow (al); }
  NumberPowSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Raise 'base' to the power of 'exponent'.");
    syntax.add_object ("base", Number::component,
                       "The base operand for this function.");
    syntax.add_object ("exponent", Number::component,
                       "The exponent operand for this function.");
    syntax.order ("base", "exponent");
    Librarian::add_type (Number::component, "pow", alist, syntax, &make);
  }
} NumberPow_syntax;

struct NumberOperands : public Number
{
  // Parameters.
  const std::vector<Number*> operands;

  // Utilities.
  symbol unique_dimension (const Scope& scope) const 
  { 
    static const symbol unspecified ("<unspecified>");
    symbol found = unspecified;
    for (size_t i = 0; i < operands.size (); i++)
      if (known (operands[i]->dimension (scope)))
        if (found == unspecified)
          found = operands[i]->dimension (scope);
        else
          {
            if (operands[i]->dimension (scope) != found)
              return Syntax::unknown ();
          }
    
    return found != unspecified ? found : Syntax::unknown ();
  }

  // Use.
  void tick (const Scope& scope, Treelog& msg)
  { 
    for (size_t i = 0; i < operands.size (); i++)
      operands[i]->tick (scope, msg);
  }
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
#ifdef CHECK_OPERANDS_DIM
  static const struct Unique : public VCheck
  {
    void check (const Syntax&, const AttributeList& al,
                const std::string&) const throw (std::string)
    {
      typedef std::vector<const Number*> op_x;

      const struct Operands : public  op_x
      {
        Operands (Block& Block, const std::vector<const AttributeList*>& as)
          : op_x (Librarian:build_vector_const<Number> (as))
        { }
        ~Operands ()
        { sequence_delete (begin (), end ()); }
      } operands (al.alist_sequence ("operands"));
      
      const string* found = NULL;
      for (size_t i = 0; i < operands.size (); i++)
        if (known (operands[i]->dimension (scope)))
          if (found)
            {
              if (operands[i]->dimension (scope) != *found)
                {
                  std::ostringstream tmp;
                  tmp << "Dimension [" << operands[i]->dimension (scope) 
                         << "] differ from [" << *found << "]";
                  throw string (tmp.str ());
                }
            }
          else
            found = &operands[i]->dimension (scope);
    }
    Unique (const Scope& s)
  } unique;
#endif // CHECK_OPERANDS_DIM

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
  NumberOperands (Block& al)
    : Number (al),
      operands (Librarian::build_vector<Number> (al, "operands"))
  { }
  ~NumberOperands ()
  { sequence_delete (operands.begin (), operands.end ()); }
};

#ifdef CHECK_OPERANDS_DIM
const NumberOperands::Unique NumberOperands::unique;
#endif // CHECK_OPERANDS_DIM

struct NumberMax : public NumberOperands
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    daisy_assert (operands.size () > 0);
    double max = -42.42e42;
    for (size_t i = 0; i < operands.size (); i++)
      {
        const double value = operands[i]->value (scope);
        if (i == 0 || value > max)
          max = value;
      }
    return max;
  }
  symbol dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberMax (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberMaxSyntax
{
  static Model& make (Block& al)
  { return *new NumberMax (al); }
  NumberMaxSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the largest value of its operands.");
    syntax.add_object ("operands", Number::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (),
                            NumberOperands::unique);
#endif // CHECK_OPERANDS_DIM
    syntax.add_check ("operands", VCheck::min_size_1 ());
    syntax.order ("operands");
    Librarian::add_type (Number::component, "max", alist, syntax, &make);
  }
} NumberMax_syntax;

struct NumberMin : public NumberOperands
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    daisy_assert (operands.size () > 0);
    double min = 42.42e42;
    for (size_t i = 0; i < operands.size (); i++)
      {
        const double value = operands[i]->value (scope);
        if (i == 0 || value < min)
          min = value;
      }
    return min;
  }
  symbol dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberMin (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberMinSyntax
{
  static Model& make (Block& al)
  { return *new NumberMin (al); }
  NumberMinSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the smallest value of its operands.");
    syntax.add_object ("operands", Number::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (), 
                            NumberOperands::unique);
    syntax.add_check ("operands", all);
#else // !CHECK_OPERANDS_DIM
    syntax.add_check ("operands", VCheck::min_size_1 ());
#endif // !CHECK_OPERANDS_DIM
    syntax.order ("operands");
    Librarian::add_type (Number::component, "min", alist, syntax, &make);
  }
} NumberMin_syntax;

struct NumberProduct : public NumberOperands
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    double product = 1.0;
    for (size_t i = 0; i < operands.size (); i++)
      product *= operands[i]->value (scope);
    return product;
  }
  symbol dimension (const Scope& scope) const 
  { 
    symbol dim = Syntax::none ();
    for (size_t i = 0; i < operands.size (); i++)
      dim = Units::multiply (dim, operands[i]->dimension (scope));
    return dim;
  }

  // Create.
  NumberProduct (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberProductSyntax
{
  static Model& make (Block& al)
  { return *new NumberProduct (al); }
  NumberProductSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the product of its operands.");
    syntax.add_object ("operands", Number::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
    syntax.order ("operands");
    Librarian::add_type (Number::component, "*", alist, syntax, &make);
  }
} NumberProduct_syntax;

struct NumberSum : public NumberOperands
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    double sum = 0.0;
    for (size_t i = 0; i < operands.size (); i++)
      sum += operands[i]->value (scope);
    return sum;
  }
  symbol dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberSum (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberSumSyntax
{
  static Model& make (Block& al)
  { return *new NumberSum (al); }
  NumberSumSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the sum of its operands.");
    syntax.add_object ("operands", Number::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    syntax.add_check ("operands", NumberOperands::unique);
#endif // CHECK_OPERANDS_DIM
    syntax.order ("operands");
    Librarian::add_type (Number::component, "+", alist, syntax, &make);
  }
} NumberSum_syntax;

struct NumberSubtract : public NumberOperands
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    daisy_assert (operands.size () > 0);
    double val = operands[0]->value (scope);
    if (operands.size () == 1)
      return -val; 
    for (size_t i = 1; i < operands.size (); i++)
      val -= operands[i]->value (scope);
    return val;
  }
  symbol dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberSubtract (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberSubtractSyntax
{
  static Model& make (Block& al)
  { return *new NumberSubtract (al); }
  NumberSubtractSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Negate number or subtract numbers.\n\
With one operand, negates it.  With more than one operand,\n\
subtracts all but the first from the first.");
    syntax.add_object ("operands", Number::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (), 
                            NumberOperands::unique);
    syntax.add_check ("operands", all);
#endif // CHECK_OPERANDS_DIM
    syntax.order ("operands");
    Librarian::add_type (Number::component, "-", alist, syntax, &make);
  }
} NumberSubtract_syntax;

struct NumberDivide : public NumberOperands
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    daisy_assert (operands.size () > 0);
    double val = operands[0]->value (scope);
    for (size_t i = 1; i < operands.size (); i++)
      val /= operands[i]->value (scope);
    return val;
  }
  symbol dimension (const Scope&) const 
  { return Syntax::unknown (); }

  // Create.
  NumberDivide (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberDivideSyntax
{
  static Model& make (Block& al)
  { return *new NumberDivide (al); }
  NumberDivideSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Divide the first operand by the rest.");
    syntax.add_object ("operands", Number::component,
                       Syntax::Const, Syntax::Sequence,
                       "The operands for this function.");
    syntax.add_check ("operands", VCheck::min_size_1 ());
    syntax.order ("operands");
    Librarian::add_type (Number::component, "/", alist, syntax, &make);
  }
} NumberDivide_syntax;

// number_arit.C ends here.

