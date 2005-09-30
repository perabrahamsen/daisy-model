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


#include "number.h"
#include "units.h"
#include "vcheck.h"
#include "mathlib.h"
#include "memutils.h"
#include <sstream>
#include <memory>

using namespace std;

struct NumberOperand : public Number
{
  // Parameters.
  const auto_ptr<Number> operand;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return operand->missing (scope); }
  const string& dimension (const Scope&) const
  { return Syntax::Unknown (); }

  // Create.
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    return operand->check (scope, err); 
  }
  NumberOperand (const AttributeList& al)
    : Number (al),
      operand (Librarian<Number>::create (al.alist ("operand")))
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
  NumberLog10 (const AttributeList& al)
    : NumberOperand (al)
  { }
};

static struct NumberLog10Syntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberLog10 (al); }
  NumberLog10Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the base 10 logarithm of its argument.");
    syntax.add ("operand", Librarian<Number>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Number>::add_type ("log10", alist, syntax, &make);
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
  NumberLn (const AttributeList& al)
    : NumberOperand (al)
  { }
};

static struct NumberLnSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberLn (al); }
  NumberLnSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the natural logarithm of its argument.");
    syntax.add ("operand", Librarian<Number>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Number>::add_type ("ln", alist, syntax, &make);
  }
} NumberLn_syntax;

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
  NumberSqrt (const AttributeList& al)
    : NumberOperand (al)
  { }
};

static struct NumberSqrtSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberSqrt (al); }
  NumberSqrtSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the square root of its argument.");
    syntax.add ("operand", Librarian<Number>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Number>::add_type ("sqrt", alist, syntax, &make);
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

  // Create.
  NumberSqr (const AttributeList& al)
    : NumberOperand (al)
  { }
};

static struct NumberSqrSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberSqr (al); }
  NumberSqrSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the square of its argument.");
    syntax.add ("operand", Librarian<Number>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Number>::add_type ("sqr", alist, syntax, &make);
  }
} NumberSqr_syntax;

struct NumberPow : public Number
{
  // Parameters.
  const auto_ptr<Number> base;
  const auto_ptr<Number> exponent;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return base->missing (scope) || exponent->missing (scope); }
  double value (const Scope& scope) const
  { 
    const double x = base->value (scope);
    const double y = exponent->value (scope);
    daisy_assert (x >= 0.0);
    return pow (x, y); 
  }
  const string& dimension (const Scope&) const 
  { return Syntax::Unknown (); }

  // Create.
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
  NumberPow (const AttributeList& al)
    : Number (al),
      base (Librarian<Number>::create (al.alist ("base"))),
      exponent (Librarian<Number>::create (al.alist ("exponent")))
  { }
};

static struct NumberPowSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberPow (al); }
  NumberPowSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Raise 'base' to the power of 'exponent'.");
    syntax.add ("base", Librarian<Number>::library (),
		"The base operand for this function.");
    syntax.add ("exponent", Librarian<Number>::library (),
		"The exponent operand for this function.");
    syntax.order ("base", "exponent");
    Librarian<Number>::add_type ("pow", alist, syntax, &make);
  }
} NumberPow_syntax;

struct NumberOperands : public Number
{
  // Parameters.
  const vector<const Number*> operands;

  // Utilities.
  const string& unique_dimension (const Scope& scope) const 
  { 
    const string* found = NULL;
    for (size_t i = 0; i < operands.size (); i++)
      if (known (operands[i]->dimension (scope)))
        if (found)
          {
            if (operands[i]->dimension (scope) != *found)
              return Syntax::Unknown ();
          }
        else
          found = &operands[i]->dimension (scope);
    
    return found ? *found : Syntax::Unknown ();
  }

  // Use.
  bool missing (const Scope& scope) const 
  { 
    for (size_t i = 0; i < operands.size (); i++)
      if (operands[i]->missing (scope))
        return true;
    return false;
  }

  // Create.
#ifdef CHECK_OPERANDS_DIM
  static const struct Unique : public VCheck
  {
    void check (const Syntax&, const AttributeList& al,
                const std::string&) const throw (std::string)
    {
      typedef vector<const Number*> op_x;

      const struct Operands : public  op_x
      {
        Operands (const vector<AttributeList*>& as)
          : op_x (map_create_const<Number> (as))
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
  NumberOperands (const AttributeList& al)
    : Number (al),
      operands (map_create_const<Number> 
                (al.alist_sequence ("operands")))
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
  const string& dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberMax (const AttributeList& al)
    : NumberOperands (al)
  { }
};

static struct NumberMaxSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberMax (al); }
  NumberMaxSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the largest value of its operands.");
    syntax.add ("operands", Librarian<Number>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (),
                            NumberOperands::unique);
#endif // CHECK_OPERANDS_DIM
    syntax.add_check ("operands", VCheck::min_size_1 ());
    syntax.order ("operands");
    Librarian<Number>::add_type ("max", alist, syntax, &make);
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
  const string& dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberMin (const AttributeList& al)
    : NumberOperands (al)
  { }
};

static struct NumberMinSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberMin (al); }
  NumberMinSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the smallest value of its operands.");
    syntax.add ("operands", Librarian<Number>::library (),
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
    Librarian<Number>::add_type ("min", alist, syntax, &make);
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
  const string& dimension (const Scope&) const 
  { return Syntax::Unknown (); }

  // Create.
  NumberProduct (const AttributeList& al)
    : NumberOperands (al)
  { }
};

static struct NumberProductSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberProduct (al); }
  NumberProductSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the product of its operands.");
    syntax.add ("operands", Librarian<Number>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    syntax.order ("operands");
    Librarian<Number>::add_type ("*", alist, syntax, &make);
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
  const string& dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberSum (const AttributeList& al)
    : NumberOperands (al)
  { }
};

static struct NumberSumSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberSum (al); }
  NumberSumSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the sum of its operands.");
    syntax.add ("operands", Librarian<Number>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    syntax.add_check ("operands", NumberOperands::unique);
#endif // CHECK_OPERANDS_DIM
    syntax.order ("operands");
    Librarian<Number>::add_type ("+", alist, syntax, &make);
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
  const string& dimension (const Scope& scope) const 
  { return unique_dimension (scope); }

  // Create.
  NumberSubtract (const AttributeList& al)
    : NumberOperands (al)
  { }
};

static struct NumberSubtractSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberSubtract (al); }
  NumberSubtractSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Negate number or subtract numbers.\n\
With one operand, negates it.  With more than one operand,\n\
subtracts all but the first from the first.");
    syntax.add ("operands", Librarian<Number>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (), 
                            NumberOperands::unique);
    syntax.add_check ("operands", all);
#endif // CHECK_OPERANDS_DIM
    syntax.order ("operands");
    Librarian<Number>::add_type ("-", alist, syntax, &make);
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
  const string& dimension (const Scope&) const 
  { return Syntax::Unknown (); }

  // Create.
  NumberDivide (const AttributeList& al)
    : NumberOperands (al)
  { }
};

static struct NumberDivideSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberDivide (al); }
  NumberDivideSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Divide the first operand by the rest.");
    syntax.add ("operands", Librarian<Number>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    syntax.add_check ("operands", VCheck::min_size_1 ());
    syntax.order ("operands");
    Librarian<Number>::add_type ("/", alist, syntax, &make);
  }
} NumberDivide_syntax;

