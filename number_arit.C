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
#include "units.h"
#include "vcheck.h"
#include "mathlib.h"
#include "memutils.h"
#include "block.h"
#include "librarian.h"
#include "submodeler.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>
#include <memory>

struct NumberOperand : public Number
{
  // Parameters.
  const std::auto_ptr<Number> operand;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { operand->tick (units, scope, msg); }
  bool missing (const Scope& scope) const 
  { return operand->missing (scope); }
  symbol dimension (const Scope& scope) const
  {
    if (operand->dimension (scope) == Value::None ())
      return Value::None ();

    return Value::Unknown (); 
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& err)
  { 
    Treelog::Open nest (err, name);
    return operand->initialize (units, scope, err); 
  }
  bool check (const Units& units, const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    return operand->check (units, scope, err); 
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

static struct NumberLog10Syntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberLog10 (al); }
  NumberLog10Syntax ()
    : DeclareModel (Number::component, "log10", 
	       "Take the base 10 logarithm of its argument.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operand", Number::component,
                       "Operand for this function.");
    frame.order ("operand");
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

static struct NumberLnSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberLn (al); }
  NumberLnSyntax ()
    : DeclareModel (Number::component, "ln", 
	       "Take the natural logarithm of its argument.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operand", Number::component,
                       "Operand for this function.");
    frame.order ("operand");
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

static struct NumberExpSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberExp (al); }
  NumberExpSyntax ()
    : DeclareModel (Number::component, "exp", 
	       "Take the exponential of its argument.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operand", Number::component,
                       "Operand for this function.");
    frame.order ("operand");
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

static struct NumberSqrtSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberSqrt (al); }
  NumberSqrtSyntax ()
    : DeclareModel (Number::component, "sqrt", 
	       "Take the square root of its argument.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operand", Number::component,
                       "Operand for this function.");
    frame.order ("operand");
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

static struct NumberSqrSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberSqr (al); }
  NumberSqrSyntax ()
    : DeclareModel (Number::component, "sqr", 
	       "Take the square of its argument.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operand", Number::component,
                       "Operand for this function.");
    frame.order ("operand");
  }
} NumberSqr_syntax;

struct NumberPow : public Number
{
  // Parameters.
  const std::auto_ptr<Number> base;
  const std::auto_ptr<Number> exponent;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { 
    base->tick (units, scope, msg);
    exponent->tick (units, scope, msg);
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
  { return Value::Unknown (); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& err)
  { 
    Treelog::Open nest (err, name);
    bool ok = true;
    if (!base->initialize (units, scope, err))
      ok = false;
    if (!exponent->initialize (units, scope, err))
      ok = false;
    return ok;
  }
  bool check (const Units& units, const Scope& scope, Treelog& err) const
  {
    Treelog::Open nest (err, name);
    bool ok = true;
    if (!base->check (units, scope, err))
      ok = false;
    if (!exponent->check (units, scope, err))
      ok = false;
    return ok;
  }
  NumberPow (Block& al)
    : Number (al),
      base (Librarian::build_item<Number> (al, "base")),
      exponent (Librarian::build_item<Number> (al, "exponent"))
  { }
};

static struct NumberPowSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberPow (al); }
  NumberPowSyntax ()
    : DeclareModel (Number::component, "pow", 
	       "Raise 'base' to the power of 'exponent'.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("base", Number::component,
                       "The base operand for this function.");
    frame.declare_object ("exponent", Number::component,
                       "The exponent operand for this function.");
    frame.order ("base", "exponent");
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
              return Value::Unknown ();
          }
    
    return found != unspecified ? found : Value::Unknown ();
  }

  // Use.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { 
    for (size_t i = 0; i < operands.size (); i++)
      operands[i]->tick (units, scope, msg);
  }
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
#ifdef CHECK_OPERANDS_DIM
  static const struct Unique : public VCheck
  {
    void check (Metalib&, const Frame& al, const std::string&)
      const throw (std::string)
    {
      typedef std::vector<const Number*> op_x;

      const struct Operands : public  op_x
      {
        Operands (Block& Block, const std::vector<const Frame*>& as)
          : op_x (Librarian:build_vector_const<Number> (as))
        { }
        ~Operands ()
        { sequence_delete (begin (), end ()); }
      } operands (frame_sequence ("operands"));
      
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

  bool check (const Units& units, const Scope& scope, Treelog& err) const
  { 
    bool ok = true;
    for (size_t i = 0; i < operands.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (err, tmp.str ());
        
        if (!operands[i]->check (units, scope, err))
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

static struct NumberMaxSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberMax (al); }
  NumberMaxSyntax ()
    : DeclareModel (Number::component, "max", 
	       "Use the largest value of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Number::component,
                       Value::Const, Value::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (),
                            NumberOperands::unique);
#endif // CHECK_OPERANDS_DIM
    frame.set_check ("operands", VCheck::min_size_1 ());
    frame.order ("operands");
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

static struct NumberMinSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberMin (al); }
  NumberMinSyntax ()
    : DeclareModel (Number::component, "min", 
	       "Use the smallest value of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Number::component,
                       Value::Const, Value::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (), 
                            NumberOperands::unique);
    frame.set_check ("operands", all);
#else // !CHECK_OPERANDS_DIM
    frame.set_check ("operands", VCheck::min_size_1 ());
#endif // !CHECK_OPERANDS_DIM
    frame.order ("operands");
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
    symbol dim = Value::None ();
    for (size_t i = 0; i < operands.size (); i++)
      dim = Units::multiply (dim, operands[i]->dimension (scope));
    return dim;
  }

  // Create.
  NumberProduct (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberProductSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberProduct (al); }
  NumberProductSyntax ()
    : DeclareModel (Number::component, "*", 
	       "Use the product of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Number::component,
                       Value::Const, Value::Sequence,
                       "The operands for this function.");
    frame.order ("operands");
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

static struct NumberSumSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberSum (al); }
  NumberSumSyntax ()
    : DeclareModel (Number::component, "+", 
	       "Use the sum of its operands.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Number::component,
                       Value::Const, Value::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    frame.set_check ("operands", NumberOperands::unique);
#endif // CHECK_OPERANDS_DIM
    frame.order ("operands");
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

static struct NumberSubtractSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberSubtract (al); }
  NumberSubtractSyntax ()
    : DeclareModel (Number::component, "-", 
	       "Negate number or subtract numbers.\n\
With one operand, negates it.  With more than one operand,\n\
subtracts all but the first from the first.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Number::component,
                       Value::Const, Value::Sequence,
                       "The operands for this function.");
#ifdef CHECK_OPERANDS_DIM
    static VCheck::All all (VCheck::min_size_1 (), 
                            NumberOperands::unique);
    frame.set_check ("operands", all);
#endif // CHECK_OPERANDS_DIM
    frame.order ("operands");
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
  { return Value::Unknown (); }

  // Create.
  NumberDivide (Block& al)
    : NumberOperands (al)
  { }
};

static struct NumberDivideSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberDivide (al); }
  NumberDivideSyntax ()
    : DeclareModel (Number::component, "/", 
	       "Divide the first operand by the rest.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("operands", Number::component,
                       Value::Const, Value::Sequence,
                       "The operands for this function.");
    frame.set_check ("operands", VCheck::min_size_1 ());
    frame.order ("operands");
  }
} NumberDivide_syntax;

// number_arit.C ends here.

