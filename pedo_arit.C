// pedo_arit.C -- Arithmetics on pedotransfer functions.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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


#include "pedo.h"
#include "soil.h"
#include "units.h"
#include "vcheck.h"
#include "mathlib.h"
#include <sstream>
#include <memory>

using namespace std;

struct PedotransferIdentity : public Pedotransfer
{
  // Parameters.
  const auto_ptr<Pedotransfer> child;
  const string dim;

  // Simulation.
  double value (const Soil& soil, int lay) const
  { return child->value (soil, lay); }
  const string& dimension () const
  { return dim; }

  // Create.
  bool check_nested (const Soil& soil, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    bool ok = true;

    if (!child->check_nested (soil, err))
      ok = false;
    
    if (known (dim) && known (child->dimension ())
        && !Units::can_convert (child->dimension (), dim))
      {
        err.error ("Cannot convert [" + child->dimension () 
                   + "] to [" + dim + "]");
        ok = false;
      }
    return ok;
  }
  PedotransferIdentity (const AttributeList& al)
    : Pedotransfer (al),
      child (Librarian<Pedotransfer>::create (al.alist ("value"))),
      dim (al.name ("dimension", child->dimension ()))
  { }
};

static struct PedotransferIdentitySyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferIdentity (al); }
  PedotransferIdentitySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Pass value unchanged.");
    syntax.add ("value", Librarian<Pedotransfer>::library (),
		"Operand for this function.");
    syntax.add ("dimension", Syntax::String, Syntax::OptionalConst,
		"Dimension of this value.");
    Librarian<Pedotransfer>::add_type ("identity", alist, syntax, &make);
  }
} PedotransferIdentity_syntax;

struct PedotransferOperand : public Pedotransfer
{
  // Parameters.
  const auto_ptr<Pedotransfer> operand;

  // Simulation.
  const string& dimension () const
  { return Syntax::Unknown (); }

  // Create.
  bool check_nested (const Soil& soil, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    return operand->check_nested (soil, err); 
  }
  PedotransferOperand (const AttributeList& al)
    : Pedotransfer (al),
      operand (Librarian<Pedotransfer>::create (al.alist ("operand")))
  { }
};

struct PedotransferLog10 : public PedotransferOperand
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    const double v = operand->value (soil, lay);
    daisy_assert (v > 0.0);
    return log10 (v); 
  }

  // Create.
  PedotransferLog10 (const AttributeList& al)
    : PedotransferOperand (al)
  { }
};

static struct PedotransferLog10Syntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferLog10 (al); }
  PedotransferLog10Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the base 10 logarithm of its argument.");
    syntax.add ("operand", Librarian<Pedotransfer>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Pedotransfer>::add_type ("log10", alist, syntax, &make);
  }
} PedotransferLog10_syntax;

struct PedotransferLn : public PedotransferOperand
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    const double v = operand->value (soil, lay);
    daisy_assert (v > 0.0);
    return log (v); 
  }

  // Create.
  PedotransferLn (const AttributeList& al)
    : PedotransferOperand (al)
  { }
};

static struct PedotransferLnSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferLn (al); }
  PedotransferLnSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the natural logarithm of its argument.");
    syntax.add ("operand", Librarian<Pedotransfer>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Pedotransfer>::add_type ("ln", alist, syntax, &make);
  }
} PedotransferLn_syntax;

struct PedotransferSqrt : public PedotransferOperand
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    const double v = operand->value (soil, lay);
    daisy_assert (v >= 0.0);
    return sqrt (v); 
  }

  // Create.
  PedotransferSqrt (const AttributeList& al)
    : PedotransferOperand (al)
  { }
};

static struct PedotransferSqrtSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferSqrt (al); }
  PedotransferSqrtSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the square root of its argument.");
    syntax.add ("operand", Librarian<Pedotransfer>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Pedotransfer>::add_type ("sqrt", alist, syntax, &make);
  }
} PedotransferSqrt_syntax;

struct PedotransferSqr : public PedotransferOperand
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    const double v = operand->value (soil, lay);
    return v * v; 
  }

  // Create.
  PedotransferSqr (const AttributeList& al)
    : PedotransferOperand (al)
  { }
};

static struct PedotransferSqrSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferSqr (al); }
  PedotransferSqrSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Take the square of its argument.");
    syntax.add ("operand", Librarian<Pedotransfer>::library (),
		"Operand for this function.");
    syntax.order ("operand");
    Librarian<Pedotransfer>::add_type ("sqr", alist, syntax, &make);
  }
} PedotransferSqr_syntax;

struct PedotransferPow : public Pedotransfer
{
  // Parameters.
  const auto_ptr<Pedotransfer> base;
  const auto_ptr<Pedotransfer> exponent;

  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    const double x = base->value (soil, lay);
    const double y = exponent->value (soil, lay);
    daisy_assert (x >= 0.0);
    return pow (x, y); 
  }
  const string& dimension () const 
  { return Syntax::Unknown (); }

  // Create.
  bool check_nested (const Soil& soil, Treelog& err) const
  {
    Treelog::Open nest (err, name);
    bool ok = true;
    if (!base->check_nested (soil, err))
      ok = false;
    if (!exponent->check_nested (soil, err))
      ok = false;
    return ok;
  }
  PedotransferPow (const AttributeList& al)
    : Pedotransfer (al),
      base (Librarian<Pedotransfer>::create (al.alist ("base"))),
      exponent (Librarian<Pedotransfer>::create (al.alist ("exponent")))
  { }
};

static struct PedotransferPowSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferPow (al); }
  PedotransferPowSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Raise 'base' to the power of 'exponent'.");
    syntax.add ("base", Librarian<Pedotransfer>::library (),
		"The base operand for this function.");
    syntax.add ("exponent", Librarian<Pedotransfer>::library (),
		"The exponent operand for this function.");
    syntax.order ("base", "exponent");
    Librarian<Pedotransfer>::add_type ("pow", alist, syntax, &make);
  }
} PedotransferPow_syntax;

struct PedotransferOperands : public Pedotransfer
{
  // Parameters.
  const vector<const Pedotransfer*> operands;

  // Utilities.
  const string& unique_dimension () const 
  { 
    const string* found = NULL;
    for (size_t i = 0; i < operands.size (); i++)
      if (known (operands[i]->dimension ()))
        if (found)
          {
            if (operands[i]->dimension () != *found)
              return Syntax::Unknown ();
          }
        else
          found = &operands[i]->dimension ();
    
    return found ? *found : Syntax::Unknown ();
  }

  // Create.
  static const struct Unique : public VCheck
  {
    void check (const Syntax&, const AttributeList& al,
                const std::string&) const throw (std::string)
    {
      typedef vector<const Pedotransfer*> op_x;

      const struct Operands : public  op_x
      {
        Operands (const vector<AttributeList*>& as)
          : op_x (map_create_const<Pedotransfer> (as))
        { }
        ~Operands ()
        { sequence_delete (begin (), end ()); }
      } operands (al.alist_sequence ("operands"));
      
      const string* found = NULL;
      for (size_t i = 0; i < operands.size (); i++)
        if (known (operands[i]->dimension ()))
          if (found)
            {
              if (operands[i]->dimension () != *found)
                {
                  std::ostringstream tmp;
                  tmp << "Dimension [" << operands[i]->dimension () 
                         << "] differ from [" << *found << "]";
                  throw string (tmp.str ());
                }
            }
          else
            found = &operands[i]->dimension ();
    }
  } unique;

  bool check_nested (const Soil& soil, Treelog& err) const
  { 
    bool ok = true;
    for (size_t i = 0; i < operands.size (); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]";
        Treelog::Open nest (err, tmp.str ());
        
        if (!operands[i]->check_nested (soil, err))
          ok = false;
      }
    return ok;
  }
  PedotransferOperands (const AttributeList& al)
    : Pedotransfer (al),
      operands (map_create_const<Pedotransfer> 
                (al.alist_sequence ("operands")))
  { }
  ~PedotransferOperands ()
  { sequence_delete (operands.begin (), operands.end ()); }
};

const PedotransferOperands::Unique PedotransferOperands::unique;

struct PedotransferMax : public PedotransferOperands
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    daisy_assert (operands.size () > 0);
    double max = -42.42e42;
    for (size_t i = 0; i < operands.size (); i++)
      {
        const double value = operands[i]->value (soil, lay);
        if (i == 0 || value > max)
          max = value;
      }
    return max;
  }
  const string& dimension () const 
  { return unique_dimension (); }

  // Create.
  PedotransferMax (const AttributeList& al)
    : PedotransferOperands (al)
  { }
};

static struct PedotransferMaxSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferMax (al); }
  PedotransferMaxSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the largest value of its operands.");
    syntax.add ("operands", Librarian<Pedotransfer>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    static VCheck::All all (VCheck::min_size_1 (),
                            PedotransferOperands::unique);
    syntax.add_check ("operands", all);
    syntax.order ("operands");
    Librarian<Pedotransfer>::add_type ("max", alist, syntax, &make);
  }
} PedotransferMax_syntax;

struct PedotransferMin : public PedotransferOperands
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    daisy_assert (operands.size () > 0);
    double min = 42.42e42;
    for (size_t i = 0; i < operands.size (); i++)
      {
        const double value = operands[i]->value (soil, lay);
        if (i == 0 || value < min)
          min = value;
      }
    return min;
  }
  const string& dimension () const 
  { return unique_dimension (); }

  // Create.
  PedotransferMin (const AttributeList& al)
    : PedotransferOperands (al)
  { }
};

static struct PedotransferMinSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferMin (al); }
  PedotransferMinSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the smallest value of its operands.");
    syntax.add ("operands", Librarian<Pedotransfer>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    static VCheck::All all (VCheck::min_size_1 (), 
                            PedotransferOperands::unique);
    syntax.add_check ("operands", all);
    syntax.order ("operands");
    Librarian<Pedotransfer>::add_type ("min", alist, syntax, &make);
  }
} PedotransferMin_syntax;

struct PedotransferProduct : public PedotransferOperands
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    double product = 1.0;
    for (size_t i = 0; i < operands.size (); i++)
      product *= operands[i]->value (soil, lay);
    return product;
  }
  const string& dimension () const 
  { return Syntax::Unknown (); }

  // Create.
  PedotransferProduct (const AttributeList& al)
    : PedotransferOperands (al)
  { }
};

static struct PedotransferProductSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferProduct (al); }
  PedotransferProductSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the product of its operands.");
    syntax.add ("operands", Librarian<Pedotransfer>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    syntax.order ("operands");
    Librarian<Pedotransfer>::add_type ("*", alist, syntax, &make);
  }
} PedotransferProduct_syntax;

struct PedotransferSum : public PedotransferOperands
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    double sum = 0.0;
    for (size_t i = 0; i < operands.size (); i++)
      sum += operands[i]->value (soil, lay);
    return sum;
  }
  const string& dimension () const 
  { return unique_dimension (); }

  // Create.
  PedotransferSum (const AttributeList& al)
    : PedotransferOperands (al)
  { }
};

static struct PedotransferSumSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferSum (al); }
  PedotransferSumSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Use the sum of its operands.");
    syntax.add ("operands", Librarian<Pedotransfer>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    static VCheck::All all (VCheck::min_size_1 (), 
                            PedotransferOperands::unique);
    syntax.add_check ("operands", PedotransferOperands::unique);
    syntax.order ("operands");
    Librarian<Pedotransfer>::add_type ("+", alist, syntax, &make);
  }
} PedotransferSum_syntax;

struct PedotransferSubtract : public PedotransferOperands
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    daisy_assert (operands.size () > 0);
    double val = operands[0]->value (soil, lay);
    if (operands.size () == 1)
      return -val; 
    for (size_t i = 1; i < operands.size (); i++)
      val -= operands[i]->value (soil, lay);
    return val;
  }
  const string& dimension () const 
  { return unique_dimension (); }

  // Create.
  PedotransferSubtract (const AttributeList& al)
    : PedotransferOperands (al)
  { }
};

static struct PedotransferSubtractSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferSubtract (al); }
  PedotransferSubtractSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Negate number or subtract numbers.\n\
With one operand, negates it.  With more than one operand,\n\
subtracts all but the first from the first.");
    syntax.add ("operands", Librarian<Pedotransfer>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    static VCheck::All all (VCheck::min_size_1 (), 
                            PedotransferOperands::unique);
    syntax.add_check ("operands", all);
    syntax.order ("operands");
    Librarian<Pedotransfer>::add_type ("-", alist, syntax, &make);
  }
} PedotransferSubtract_syntax;

struct PedotransferDivide : public PedotransferOperands
{
  // Simulation.
  double value (const Soil& soil, int lay) const
  { 
    daisy_assert (operands.size () > 0);
    double val = operands[0]->value (soil, lay);
    for (size_t i = 1; i < operands.size (); i++)
      val /= operands[i]->value (soil, lay);
    return val;
  }
  const string& dimension () const 
  { return Syntax::Unknown (); }

  // Create.
  PedotransferDivide (const AttributeList& al)
    : PedotransferOperands (al)
  { }
};

static struct PedotransferDivideSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferDivide (al); }
  PedotransferDivideSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Divide the first operand by the rest.");
    syntax.add ("operands", Librarian<Pedotransfer>::library (),
                Syntax::Const, Syntax::Sequence,
		"The operands for this function.");
    syntax.add_check ("operands", VCheck::min_size_1 ());
    syntax.order ("operands");
    Librarian<Pedotransfer>::add_type ("/", alist, syntax, &make);
  }
} PedotransferDivide_syntax;

