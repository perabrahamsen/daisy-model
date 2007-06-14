// average.C --- Find the average of two numbers.
// 
// Copyright 1996-2001, 2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2005 KVL.
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


#include "average.h"
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include "librarian.h"

// average component.

const char *const Average::component = "average";

Average::Average (Block& al)
  : name (al.identifier ("type"))
{ }

Average::Average (const char *const id)
  : name (id)
{ }

Average::~Average ()
{ }

// arithmetic model.

struct AverageArithmetic : public Average
{
  // Simulation.
  double operator()(double a, double b) const
  { return (a + b) / 2.0; }
  // Create and Destroy.
  AverageArithmetic (Block& al)
    : Average (al)
  { }
  AverageArithmetic (const char *const id)
    : Average (id)
  { }
  ~AverageArithmetic ()
  { }
};

static struct AverageArithmeticSyntax
{
  static Model& make (Block& al)
  { return *new AverageArithmetic (al); }
  AverageArithmeticSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Arithmetic average '(a+b)/2'.");
    Librarian::add_type (Average::component, "arithmetic", alist, syntax, &make);
  }
} AverageArithmetic_syntax;

const AttributeList& 
Average::arithmetic_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
#if 0
      Syntax dummy;
      AverageArithmetic::load_syntax (dummy, alist);
#endif
      alist.add ("type", "arithmetic");
    }
  return alist;
}

std::auto_ptr<const Average>
Average::build_arithmetic ()
{ return std::auto_ptr<const Average> (new AverageArithmetic (__FUNCTION__)); }

// harmonic model.

struct AverageHarmonic : public Average
{
  // Simulation.
  double operator()(double a, double b) const
  { return 2.0 * a * b / (a + b); }
  // Create and Destroy.
  AverageHarmonic (Block& al)
    : Average (al)
  { }
  ~AverageHarmonic ()
  { }
};

static struct AverageHarmonicSyntax
{
  static Model& make (Block& al)
  { return *new AverageHarmonic (al); }
  AverageHarmonicSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Harmonic average '2ab/(a+b)'.");
    Librarian::add_type (Average::component, "harmonic", alist, syntax, &make);
  }
} AverageHarmonic_syntax;

// geometric model.

struct AverageGeometric : public Average
{
  // Simulation.
  double operator()(double a, double b) const
  { return sqrt (a * b); }
  // Create and Destroy.
  AverageGeometric (Block& al)
    : Average (al)
  { }
  AverageGeometric (const char *const id)
    : Average (id)
  { }
  ~AverageGeometric ()
  { }
};

static struct AverageGeometricSyntax
{
  static Model& make (Block& al)
  { return *new AverageGeometric (al); }
  AverageGeometricSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Geometric average 'sqrt(a*b)'.");
    Librarian::add_type (Average::component, "geometric", alist, syntax, &make);
  }
} AverageGeometric_syntax;

std::auto_ptr<const Average>
Average::build_geometric ()
{ return std::auto_ptr<const Average> (new AverageGeometric (__FUNCTION__)); }

static Librarian Average_init (Average::component, "\
Find the average of two numbers.");

// average.C ends here.
