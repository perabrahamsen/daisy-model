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


#include "average.h"
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"

// average component.

template<>
BuildBase* Librarian<Average>::content = NULL;

const char *const Average::description = "\
Find the average of two numbers.";

Average::Average (Block& al)
  : name (al.identifier ("type"))
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
    Librarian<Average>::add_type ("arithmetic", alist, syntax, &make);
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
    Librarian<Average>::add_type ("harmonic", alist, syntax, &make);
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
    Librarian<Average>::add_type ("geometric", alist, syntax, &make);
  }
} AverageGeometric_syntax;

// average.C ends here.
