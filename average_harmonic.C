// average_harmonic.C -- Harmonic average.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

struct AverageHarmonic : public Average
{
  // Simulation.
  double operator()(double a, double b) const
    { return 2.0 * a * b / (a + b); }
  // Create and Destroy.
  AverageHarmonic (const AttributeList& al)
    : Average (al)
    { }
  ~AverageHarmonic ()
    { }
};

static struct AverageHarmonicSyntax
{
  static Average&
  make (const AttributeList& al)
    { return *new AverageHarmonic (al); }
  AverageHarmonicSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Harmonic average '2ab/(a+b)'.");
      Librarian<Average>::add_type ("harmonic", alist, syntax, &make);
    }
} AverageHarmonic_syntax;
