// adsorption_full.C
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


#include "adsorption.h"
#include "mathlib.h"

class AdsorptionFull : public Adsorption
{
  // Simulation.
public:
  double C_to_M (const Soil&, double, int, const double C) const
  { 
    daisy_assert (fabs (C) < 1.0e-100); 
    return 0.0;
  }
  double M_to_C (const Soil&, double, int, double) const
  { return 0; }

  // Create.
public:
  AdsorptionFull (const AttributeList& al)
    : Adsorption (al.name ("type"))
  { }
};

static struct AdsorptionFullSyntax
{
  static Adsorption& make (const AttributeList& al)
  { return *new AdsorptionFull (al); }

  AdsorptionFullSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Full adsorption.\n\
Used for non-solutes, fully adsorped in the soil.");
    Librarian<Adsorption>::add_type ("full", alist, syntax, &make);
  }
} AdsorptionFull_syntax;
