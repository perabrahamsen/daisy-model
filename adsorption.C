// adsorption.C -- Equilibrium between sorbed and solute states.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and University of Copenhagen.
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

#include "adsorption.h"
#include "block.h"
#include "librarian.h"
#include "mathlib.h"

// "adsorption" component.

const char *const Adsorption::component = "adsorption";

symbol 
Adsorption::library_id () const
{
  static const symbol id (component);
  return id;
}

bool
Adsorption::full () const
{ return false; }

void
Adsorption::output (Log&) const
{ }

Adsorption::Adsorption (const char *const type)
  : ModelDerived (symbol (type))
{ }

Adsorption::Adsorption (const Block& al)
  : ModelDerived (al.type_name ())
{ }

Adsorption::~Adsorption ()
{ }

static struct AdsorptionInit : public DeclareComponent
{
  AdsorptionInit () 
    : DeclareComponent (Adsorption::component, "\
This component describes the adsorption of a chemical to the soil,\n\
which among other things affects how large a fraction can be\n\
transported with the water.")
  { }
} Adsorption_init;

// "linear" special.

AdsorptionLinear::AdsorptionLinear (const Block& al)
  : Adsorption (al)
{ }

// "none" model.

class AdsorptionNone : public Adsorption
{
  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C) const
  { return C * Theta; }
  double M_to_C (const Soil&, double Theta, int, double M) const
  { return M / Theta; }

  // Create.
public:
  AdsorptionNone ()
    : Adsorption ("none")
  { }
  AdsorptionNone (const Block& al)
    : Adsorption (al)
  { }
};

const Adsorption& 
Adsorption::none ()
{
  static const AdsorptionNone none;
  return none;
}

static struct AdsorptionNoneSyntax : DeclareModel
{
  Model* make (const Block& al) const
  { return new AdsorptionNone (al); }
  AdsorptionNoneSyntax ()
    : DeclareModel (Adsorption::component, "none", "No adsorption.\n\
Used for solutes that are not adsorped to the soil.")
  { }
  void load_frame (Frame& frame) const
  { }
} AdsorptionNone_syntax;

// "full" model.

class AdsorptionFull : public Adsorption
{
  // Simulation.
public:
  bool full () const
  { return true; }

  double C_to_M (const Soil&, double Theta, int, double C) const
  { 
    if (fabs (C) < 1.0e-100)
      return 0.0;

    // If we initialized a non-zero C, put it all in M right away.
    return C * Theta;
  }
  double M_to_C (const Soil&, double, int, double) const
  { return 0; }

  // Create.
public:
  AdsorptionFull (const Block& al)
    : Adsorption (al)
  { }
};

static struct AdsorptionFullSyntax : DeclareModel
{
  Model* make (const Block& al) const
  { return new AdsorptionFull (al); }
  AdsorptionFullSyntax ()
    : DeclareModel (Adsorption::component, "full", "Full adsorption.\n\
Used for non-solutes, fully adsorped in the soil.")
  { }
  void load_frame (Frame& frame) const
  { }
} AdsorptionFull_syntax;

// adsorption.C ends here.

