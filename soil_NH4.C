// soil_NH4.C
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

#define BUILD_DLL

#include "soil_NH4.h"
#include "soil.h"
#include "soil_water.h"
#include "submodel.h"
#include "alist.h"
#include "assertion.h"

double 
SoilNH4::diffusion_coefficient () const
{
  return 3600 * 1.8e-5; // [cm²/h]
}

void 
SoilNH4::default_initialize (const Soil& soil, const SoilWater& soil_water, 
			     const SoilHeat&, Treelog&)
{
  daisy_assert (C_.size () == 0);
  daisy_assert (M_.size () == 0);
  // We initialize to approximatey 5% of the N corresponding to the
  // allowed content of NO3 in drinking water.
  // [ 0.05 * 100 mg/l = 0.5e-6 g/cm^3 ]
  for (unsigned int i = 0; i < soil.size (); i++)
    M_.push_back (0.5e-6 * soil_water.Theta (i));
}

void
SoilNH4::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "SoilNH4");
  alist.add ("description", "Ammonium in soil.\n\
If unspecified, initial value will be 5 mg NH4/l (including adsorbed).");
  Solute::load_syntax (syntax, alist); 
  // Use linear adsorption by default.
  AttributeList linear;
  linear.add ("type", "linear");
  linear.add ("K_clay", 117.116);
  alist.add ("adsorption", linear);
}

SoilNH4::SoilNH4 (Block& al)
  : Solute (al)
{ }

static Submodel::Register 
soil_NH4_submodel ("SoilNH4", SoilNH4::load_syntax);

