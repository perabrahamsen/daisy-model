// soil_NO3.C
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


#include "soil_NO3.h"
#include "soil.h"
#include "submodel.h"
#include "alist.h"
#include "assertion.h"

double 
SoilNO3::diffusion_coefficient () const
{
  return 3600 * 2.0e-5; // [cm²/h]
}

void 
SoilNO3::default_initialize (const Soil& soil, const SoilWater&)
{
  daisy_assert (C_.size () == 0);
  daisy_assert (M_.size () == 0);
  // We initialize to approximatey half the allowed content in
  // drinking water [ 0.5 * 100 mg NO3/l ~= 5.0e-6 g NO3-N/cm^3 ]
  C_.insert (C_.begin (), soil.size (), 5.0e-6); 
}

void
SoilNO3::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilNO3");
  alist.add ("description", "Nitrate in soil.\n\
If unspecified, initial value will be 50 mg NO3/l.");
  Solute::load_syntax (syntax, alist);
  // Use "none" adsorption by default.
  AttributeList none;
  none.add ("type", "none");
  alist.add ("adsorption", none);
}

SoilNO3::SoilNO3 (const AttributeList& al)
  : Solute (al)
{ }

static Submodel::Register 
soil_NO3_submodel ("SoilNO3", SoilNO3::load_syntax);
