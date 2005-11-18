// soil_NH4.h
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


#ifndef SOIL_NH4_H
#define SOIL_NH4_H

#include "solute.h"

class SoilNH4 : public Solute
{
public:
  // Substance specific constants.
  double diffusion_coefficient () const; // in free solu. [cm² / h]

private:
  void default_initialize (const Soil& soil, const SoilWater&);
  SoilNH4 (const SoilNH4&);
public:
  static void load_syntax (Syntax&, AttributeList&);
  SoilNH4 (const AttributeList&);
};

#endif // SOIL_NH4_H
