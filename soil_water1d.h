// soil_water1d.h
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


#ifndef SOIL_WATER1D_H
#define SOIL_WATER1D_H

#include "soil_water.h"
#include "macro.h"		// Must be initialized.

class Geometry1D;
class UZmodel;

class SoilWater1D : public SoilWater
{
  // Content.
  std::auto_ptr<UZmodel> top;
  std::auto_ptr<UZmodel> reserve;
  std::auto_ptr<Macro> macro;

  // Simulation.
public:
  void macro_tick (const Geometry1D&, const Soil&, Surface&, Treelog&);
  void tick (const Geometry1D& geo,
             const Soil&, const SoilHeat&, Surface&, Groundwater&, Treelog&);
  void output (Log&) const;

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  SoilWater1D (Block&);
  void initialize (const AttributeList&, 
		   const Geometry1D& geo,
                   const Soil& soil, const Groundwater& groundwater,
		   Treelog&);
  ~SoilWater1D ();
};

#endif // SOIL_WATER1D_H
