// solupt.h --- Solute uptake through root system.
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


#ifndef SOLUPT_H
#define SOLUPT_H

#include "model_derived.h"
#include "symbol.h"

#include <vector>

class BlockModel;
class Geometry;
class Soil;
class SoilWater;
class Chemical;
class Log;
class Treelog;

class Solupt : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual double value (const Geometry& geo, const Soil& soil,
                        const SoilWater& soil_water,
                        const std::vector<double> Density,
                        const std::vector<double> H2OExtraction,
                        const double Rad,
                        Chemical& solute,
                        double PotNUpt, // [g/m^2/h]
                        std::vector<double>& uptake,
                        const double I_max,      // [g/cm R/h]
                        const double C_root_min) = 0; // [g/cm^3 W]
  virtual void output (Log& log) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (const Geometry&, Treelog&) = 0;
protected:
  Solupt (const BlockModel&);
public:
  ~Solupt ();
};

#endif // SOLUPT_H
