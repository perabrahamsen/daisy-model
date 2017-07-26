// svat.h --- Soil, Vegetation and ATmostphere.
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


#ifndef SVAT_H
#define SVAT_H

#include "model_derived.h"
#include "symbol.h"

class Log;
class Geometry;
class Soil;
class SoilHeat;
class SoilWater;
class Weather;
class Vegetation;
class Surface;
class Pet;
class Bioclimate;
class BlockModel;
class Treelog;
class Movement;

class SVAT : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void tick (const Weather&, const Vegetation&,
		     const Geometry&, 
                     const Soil&, const SoilHeat&, const double T_bottom,
		     const SoilWater&, 
                     const Bioclimate&,
		     const Movement&, 
		     const double dt /* [h] */,
		     const double max_T /* [dg C] */, 
		     const double max_ec /* [Pa] */,
		     Treelog&) = 0;
  virtual void output (Log&) const;
  virtual double production_stress () const = 0; // []
  virtual void solve (const double /* shadow stomata cond. [m/s]*/, 
                      const double /* sunlit stomata cond. [m/s]*/, 
                      Treelog&) = 0;
  virtual bool stable () const;                      // Stable solution found?
  virtual double transpiration () const = 0; // [mm/h]
  virtual double CanopyTemperature () const = 0; // [dg C]
  virtual double SunLeafTemperature () const = 0; // [dg C]
  virtual double ShadowLeafTemperature () const = 0; // [dg C]
  virtual double CanopyVapourPressure () const = 0;  // [Pa]
  virtual double SunBoundaryLayerWaterConductivity () const = 0; // [m/s]
  virtual double ShadowBoundaryLayerWaterConductivity () const = 0; // [m/s]
  virtual double SoilSurfaceTemperature () const = 0; // [dg C]
  
  // Create and Destroy.
public:
  virtual bool check (const Weather& weather, Treelog& msg) const = 0;
protected:
  SVAT (const BlockModel&);
public:
  virtual void summarize (Treelog& msg) const = 0;
  ~SVAT ();
};

#endif // SVAT_H

// svat.h ends here.
