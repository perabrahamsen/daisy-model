// tertiary.h --- Transport of water and solutes outside the matrix.
// 
// Copyright 2008 Per Abrahamsen and KU.
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
//
// URL: <http://code.google.com/p/daisy-model/wiki/ComponentTertiary>

#ifndef TERTIARY_H
#define TERTIARY_H

#include "model_derived.h"
#include <map>

class BlockModel;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Surface;
class Chemical;
class Scope;
class Treelog;
class Log;
class Units;
class Groundwater;

class Tertiary : public ModelDerived
{
  // Identity.
public:
  static const char *const component;
  symbol library_id () const;
  virtual bool has_macropores () = 0;

  // Simulation.
public:
  // - For use by Column.
  virtual void tick_source (const Geometry&, const Soil&, const SoilHeat&, 
                            SoilWater&, Treelog&) = 0;
  virtual double suggest_dt (double weather_dt, double max_pond) const;
  virtual void tick (const Units&, 
                     const Geometry&, const Soil&, const SoilHeat&,
                     const double dt, SoilWater&, Surface&, Treelog&) = 0;
  virtual void remove_solute (const symbol) = 0;
  virtual double total_solute (const Geometry&,
			       const symbol chem) const = 0; //[g/m^2]

public:
  // - For use in Movement::solute
  virtual void solute (const Geometry&, const SoilWater&, 
                       const std::map<size_t, double>& J_tertiary,
                       const double dt,
                       Chemical&, Treelog&) = 0;

  // - Output
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual bool initialize (const Units&,
                           const Geometry&, const Soil&, SoilWater&,
                           const Scope&, 
                           const Groundwater&, Treelog&) = 0;
  virtual bool check (const Geometry&, Treelog&) const = 0;
protected:
  explicit Tertiary (const BlockModel& al);
public:
  ~Tertiary ();
};

#endif // TERTIARY_H
