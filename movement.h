// movement.h
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#ifndef MOVEMENT_H
#define MOVEMENT_H

#include "librarian.h"

// Needed for initialization order.
#include "uzmodel.h"
#include "uz1d.h"
#include "macro.h"
#include "transport.h"
#include "mactrans.h"

#include <vector>

class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Element;
class Solute;
class Adsorption;
class Surface;
class Groundwater;
class Weather;
class Time;
class Treelog;

class Movement : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  static const char *const component;

  virtual Geometry& geometry () const = 0;

  // Simulation.
public:
  virtual void macro_tick (const Soil&, SoilWater&, Surface&, 
                           const double dt, Treelog&) = 0;
  virtual void tick (const Soil&, SoilWater&, SoilHeat&, Surface&,
                     Groundwater&, const Time&, const Weather&, 
                     double dt, Treelog&) = 0;
  virtual void solute (const Soil&, const SoilWater&, 
                       const double J_in, Solute&, double dt, Treelog&) = 0;
  virtual void element (const Soil&, const SoilWater&, 
                        Element&, Adsorption&,
                        double diffusion_coefficient, double dt, Treelog&) = 0;
  virtual void ridge (Surface&, const Soil&, const SoilWater&, 
                      const AttributeList&) = 0;

  virtual void output (Log&) const = 0;

  // Heat.
  virtual double surface_snow_T (const Soil&, const SoilWater&, const SoilHeat&,
                                 double T_snow, double K_snow,
                                 double dZs) const = 0;
  virtual std::vector<double> default_heat (const Soil&, 
                                            const Time&, const Weather&) = 0;

  // Create and Destroy.
public:
  virtual bool check (Treelog& err) const = 0;
  virtual void initialize (Block&, const AttributeList&,
                           const Soil&, const Groundwater&) = 0;
  static const AttributeList& default_model ();
  static void load_vertical (Syntax& syntax, AttributeList& alist);
  static Movement* build_vertical (Block& al);
protected:
  Movement (Block&);
public:
  ~Movement ();
};

#endif // MOVEMENT_H
