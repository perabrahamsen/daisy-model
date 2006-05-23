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
#include "macro.h"
#include "transport.h"
#include "mactrans.h"

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

class Movement
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  virtual Geometry& geometry () const = 0;
  virtual SoilHeat& soil_heat () const = 0;

  // Simulation.
public:
  virtual void macro_tick (const Soil&, SoilWater&, Surface&, Treelog&) = 0;
  virtual void tick (const Soil&, SoilWater&, Surface&, Groundwater&,
                     const Time&, const Weather&, Treelog&) = 0;
  virtual void solute (const Soil&, const SoilWater&, 
                       const double J_in, Solute&, Treelog&) = 0;
  virtual void element (const Soil&, const SoilWater&, 
                        Element&, Adsorption&,
                        double diffusion_coefficient, Treelog&) = 0;
  virtual void ridge (Surface&, const Soil&, const SoilWater&, 
                      const AttributeList&) = 0;

  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual bool check (Treelog& err) const = 0;
  virtual void initialize (const AttributeList&,
                           const Soil&, const Groundwater&,
                           const Time&, const Weather&, Treelog&) = 0;
  static const AttributeList& default_model ();
  static void load_vertical (Syntax& syntax, AttributeList& alist);
  static Movement* build_vertical (Block& al);
protected:
  Movement (Block&);
public:
  virtual ~Movement ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Movement>::Content* Librarian<Movement>::content;
#endif

static Librarian<Movement> Movement_init ("movement");

#endif // MOVEMENT_H
