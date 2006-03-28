// soil_heat.h
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


#ifndef SOIL_HEAT_H
#define SOIL_HEAT_H

#include <string>
#include <vector>

class AttributeList;
class Log;
class Surface;
class Bioclimate;
class Syntax;
class Soil;
class SoilWater;
class Weather;
class Time;
class Treelog;

class SoilHeat
{
  struct Implementation;
  Implementation& impl;

  enum state_t { liquid, freezing, frozen, thawing };
  state_t state (size_t i) const;
  double capacity (const Soil&, const SoilWater&, size_t i) const;
  double capacity_apparent (const Soil&, const SoilWater&, size_t i) const;
public:
  double top_flux (const Soil&, const SoilWater&) const; // [W/m^2]
  void tick (const Time&, const Soil&, SoilWater&, 
	     const Surface&, const Weather& weather);
  double energy (const Soil&, const SoilWater&, double from, double to) const;
  void set_energy (const Soil&, const SoilWater&, 
		   double from, double to, double energy);
  void swap (const Soil&, double from, double middle, double to);
  double source (size_t i) const;
  void set_source (size_t i, double value); // [erg/cm^3/h]
  double T (unsigned int i) const; // [dg C]
  void output (Log&) const;
  bool check (unsigned n, Treelog&) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilHeat (const AttributeList&);
  void initialize (const AttributeList& al, 
		   const Soil& soil, const Time& time, const Weather& weather,
		   Treelog&);
  ~SoilHeat ();
};

#endif // SOIL_HEAT_H
