// soil_heat.h
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


#ifndef SOIL_HEAT_H
#define SOIL_HEAT_H

#include <vector>

class AttributeList;
class Block;
class Log;
class Syntax;
class Geometry;
class Soil;
class SoilWater;
class Treelog;

class SoilHeat
{
  // Simulation.
protected:
  std::vector<double> T_;
public:
  double T (size_t i) const // [dg C]
  { return T_[i]; }
  virtual double top_flux (const Geometry& geo,
                           const Soil&, const SoilWater&) const = 0;
  virtual double T_surface_snow (const Geometry& geo,
                                 const Soil& soil,
                                 const SoilWater& soil_water,
                                 double T_snow,
                                 double K_snow,
                                 double dZs) const = 0;

  // Create and destroy.
public:
  void output_base (Log&) const;
  bool check (size_t n, Treelog&) const;
  static void load_base (Syntax&, AttributeList&);
  SoilHeat (const Block&);
  void initialize_base (const AttributeList& al, 
                        const Geometry& geo,
                        Treelog&);
  virtual ~SoilHeat ();
};

#endif // SOIL_HEAT_H
