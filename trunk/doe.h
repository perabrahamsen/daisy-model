// doe.h --- Dissolved organic element.  A single element in a compound solute.
// 
// Copyright 2002, 2006 Per Abrahamsen and KVL.
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

#ifndef DOE_H
#define DOE_H

#include <vector>

class Log;
class Geometry;
class Soil;
class SoilWater;
class Treelog;
class Syntax;
class AttributeList;

class DOE
{
  // Content.
public:
  std::vector<double> M;        // Concentration in soil [g/cm^3]
  std::vector<double> C;    // Concentration in soil solution [g/cm^3]
  std::vector<double> S;        // Combined source term.
  std::vector<double> S_p;      // Source term for macropores only.
  std::vector<double> S_drain;	// Source term for soil drainage only.
  std::vector<double> J_matrix;        // Solute transport log in matrix.
  std::vector<double> J_tertiary;      // Solute transport log in macropores.

  // Simulation.
public:
  void output (Log&) const;
  void mix (const Geometry&, const Soil&, const SoilWater&, 
	    double from, double to);
  void swap (const Geometry&, const Soil&, const SoilWater&,
	     double from, double middle, double to);
  void tick (size_t cell_size, const SoilWater& soil_water, double dt);
  
  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Geometry&, 
                   const Soil&, const SoilWater&, Treelog&);
  DOE (const AttributeList& al);
  ~DOE ();
};

#endif // DOE_H
