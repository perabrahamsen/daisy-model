// denitrification.h
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


#ifndef DENITRIFICATION_H
#define DENITRIFICATION_H

#include "plf.h"
#include <vector>

class AttributeList;
class Syntax;
class Soil;
class SoilWater;
class SoilHeat;
class SoilNO3;
class OrganicMatter;
class Log;

class Denitrification
{
  // Parameters.
private: 
  const bool active_underground; // True, iff turnover happens below rootzone.
  const bool active_groundwater; // True, iff turnover happens in groundwater.
  const double K;
  const double K_fast;
  const double alpha;
  const double alpha_fast;
  const PLF heat_factor;
  const PLF water_factor;
  const PLF water_factor_fast;
  const double redox_height;	// Chemical denitrification below this depth.

  // Log variable.
private:
  std::vector<double> converted;
  std::vector<double> converted_fast;
  std::vector<double> converted_redox;
  std::vector<double> potential;
  std::vector<double> potential_fast;
  
  // Simulation.
public:
  void output (Log&) const;
  void tick (const Soil&, const SoilWater&, const SoilHeat&, SoilNO3&,
	     const OrganicMatter&);

  // Create.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Denitrification (const AttributeList&);
};


#endif // DENITRIFICATION_H
