// soil_chemical.h
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


#ifndef SOIL_CHEMICAL_H
#define SOIL_CHEMICAL_H

#include "solute.h"
#include "plf.h"
#include <vector>

struct Chemical;
struct Soil;
struct SoilWater;
struct SoilHeat;
struct OrganicMatter;
struct Syntax;
struct AttributeList;

class SoilChemical : public Solute
{
  // Content.
public:
  const Chemical& chemical;
private:
  vector<double> decomposed;
  vector<double> uptaken;
  vector<double> lag;
  const PLF lag_increment;

  // Simulation.
public:
  void uptake (const Soil&, const SoilWater&);
  void decompose (const Soil&, const SoilWater&, const SoilHeat&, 
		  const OrganicMatter*);
  void output (Log&) const;
  
public:
  // Substance specific constants.
  double diffusion_coefficient () const; // in free solu. [cm² / h]

  // Create & Destroy.
private:
  static const PLF& no_lag ();
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const AttributeList&, const Soil&, const SoilWater&,
		   Treelog&);
  SoilChemical (const Chemical&, const AttributeList&);	// From parser.
  SoilChemical (const Chemical&); // From influx.
  ~SoilChemical ();
private:
  SoilChemical (const SoilChemical&);
};

#endif // SOIL_CHEMICAL_H
