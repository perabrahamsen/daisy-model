// soil_chemicals.h
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


#ifndef SOIL_CHEMICALS_H
#define SOIL_CHEMICALS_H

#ifndef COMPOUNDS
#error "don't use this file"
#endif

// These must be included in the header file, for 'load_syntax' to work.
#include "soil_chemical.h"
#include <map>

struct Soil;
struct SoilWater;
struct SoilHeat;
struct OrganicMatter;
struct Chemicals;
struct Log;
struct Syntax;
struct AttributeList;

class SoilChemicals
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
public:
  typedef std::map<symbol, SoilChemical*> SoluteMap;

  // Simulation.
public:
  const SoluteMap& all () const;
  SoilChemical& find (const Geometry& geo,
                      const Soil& soil, 
		      const SoilWater& soil_water,
		      symbol name, Treelog&);
  void tick (const Geometry& geo,
             const Soil&, const SoilWater&, const SoilHeat&, 
	     const OrganicMatter*, const Chemicals& flux_in, 
             double dt, Treelog&);
  void mixture (const Geometry& geo,
                Chemicals& storage, // [g/m^2]
		Chemicals& up,	// [g/m^2/h]
		double pond,	// [mm]
		double rate, 	// [h/mm]
                double dt) const;
  void output (Log&) const;
  void mix (const Geometry& geo,
            const Soil&, const SoilWater&, double from, double to, double dt);
  void swap (const Geometry& geo,
             const Soil&, const SoilWater&,
	     double from, double middle, double to, double dt);

  // Create & Destroy.
public:
  void clear ();
  void initialize (const AttributeList&, const Geometry& geo,
                   const Soil&, const SoilWater&, 
		   Treelog&);
  bool check (unsigned n, Treelog&) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilChemicals (Block&);
  ~SoilChemicals ();
};

#endif // SOIL_CHEMICALS_H
