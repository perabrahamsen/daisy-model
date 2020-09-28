// litter_residue.h -- Dynamic litter layer affecting water and heat.
// 
// Copyright 2010 Per Abrahamsen and KVL.
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


#ifndef LITTER_RESIDUE_H
#define LITTER_RESIDUE_H

#include "litter.h"

struct LitterResidue : public Litter
{
  // Parameters.
  const double water_capacity_;	      // Max water in litter DM [L/kg]
  const double vapor_flux_factor_;    // Ep-reduction []
  const double specific_AI;	      // Spec. litter area [m^2/kg DM]
  const double extinction_coefficent; // Beers law for cover []
  const double albedo_;
  
  // Log variables.
  double mass;                    // Surface residuals [kg DM/m^2]
  double cover_;		  // Surface residuals [m^2 mulch/m^2 soil]

  // Simulation.
  void output (Log& log) const;
  void tick (const Bioclimate& bioclimate,
	     const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat,
	     OrganicMatter& organic, Chemistry& chemistry,
	     const double dt,
	     Treelog& msg);
  double cover () const
  { return cover_; }
  double vapor_flux_factor () const
  { return vapor_flux_factor_; }
  double water_capacity () const
  { return mass * water_capacity_; }
  double albedo () const
  { return albedo_; }

  // Create and Destroy.
  LitterResidue (const BlockModel& al);
  ~LitterResidue ();
};

#endif // LITTER_RESIDUE_H
