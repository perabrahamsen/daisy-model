// movement_solute.h --- Geometry independent solute movement.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#ifndef MOVEMENT_SOLUTE_H
#define MOVEMENT_SOLUTE_H

#include "movement.h"
#include "memutils.h"

class Msoltranrect;

struct MovementSolute : public Movement
{
  // Solute.
  const auto_vector<Msoltranrect*> matrix_solute;
  const std::auto_ptr<Msoltranrect> matrix_solid;
  static void secondary_flow (const Geometry& geo, 
                              const std::vector<double>& Theta_old,
                              const std::vector<double>& Theta_new,
                              const std::vector<double>& q,
                              const symbol name,
                              std::vector<double>& M, 
                              const std::vector<double>& S, 
                              std::vector<double>& J_sum, 
                              const double C_below,
                              const double dt,
                              Treelog& msg);
  static void secondary_transport (const Geometry&,
                                   const Soil&, const SoilWater&,
                                   const double J_above, Chemical& solute, 
                                   std::vector<double>& S_extra,
                                   const bool flux_below, const double dt,
                                   const Scope& scope, Treelog& msg);
  static void primary_transport (const Geometry& geo,
                                 const Soil& soil, const SoilWater& soil_water,
                                 const Msoltranrect&,
                                 const double J_above, Chemical& solute, 
                                 const std::vector<double>& S_extra,
                                 const bool flux_below, const double dt,
                                 const Scope& scope, Treelog& msg);
  void solute (const Soil& soil, const SoilWater& soil_water,
               double J_above, Chemical&, const bool flux_below, 
	       double dt, const Scope&, Treelog&);
  void element (const Soil& soil, const SoilWater& soil_water,
                DOE& element, 
		double diffusion_coefficient, double dt, Treelog& msg);

  // Create.
  bool check_solute (Treelog&) const;
  MovementSolute (Block& al);
  static void load_solute (Syntax&, AttributeList&);
};

#endif // MOVEMENT_SOLUTE_H
