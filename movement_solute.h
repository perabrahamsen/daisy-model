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
#include <map>

class Transport;

class MovementSolute : public Movement
{
  // Solute.
private:
  const auto_vector<Transport*> matrix_solute;
  const std::auto_ptr<Transport> matrix_solid;
  static void secondary_flow (const Geometry& geo, 
                              const std::vector<double>& Theta_old,
                              const std::vector<double>& Theta_new,
                              const std::vector<double>& q,
                              const symbol name,
                              const std::vector<double>& S, 
                              const std::map<size_t, double>& J_forced,
                              const std::map<size_t, double>& C_border,
                              std::vector<double>& M, 
                              std::vector<double>& J, 
                              const double dt,
                              Treelog& msg);
  static void secondary_transport (const Geometry&,
                                   const Soil&, const SoilWater&,
                                   const std::map<size_t, double>& J_forced,
                                   const std::map<size_t, double>& C_border,
                                   Chemical& solute, 
                                   std::vector<double>& S_extra,
                                   const double dt, 
                                   const Scope& scope, Treelog& msg);
  static void primary_transport (const Geometry& geo,
                                 const Soil& soil, const SoilWater& soil_water,
                                 const Transport&,
                                 const std::map<size_t, double>& J_forced,
                                 const std::map<size_t, double>& C_border,
                                 Chemical& solute, 
                                 const std::vector<double>& S_extra,
                                 const double dt,
                                 const Scope& scope, Treelog& msg);
  static void divide_top_incomming (const Geometry& geo, 
                                    const SoilWater& soil_water, 
                                    const double J_above,
                                    std::map<size_t, double>& J_primary,
                                      std::map<size_t, double>& J_secondary,
                                    std::map<size_t, double>& J_tertiary);
  static void divide_top_outgoing (const Geometry& geo, 
                                   const Chemical& chemical,
                                   const double J_above,
                                   std::map<size_t, double>& J_primary,
                                   std::map<size_t, double>& J_secondary,
                                   std::map<size_t, double>& J_tertiary);
  static void zero_top (const Geometry& geo, 
                        std::map<size_t, double>& J_primary,
                        std::map<size_t, double>& J_secondary,
                        std::map<size_t, double>& J_tertiary);
  void solute (const Soil& soil, const SoilWater& soil_water,
               double J_above, Chemical&, const bool flux_below, 
	       double dt, const Scope&, Treelog&);
  void element (const Soil& soil, const SoilWater& soil_water,
                DOE& element, 
		double diffusion_coefficient, double dt, Treelog& msg);

  // Create.
protected:
  bool check_solute (Treelog&) const;
  MovementSolute (Block& al);
public:
  static void load_solute  (Syntax& syntax, AttributeList& alist, 
                            const AttributeList& prefered_solute);
};

#endif // MOVEMENT_SOLUTE_H
