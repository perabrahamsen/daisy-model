// surface_simple.h -- Simple surface models.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2022 UCPH
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

#ifndef SURFACE_SIMPLE_H
#define SURFACE_SIMPLE_H

#include "surface.h"

struct SurfaceSimple : public Surface
{
  // Content.
  double T; 			// [dg C]
  
  // Communication with soil water.
  void accept_top (double amount, const Geometry&, size_t edge, 
                   double dt, Treelog&)
  { }

  // Column.
  double runoff_rate () const // [h^-1]
  { return 0.0; }
  double mixing_resistance () const // [h/mm]
  { return 1.0e9; }
  double mixing_depth () const // [cm]
  { return 0.1; }
  
  // Manager.
  void set_detention_capacity (double)
  { }

  // Simulation.
  void output (Log&) const
  { }
  void update_pond_average (const Geometry& )
  { }
  void tick (const Time&, double dt /* [h] */,
             double PotSoilEvaporationWet, 
             double PotSoilEvaporationDry, 
             double flux_in /* [mm/h] */,
             double temp /* [dg C] */, const Geometry& geo,
             const Soil&, const SoilWater&,
             double soil_T /* [dg C] */, Treelog&)
  { T = temp; }

  // Communication with bioclimate.
  double ponding_average () const // [mm]
  { return 0.0; }
  double ponding_max () const     // [mm]
  { return 0.0; }
  double temperature () const     // [dg C]
  { return T; }
  double EpFactor () const        // []
  { return 0.6; }
  double albedo (const Geometry&, const Soil&, const SoilWater&) const
  { return 0.08; }
  double exfiltration (double dt) const // [mm/h]
  { return 0.0; }
  double evap_soil_surface () const // [mm/h]
  { return 0.0; }
  double evap_pond (double dt, Treelog&) const // [mm/h]
  { return 0.0; }
  void put_ponding (double pond)	// [mm]
  { }
  void set_svat_temperature (double temp /* dg C */)
  { T = temp; }
  
  // Create.
  void initialize (const Geometry&)
  { }
  SurfaceSimple (const BlockModel& al)
    : Surface (al)
  { }
  ~SurfaceSimple ()
  { }
};

#endif // SURFACE_SIMPLE_H

// surface_simple.h ends here.
