// soil_heat1d.h
// 
// Copyright 1996-2001, 2006 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2006 KVL.
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


#ifndef SOIL_HEAT1D_H
#define SOIL_HEAT1D_H

#include "soil_heat.h"
#include <string>

class Geometry1D;
class Surface;
class Bioclimate;
class SoilWater;
class SoilWater1D;
class Weather;
class Time;

class SoilHeat1D : public SoilHeat
{
  // Parameters
private:
  const double h_frozen;
  const bool enable_ice;

  // State
private:
  std::vector<double> T_old;
  double T_top;
  double T_top_old;
  double T_bottom;
  std::vector<double> T_freezing;
  std::vector<double> T_thawing;
  std::vector<double> freezing_rate;
  enum state_t { liquid, freezing, frozen, thawing };
  std::vector<state_t> state;
  std::vector<double> q;

  /* const */ double delay;	// Period delay [ cm/rad ??? ]

  // Solve.
private:
  double capacity (const Soil&, const SoilWater&, size_t i) const;
  double capacity_apparent (const Soil&, const SoilWater1D&, size_t i) const;
  void update_freezing_points (const Soil& soil,
                               const SoilWater1D& soil_water);
  bool update_state (const Geometry1D& geo,
                     const Soil& soil, const SoilWater1D& soil_water, 
                     std::vector<double>& T);
  double calculate_freezing_rate (const Geometry1D& geo,
                                  const Soil& soil,
                                  const SoilWater1D& soil_water,
                                  unsigned int i, 
                                  const std::vector<double>& T);
  bool check_state (const Soil& soil, 
                    const std::vector<double>& T) const;
  void force_state (std::vector<double>& T);
  void solve (const Time&, const Geometry1D& geo,
              const Soil&, const SoilWater1D&, 
              const Surface&, const Weather&, 
              std::vector<double>& T);
  void calculate_heat_flux (const Geometry& geo,
                            const Soil&, const SoilWater1D&, 
                            const std::vector<double>& T);
  double bottom (const Time&, const Weather& weather) const;

  // Simulation.
public:
  double top_flux (const Geometry& geo,
                   const Soil&, const SoilWater&) const; // [W/m^2]
  double T_surface_snow (const Geometry&, const Soil&, const SoilWater&,
                         double T_snow, double K_snow, double dZs) const;
  void tick (const Time&, const Geometry1D& geo,
             const Soil&, SoilWater1D&, 
	     const Surface&, const Weather& weather);
  void output (Log&) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilHeat1D (const Block&);
  void initialize (const AttributeList& al, 
		   const Geometry1D& geo,
                   const Soil& soil, const Time& time, const Weather& weather,
		   Treelog&);
  ~SoilHeat1D ();
};

#endif // SOIL_HEAT1D_H
