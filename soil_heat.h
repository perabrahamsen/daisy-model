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

class Time;
class Weather;
class AttributeList;
class Block;
class Log;
class Syntax;
class Geometry;
class Soil;
class SoilWater;
class Treelog;

static const double latent_heat_of_fussion = 3.35e9; // [erg/g]
static const double water_heat_capacity = 4.2e7; // [erg/cm^3/dg C]

class SoilHeat
{
  friend class Movement1D;
  friend class MovementRect;

  // Parameters
private:
  const double h_frozen;
  const bool enable_ice;

  // State
private:
  std::vector<double> T_old;
  double T_top;
  std::vector<double> T_freezing;
  std::vector<double> T_thawing;
  std::vector<double> freezing_rate;
  enum state_t { liquid, freezing, frozen, thawing };
  std::vector<state_t> state;
  std::vector<double> q;
  std::vector<double> T_;
  std::vector<double> S;
  std::vector<double> capacity_;
  std::vector<double> conductivity_;
public:
  double T (size_t i) const // [dg C]
  { return T_[i]; }
  double top_flux (const Geometry& geo,
                   const Soil&, const SoilWater&) const;
  double energy (const Geometry& geo, const Soil& soil,
                 const SoilWater& soil_water,
                 const double from, const double to) const;
  void set_energy (const Geometry& geo, 
                   const Soil& soil, const SoilWater& soil_water, 
                   double from, double to, double energy);
  void swap (const Geometry& geo, double from, double middle, double to);
  double source (size_t i) const
  { return S[i]; }
  void set_source (const size_t i, const double value) // [erg/cm^3/h]
  { S[i] = value; }
  void tick_after (const size_t cell_size, 
                   const Soil&, const SoilWater&, Treelog&);

  // Solve.
private:
  double capacity (const Soil&, const SoilWater&, size_t i) const;
  double capacity_apparent (const Soil&, const SoilWater&, size_t i) const;
  void update_freezing_points (const Soil& soil,
                               const SoilWater& soil_water);
  bool update_state (const Geometry& geo,
                     const Soil& soil, const SoilWater& soil_water, 
                     std::vector<double>& T);
  double calculate_freezing_rate (const Geometry& geo,
                                  const Soil& soil,
                                  const SoilWater& soil_water,
                                  unsigned int i, 
                                  const std::vector<double>& T);
  bool check_state (const Soil& soil, 
                    const std::vector<double>& T) const;
  void force_state (std::vector<double>& T);

  // Create and destroy.
public:
  void output (Log&) const;
  bool check (size_t n, Treelog&) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilHeat (const Block&);
  void initialize (const AttributeList& al, const Geometry& geo, 
                   const std::vector<double>& default_T, Treelog&);
  ~SoilHeat ();
private:                        // Disable.
  SoilHeat ();
  SoilHeat (const SoilHeat&);
};

#endif // SOIL_HEAT_H
