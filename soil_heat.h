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
#include <boost/noncopyable.hpp>

class Time;
class Weather;
class Block;
class Log;
class Frame;
class FrameSubmodel;
class Geometry;
class Soil;
class SoilWater;
class Movement;
class Treelog;

static const double latent_heat_of_fussion = 3.35e9; // [erg/g]
static const double water_heat_capacity = 4.2e7; // [erg/cm^3/dg C]

class SoilHeat : private boost::noncopyable
{
  // Parameters
private:
  const double q_lim;           // [erg/cm^2/h]
  const double h_frozen;
  const bool enable_ice;
  const double ice_dt;            // [h]
  const double T_thawing_epsilon; // [dg C]
  // State
private:
  double T_limited;
  std::vector<double> T_freezing;
  std::vector<double> T_thawing;
  std::vector<double> freezing_rate_;
  enum state_t { liquid, freezing, frozen, thawing };
  std::vector<state_t> state;
  std::vector<double> q;
  std::vector<double> T_;
  std::vector<double> capacity_;
  std::vector<double> capacity_apparent_;
  std::vector<double> conductivity_;
public:
  double T_top () const
  { return T_limited; }
  double freezing_rate (const std::size_t c) const;
  double T (std::size_t c) const	// [dg C]
  { return T_[c]; }
  double conductivity (std::size_t c) const
  { return conductivity_[c]; }
  double top_flux (const Geometry& geo,
                   const Soil&, const SoilWater&) const;
  double top_flux (const Geometry& geo) const;
private:
  double limit_T_top (const Geometry& geo,
                      const Soil& soil, const SoilWater& soil_water,
                      const double T_surface) const;
public:
  double energy (const Geometry& geo, const Soil& soil,
                 const SoilWater& soil_water,
                 const double from, const double to) const;
  void set_energy (const Geometry& geo, 
                   const Soil& soil, const SoilWater& soil_water, 
                   double from, double to, double energy);
  void swap (const Geometry& geo, double from, double middle, double to);
  double source (const Geometry&, const SoilWater&, std::size_t c) const;
private:
  static double T_pseudo (const Geometry& geo,
			  const int cell,
			  const int other,
			  const std::vector<double>& T_old,
			  const double T_top,
			  const std::vector<double>& T,
			  const double T_bottom);
  static double K_pseudo (const Geometry& geo, const int cell, const int other,
			  const Soil& soil, const SoilWater& soil_water);
  static void calculate_heat_flux (const Geometry& geo,
                                   const Soil& soil,
                                   const SoilWater& soil_water,
                                   const std::vector<double>& T_old,
                                   double T_prev,
                                   const std::vector<double>& T,
                                   double T_bottom,
                                   std::vector<double>& q);
public:
  double suggest_dt (const double T_air) const;
  double exptected_T_z0 (const Geometry&, const Soil&, const SoilWater&,
			 const double T_bottom, const Movement&, 
			 const double T_surface, double dt, Treelog& msg) const;
  void tick (const Geometry&, const Soil&, SoilWater&, const double T_bottom,
             const Movement&, const double T_surface, double dt, Treelog& msg);
  void tick_after (const std::size_t cell_size, 
                   const Soil&, const SoilWater&, Treelog&);
  
  // Transport.
private:
  void set_temperature (const std::size_t c, const double value);
  void set_flux (const std::size_t e, const double value);

  // Solve.
private:
  double capacity_new (const Soil&, const SoilWater&, std::size_t i) const;
  double capacity_old (const Soil&, const SoilWater&, std::size_t i) const;
  double capacity_apparent (const Soil&, const SoilWater&, std::size_t i) const;
  void update_freezing_points (const Soil& soil,
                               const SoilWater& soil_water);
  bool update_state (const Geometry& geo,
                     const Soil& soil, const SoilWater& soil_water, 
                     const std::vector<double>& T_old, std::vector<double>& T, 
                     double dt);
  double calculate_freezing_rate (const Geometry& geo,
                                  const Soil& soil,
                                  const SoilWater& soil_water,
                                  unsigned int i, 
                                  const std::vector<double>& T_old,
                                  const std::vector<double>& T,
                                  double dt);
  bool check_state (const Soil& soil, 
                    const std::vector<double>& T) const;
  void force_state (std::vector<double>& T);

  // Create and destroy.
public:
  void output (Log&) const;
  bool check (std::size_t n, Treelog&) const;
  static void load_syntax (Frame&);
  SoilHeat (const Block&);
  void initialize (const FrameSubmodel&, const Geometry& geo, 
                   const std::vector<double>& default_T, Treelog&);
  ~SoilHeat ();
private:                        // Disable.
  SoilHeat ();
};

#endif // SOIL_HEAT_H
