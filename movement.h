// movement.h
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


#ifndef MOVEMENT_H
#define MOVEMENT_H

#include "model_derived.h"
#include <vector>
#include <memory>

class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class DOE;
class Chemical;
class Adsorption;
class Surface;
class Groundwater;
class Weather;
class Time;
class Treelog;
class BlockModel;
class Log;
class Scope;
class Tertiary;
class Units;
class FrameSubmodel;

class Movement : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  virtual Geometry& geometry () const = 0;

  // Failures.
private:
  std::vector<size_t> water_fail;
  std::vector<size_t> water_total;
  std::vector<size_t> solute_fail;
  std::vector<size_t> solute_total;
  int water_failure_level;
  int solute_failure_level;
protected:
  void water_attempt (size_t level);
  void water_failure (size_t level);
  void solute_attempt (size_t level);
  void solute_failure (size_t level);
public:
  virtual void summarize (Treelog& msg) const;

  // Tertiary transport.
protected:
  std::unique_ptr<Tertiary> tertiary;
public:
  void tick_tertiary (const Units&,
                      const Geometry&, const Soil&, const SoilHeat&,
                      const double dt, SoilWater&, Surface&, Treelog&);

  // Simulation.
public:
  virtual void clear ();
  void tick_source (const Soil&, const SoilHeat&, SoilWater&, 
                    Treelog&);
  double suggest_dt (double weather_dt, double max_pond) const;
  virtual void tick (const Soil&, SoilWater&, const SoilHeat&, Surface&,
                     Groundwater&, const Time&, const Scope&, const Weather&, 
                     double dt, Treelog&) = 0;
  virtual void solute (const Soil&, const SoilWater&, 
                       const double J_above, Chemical&,
		       double dt, const Scope&, Treelog&) = 0;
  virtual void element (const Soil&, const SoilWater&, 
                        DOE&, double diffusion_coefficient, 
			double dt, Treelog&) = 0;
  virtual void heat (const std::vector<double>& q_water,
		     const std::vector<double>& S_water,
		     const std::vector<double>& S_heat,
		     const std::vector<double>& capacity_new,
		     const std::vector<double>& conductivity,
		     double T_top,
		     double T_top_new,
                     double T_bottom,
		     std::vector<double>& T,
		     const double dt, Treelog&) const = 0;
  void remove_solute (const symbol chem);
  double total_solute (const Geometry&, const symbol chem) const; //[g/m^2]

protected:
  void output_base (Log&) const;
public:
  void output (Log&) const = 0;

  // Heat.
  virtual double surface_snow_T (const Soil&, const SoilWater&, const SoilHeat&,
                                 double T_snow, double K_snow,
                                 double dZs) const = 0;
  virtual std::vector<double> default_heat (const Soil&, 
                                            const Time&, const Weather&) = 0;
public:
  virtual double bottom_heat (const Time&, const Weather&) const = 0;

  // Create and Destroy.
private:
  virtual bool check_derived (Treelog& err) const = 0;
  virtual void initialize_derived (const Time&, const Scope&,
				   const Soil&, const Groundwater&, 
                                   bool has_macropores, Treelog& msg) = 0;
public:
  bool check (Treelog& err) const;
  bool initialize (const Units&,
                   const Soil&, SoilWater&, 
                   const Groundwater&, const Time&, const Scope& scope,
                   Treelog& msg);
  static Movement* build_vertical (const BlockModel& al);
protected:
  Movement (const BlockModel&);
public:
  ~Movement ();
};

#endif // MOVEMENT_H
