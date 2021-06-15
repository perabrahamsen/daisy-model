// bioclimate.h
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


#ifndef BIOCLIMATE_H
#define BIOCLIMATE_H

#include "model_framed.h"
#include <vector>

class Surface;
class Weather;
class Vegetation;
class Litter;
class Movement; 
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class Log;
class Time;
class Treelog;
class Block;
class BlockModel;
class IM;

class Bioclimate : public ModelFramed
{ 
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void tick (const Time&, Surface&, const Weather&, Vegetation&, 
                     const Litter& litter, const Movement&, const Geometry&,
		     const Soil&, SoilWater&, const SoilHeat&, 
		     const double T_bottom, 
		     double dt, Treelog&) = 0;
  virtual void clear () = 0;
  virtual double get_intercepted_water () const = 0; // [mm]
  virtual double get_litter_water () const = 0; // [mm]
  virtual double get_litter_temperature () const = 0; // [dg C]
  virtual double get_snow_storage () const = 0; // [mm]
  virtual double snow_leak_rate (double dt) const = 0; // [h^-1]
  virtual double canopy_leak_rate (double dt) const = 0; // [h^-1]
  virtual double canopy_leak () const = 0;               // [mm/h]
  virtual double litter_leak_rate (double dt) const = 0; // [h^-1]
  virtual double litter_wash_off_rate (double dt) const = 0; // [h^-1]
  virtual const IM& deposit () const = 0; // [g [stuff] /cm²/h]

  // Canopy.
public:
  virtual const std::vector<double>& height () const = 0;
  virtual const std::vector<double>& PAR () const = 0;
  virtual const std::vector<double>& sun_PAR () const = 0;
  virtual const std::vector<double>& sun_LAI_fraction () const = 0;
  virtual double cover () const = 0; 
  virtual double litter_cover () const = 0; 
  virtual double LAI () const = 0;
  virtual double sun_LAI_fraction_total () const =0;
  virtual double wind_speed_field () const = 0;
  virtual double rad_abs_soil() const = 0;
  virtual double rad_abs_sun_canopy() const = 0;
  virtual double rad_abs_shadow_canopy() const = 0;
  virtual double sin_beta() const = 0;
  virtual double shared_light_fraction () const = 0;

  // Weather.
  virtual double daily_air_temperature () const = 0;
  virtual double canopy_temperature () const =0;
  virtual double canopy_vapour_pressure () const = 0; // [Pa]
  virtual double sun_leaf_temperature () const =0;
  virtual double shadow_leaf_temperature () const =0;
  virtual double sun_boundary_layer_water_conductivity () const = 0; // [m/s]
  virtual double shadow_boundary_layer_water_conductivity () const = 0; // [m/s]
  virtual double daily_precipitation () const = 0;
  virtual double day_length () const = 0;
  virtual double daily_global_radiation () const = 0;
  virtual double global_radiation () const = 0;
  double day_fraction (double dt) const;   
  virtual double direct_rain () const = 0;
  virtual double atmospheric_CO2 () const = 0;
  virtual double atmospheric_O2 () const = 0;
  virtual double air_pressure () const = 0;

  // Manager.
public:
  virtual void irrigate_overhead (double flux, double temp) = 0;
  virtual void irrigate_surface (double flux, double temp) = 0;
  virtual void irrigate_overhead (double flux) = 0;
  virtual void irrigate_surface (double flux) = 0;
  virtual void irrigate_subsoil (double flux) = 0;
  virtual void set_subsoil_irrigation (double flux) = 0;
  virtual void add_tillage_water (double amount) = 0;

  // Communication with SVAT.
  virtual double total_ep () const = 0; // [mm/h]
  virtual double total_ea () const = 0; // [mm/h]
  virtual double snow_ea () const = 0; // [mm/h]
  virtual double pond_ea () const = 0; // [mm/h]
  virtual double soil_ea () const = 0; // [mm/h]
  virtual double soil_surface_ea () const = 0; // soil + litter + pond + snow [mm/h]
  virtual double crop_ep () const = 0; // [mm/h]
  virtual double crop_ea () const = 0; // [mm/h]
  virtual double canopy_ea () const = 0; // [mm/h]
  virtual double min_sin_beta () const = 0; // [] Lowest sun angle.

  // Utilities.
public:
  static void radiation_distribution (const size_t No, const double LAI,
                                      const double Ref,
                                      const double Si,
                                      const double Ext,
                                      std::vector <double>& Rad);
private:
  static void intensity_distribution (size_t No, double LAI,
                                      double Rad0, double Ext, 
                                      std::vector <double>& Rad);

  // Create.
public:
  virtual void initialize (const Weather&, Treelog& msg) = 0;
  virtual bool check (const Weather& weather, Treelog& msg) const = 0;
protected:
  explicit Bioclimate (const BlockModel&);
public:
  virtual void summarize (Treelog& msg) const = 0;
  ~Bioclimate ();
};

#endif // BIOCLIMATE_H
