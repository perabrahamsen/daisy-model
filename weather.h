// weather.h --- Interface to weather data.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2011 KU.
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


#ifndef WEATHER_H
#define WEATHER_H

#include "weatherdata.h"

class Time;
class IM;

class Weather 
{
  // Location.
public:
  virtual double latitude () const = 0;
  virtual double longitude () const = 0; 
  virtual double elevation () const = 0; // [m]
  virtual double timezone () const = 0;
  virtual double screen_height () const = 0; // [m]
  virtual Weatherdata::surface_t surface () const = 0;

  // Communication with Bioclimate.
public:
  virtual double air_temperature () const = 0; // [dg C]
  virtual double daily_air_temperature () const = 0; // [dg C]
  virtual double daily_max_air_temperature () const = 0; // [dg C]
  virtual double daily_min_air_temperature () const = 0; // [dg C]
  virtual double net_radiation () const = 0; // [W/m^2]
  virtual double global_radiation () const = 0; // [W/m^2]
  virtual double daily_global_radiation () const = 0; // [W/m^2]
  virtual double diffuse_radiation () const = 0; // [W/m^2]
  virtual double ground_heat_flux () const = 0; // [W/m^2]
  virtual double reference_evapotranspiration () const = 0; // [mm/h]
  virtual double daily_precipitation () const = 0; // [mm/d]
  virtual double rain () const = 0;	// [mm/h]
  virtual double snow () const = 0;	// [mm/h]
  virtual const IM& deposit () const = 0; // [g [stuff] /cm^2/h]
  virtual double cloudiness_index () const = 0; // [0-1], 1 = clear sky
  virtual double vapor_pressure () const = 0; // [Pa]
  virtual double daily_vapor_pressure () const = 0; // [Pa]
  virtual double wind () const = 0;	// [m/s]
  virtual double daily_wind () const = 0;	// [m/s]
  virtual double CO2 () const = 0; //[Pa]
  virtual double O2 () const = 0; //[Pa]
  virtual double air_pressure () const = 0; //[Pa]
  virtual double daily_air_pressure () const = 0; //[Pa]

  // Initializing bioclimate.
public:
  virtual bool has_cloudiness () const = 0;
  virtual bool has_net_radiation () const = 0;
  virtual bool has_reference_evapotranspiration () const = 0;
  virtual bool has_vapor_pressure () const = 0;
  virtual bool has_daily_vapor_pressure () const = 0;
  virtual bool has_wind () const = 0;
  virtual bool has_CO2 () const = 0;
  virtual bool has_min_max_temperature () const = 0;
  virtual bool has_diffuse_radiation () const = 0;
  virtual bool has_ground_heat_flux () const = 0;
  virtual double timestep () const = 0; // [h]

  // Light distribution.
public:
  virtual const Time& middle () const = 0; // Middle of current timestep.
  virtual double sunrise () const = 0; // [h]
  virtual double day_length () const = 0; // [h]

  // Communication with SoilHeat.
public:
  virtual double T_normal (const Time&, double delay = 0.0) const = 0;

  // OrganicMatter initialization.
public:
  virtual double average_temperature () const = 0;

  // Astronomic utilities.
public:
  
  virtual double extraterrestrial_radiation () const = 0; // [W/m2]
  virtual double sin_solar_elevation_angle () const = 0; // []

  // Create and Destroy.
protected:
  Weather ();
public:
  virtual ~Weather ();
};

#endif // WEATHER_H
