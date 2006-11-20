// weather_old.h --- Common code for old weather models.
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


#ifndef WEATHER_OLD_H
#define WEATHER_OLD_H

#include "weather.h"

class WeatherOld : public Weather
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
  
  // Simulation.
public:
  void tick (const Time& time, Treelog&);
  void output (Log&) const;
protected:
  void distribute (double precipitation);

  // Communication with Bioclimate.
public:
  double hourly_air_temperature () const; // [dg C]
  double hourly_global_radiation () const; // [W/m2]
  double daily_global_radiation () const; // [W/m2]
  double hourly_diffuse_radiation () const; // [W/m2]
  double reference_evapotranspiration () const; // [mm/h]
  double rain () const;	// [mm/h]
  double snow () const;	// [mm/h]
  double vapor_pressure () const; // [Pa]
  double relative_humidity () const; // []
  double wind () const;	// [m/s]

  // Communication with external model.
  void put_precipitation (double prec);// [mm/d]
  void put_air_temperature (double T); // [°C]
  void put_reference_evapotranspiration (double ref); // [mm/d]
  void put_global_radiation (double radiation); // [W/m²]

  // Create and Destroy.
protected:
  WeatherOld (Block&);
public:
  static void load_syntax (Syntax&, AttributeList&);
  ~WeatherOld ();
};

#endif // WEATHER_OLD_H
