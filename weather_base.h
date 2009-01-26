// weather_base.h
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


#ifndef WEATHER_BASE_H
#define WEATHER_BASE_H

#include "weather.h"

class WeatherBase : public Weather
{
  // Units.
protected:
  const Units& units;

  // Location.
protected:
  /* const */ double latitude_;
  /* const */ double longitude_;
  /* const */ double elevation_;
  /* const */ double timezone_;
  /* const */ surface_t surface_;
  /* const */ double screen_height_;
  double latitude () const;
  double longitude () const; 
  double elevation () const; // [m]
  double timezone () const;
  double screen_height () const; // [m]
  surface_t surface () const;
  
  // Deposit.
protected:
  /* const */ IM DryDeposit;
  /* const */ IM WetDeposit;

  // Temperature.
protected:
  /* const */ double T_average;
  /* const */ double T_amplitude;
  /* const */ double max_Ta_yday;

  // State
private:
  double day_length_;
  double day_cycle_;
  double cloudiness_;
  double daily_cloudiness_;
  IM deposit_;
  double cloudiness () const // [0-1]
  { return cloudiness_; }
  double daily_cloudiness () const // [0-1]
  { return daily_cloudiness_; }
  IM deposit () const; // [g [stuff] /cm²/h]

  // Simulation.
public:
  void tick (const Time& time, Treelog&);
  void tick_after (const Time& time, Treelog&);
  void output (Log&) const;

  // Communication with Bioclimate.
public:
  double CO2 () const; //[Pa]

  // Initializing bioclimate.
public:
  bool has_reference_evapotranspiration () const;
  bool has_vapor_pressure () const;
  bool has_wind () const;
  bool has_min_max_temperature () const;
  bool has_diffuse_radiation () const;
  bool has_relative_humidity () const;

  // Light distribution.
public:
  double day_length () const	// [h]
  { return day_length_; }
  double day_cycle () const	// Sum over a day is 1.0.
  { return day_cycle_; }
public:
  double day_cycle (const Time&) const;	// Sum over a day is 1.0.
private:
  double day_length (const Time&) const;

  // Communication with SoilHeat.
public:
  double T_normal (const Time&, double delay = 0.0) const;

  // OrganicMatter initialization.
public:
  double average_temperature () const;

  // Astronomic utilities.
public:
  static double SolarDeclination (const Time& time); // [rad]
  static double RelativeSunEarthDistance (const Time& time);
  static double SunsetHourAngle (double Dec, double Lat); // [rad]
  double ExtraterrestrialRadiation (const Time& time) const; // [W/m2]
  double HourlyExtraterrestrialRadiation (const Time& time) const; // [W/m2]
  double sin_solar_elevation_angle (const Time& time) const; // []

  // Create and Destroy.
public:
  bool initialize (const Time& time, Treelog& err);
  bool check (const Time& from, const Time& to, Treelog& err) const;
protected:
  WeatherBase (Block&);
};

#endif // WEATHER_BASE_H
