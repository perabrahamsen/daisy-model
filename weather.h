// weather.h
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


#ifndef WEATHER_H
#define WEATHER_H

#include "librarian.h"
#include "im.h"

class Time;
class Treelog;

class Weather
{
  // Content.
public: 
  const string name;
  static const char *const description;
  
  // Location.
protected:
  /* const */ double latitude;
  /* const */ double longitude;
  /* const */ double elevation;
  /* const */ double timezone;
  struct Surface
  { enum type { reference, field }; };
  /* const */ Surface::type surface;
  /* const */ double screen_height_;
public:
  double screen_height () const; // [m]

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
  double hourly_cloudiness_;
  double daily_cloudiness_;
  IM deposit_;

  // Simulation.
public:
  virtual void tick (const Time& time, Treelog&);
  void tick_after (const Time& time, Treelog&);
  virtual void output (Log&) const;

  // Communication with Bioclimate.
public:
  virtual double hourly_air_temperature () const = 0; // [dg C]
  virtual double daily_air_temperature () const = 0; // [dg C]
  virtual double hourly_global_radiation () const = 0; // [W/m2]
  virtual double daily_global_radiation () const = 0; // [W/m2]
  virtual double reference_evapotranspiration () const = 0; // [mm/h]
  virtual double rain () const = 0;	// [mm/h]
  virtual double snow () const = 0;	// [mm/h]
  IM deposit () const; // [g [stuff] /cm²/h]
  double hourly_cloudiness () const // [0-1]
    { return hourly_cloudiness_; }
  double daily_cloudiness () const // [0-1]
    { return daily_cloudiness_; }
  virtual double vapor_pressure () const = 0; // [Pa]
  virtual double wind () const = 0;	// [m/s]

  // Light distribution.
public:
  double day_length () const	// [h]
    { return day_length_; }
  double day_cycle () const	// Sum over a day is 1.0.
    { return day_cycle_; }

protected:
  double day_cycle (const Time&) const;	// Sum over a day is 1.0.
private:
  double day_length (const Time&) const;

  // Communication with SoilHeat.
public:
  double T_normal (const Time&, double delay = 0.0) const;

  // Communication with external model.
public:
  virtual void put_precipitation (double prec) = 0;// [mm/d]
  virtual void put_air_temperature (double T) = 0; // [°C]
  virtual void put_reference_evapotranspiration (double ref) = 0; // [mm/d]
  virtual void put_global_radiation (double radiation) = 0; // [W/m^2]

  // FAO atmospheric utilities.
public:
  static double LatentHeatVaporization (double Temp /* [dg C] */); // [J/kg]
  static double PsychrometricConstant (double AtmPressure /* [Pa] */,
				       double Temp /* [dg C] */); // [Pa/K]
  static double AirDensity (double AtmPressure /* [Pa] */,
			    double Temp /* [dg C] */); // [kg/m3]
  static double SaturationVapourPressure (double Temp /* [dg C] */); // [Pa]
  static double SlopeVapourPressureCurve (double Temp /* [dg C] */); // [Pa/K]
  double AtmosphericPressure () const; // [Pa]
  double CloudinessFactor_Arid (const Time&, double Si /* [W/m2] */) const;
  double CloudinessFactor_Humid (const Time&, double Si /* [W/m2] */) const;
  double RefNetRadiation (const Time& time, double Si /* [W/m2] */,
			  double Temp /* [dg C] */,
			  double ea /* [Pa] */,
			  Treelog&) const;// [W/m2]
  static double Makkink (double air_temperature /* [dg C] */,
 		         double global_radiation /* [W/m^2] */); /* [mm/h] */

  // Astronomic utilities.
public:
  static double SolarDeclination (const Time& time); // [rad]
  static double RelativeSunEarthDistance (const Time& time);
  static double SunsetHourAngle (double Dec, double Lat); // [rad]
  double ExtraterrestrialRadiation (const Time& time) const; // [W/m2]
  double HourlyExtraterrestrialRadiation (const Time& time) const; // [W/m2]

  // Create and Destroy.
private:
  Weather (const Weather&);
public:
  virtual void initialize (const Time& time, Treelog& err);
protected:
  Weather (const AttributeList&);
public:
  virtual bool check (const Time& from, const Time& to, Treelog& err) const;
  static void load_syntax (Syntax&, AttributeList&);
  virtual ~Weather ();
};

static Librarian<Weather> Weather_init ("weather");

#endif // WEATHER_H
