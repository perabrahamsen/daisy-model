// weather.h

#ifndef WEATHER_H
#define WEATHER_H

#include "librarian.h"
#include "im.h"

class Time;

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

  // Temperature.
protected:
  /* const */ double T_average_;
  /* const */ double T_amplitude_;
  /* const */ double rad_per_day_;
  /* const */ double max_Ta_yday_;

  // State
private:
  double day_length_;
  double day_cycle_;


  // Simulation.
public:
  virtual void tick (const Time& time) = 0;
  virtual void output (Log&) const;

  // Communication with Bioclimate.
public:
  virtual double hourly_air_temperature () const = 0; // [dg C]
  virtual double daily_air_temperature () const = 0; // [dg C]
  virtual double hourly_global_radiation () const = 0; // [W/m2]
  virtual double daily_global_radiation () const = 0; // [W/m2]
  virtual double daily_extraterrastial_radiation () const = 0; // [MJ/m2/d]
  virtual double reference_evapotranspiration () const = 0; // [mm/h]
  virtual double rain () const = 0;	// [mm/h]
  virtual double snow () const = 0;	// [mm/h]
  virtual IM deposit () const = 0; // [g [stuff] /cm²/h]
  virtual double cloudiness () const = 0; // [0-1]
  virtual double vapor_pressure () const = 0; // [Pa]
  virtual double wind () const = 0;	// [m/s]

  // Light distribution.
public:
  double day_length () const	// [h]
    { return day_length_; }
  double day_cycle () const	// Sum over a day is 1.0.
    { return day_cycle_; }
private:
  double day_length (const Time& t) const;

  // Communication with external model.
public:
  virtual void put_precipitation (double prec) = 0;// [mm/d]
  virtual void put_air_temperature (double T) = 0; // [°C]
  virtual void put_reference_evapotranspiration (double ref) = 0; // [mm/d]
  virtual void put_global_radiation (double radiation) = 0; // [W/m²]

  // Average temperature.
public:
  double T_average () const
    { return T_average_; }
  double T_amplitude () const
    { return T_amplitude_; }
  double rad_per_day () const
    { return rad_per_day_; }
  double max_Ta_yday () const
    { return max_Ta_yday_; }

  // FAO atmospheric utilities.
public:
  static double LatentHeatVaporization (double Temp /* [dg C] */); // [MJ/kg]
  static double PsychrometricConstant (double AtmPressure /* [kPa] */, 
				       double Temp /* [dg C] */); // [kPa/K]
  static double AirDensity (double AtmPressure /* [kPa] */,
			    double Temp /* [dg C] */); // [kg/m3]
  static double SaturationVapourPressure (double Temp /* [dg C] */); // [kPa]
  static double SlopeVapourPressureCurve (double Temp /* [dg C] */); // [kPa/K]
  double AtmosphericPressure () const; // [kPa]
  double CloudinessFactor_Arid (const Time&, double Si /* [MJ/m2/d] */) const;
  double CloudinessFactor_Humid (const Time&, double Si /* [MJ/m2/d] */) const;
  double RefNetRadiation (const Time& time, double Si /* [MJ/m2/d] */, 
			  double Temp /* [dg C] */, 
			  double ea /* [kPa] */) const;// [MJ/m2/d]

  // Astronomic utilities.
public:
  static double SolarDeclination (const Time& time); // [rad]
  static double RelativeSunEarthDistance (const Time& time);
  static double SunsetHourAngle (double Dec, double Lat); // [rad]
  double ExtraterrestrialRadiation (const Time& time) const; // [MJ/m2/d]

  // Create and Destroy.
private:
  Weather (const Weather&);
protected:
  static void load_syntax (Syntax&, AttributeList&);
  Weather (const AttributeList&);
public:
  virtual ~Weather ();
};

static Librarian<Weather> Weather_init ("weather");

#endif WEATHER_H
