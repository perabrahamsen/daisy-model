// weather.h

#ifndef WEATHER_H
#define WEATHER_H

#include "librarian.h"
#include "im.h"

class Time;

class Weather
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
public: 
  const string name;
  static const char *const description;
  
  // Simulation.
public:
  virtual void tick (const Time& time) = 0;
  virtual void output (Log&) const;
protected:
  void distribute (double precipitation);

  // Communication with Bioclimate.
public:
  virtual double hourly_air_temperature () const; // [dg C]
  virtual double daily_air_temperature () const = 0; // [dg C]
  virtual double hourly_global_radiation () const; // [W/m2]
  virtual double daily_global_radiation () const; // [W/m2]
  double daily_extraterrastial_radiation () const; // [MJ/m2/d]
  virtual double reference_evapotranspiration () const; // [mm/h]
  virtual double rain () const;	// [mm/h]
  virtual double snow () const;	// [mm/h]
  virtual IM deposit () const; // [g [stuff] /cm²/h]
  virtual double cloudiness () const; // [0-1]
  virtual double vapor_pressure () const; // [Pa]
  virtual double wind () const;	// [m/s]

  // Light distribution.
  double day_length () const;	// [h]
  double day_cycle () const;	// Sum over a day is 1.0.

  // Communication with external model.
  virtual void put_precipitation (double prec);// [mm/d]
  virtual void put_air_temperature (double T); // [°C]
  virtual void put_reference_evapotranspiration (double ref); // [mm/d]
  virtual void put_global_radiation (double radiation); // [W/m²]

  // Utility.
public:
  double day_length (const Time& t) const;

  // Average temperature.
public:
  double average () const;
  double amplitude () const;
  double omega () const;
  double max_Ta_yday () const;

  // FAO atmospheric utilities.
public:
  static double LatentHeatVaporization (double Temp /* [dg C] */); // [MJ/kg]
  static double PsychrometricConstant (double AtmPressure /* [kPa] */, 
				       double Temp /* [dg C] */); // [kPa/K]
  double AtmosphericPressure () const; // [kPa]
  static double AirDensity (double AtmPressure /* [kPa] */,
			    double Temp /* [dg C] */); // [kg/m3]
  static double SaturationVapourPressure (double Temp /* [dg C] */); // [kPa]
  static double SlopeVapourPressureCurve (double Temp /* [dg C] */); // [kPa/K]
  double CloudinessFactor_Arid (const Time&, double Si /* [MJ/m2/d] */) const;
  double CloudinessFactor_Humid (const Time&, double Si /* [MJ/m2/d] */) const;
  double RefNetRadiation (const Time& time, double Si /* [MJ/m2/d] */, 
			  double Temp /* [dg C] */, 
			  double ea /* [kPa] */) const; // [MJ/m2/d]

  // Create and Destroy.
private:
  Weather (const Weather&);
protected:
  Weather (const AttributeList& al);
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual ~Weather ();
};

static Librarian<Weather> Weather_init ("weather");

#endif WEATHER_H
