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
  
  // Simulation.
public:
  virtual void tick (const Time& time) = 0;
  virtual void output (Log&, Filter&) const;
protected:
  void distribute (double precipitation);

  // Communication with Bioclimate.
public:
  virtual double hourly_air_temperature () const;
  virtual double daily_air_temperature () const = 0;
  virtual double hourly_global_radiation () const;
  virtual double daily_global_radiation () const = 0;
  virtual double reference_evapotranspiration () const;
  virtual double rain () const;
  virtual double snow () const;
  virtual IM deposit () const; // [g [stuff] /cm²/h]
  virtual double cloudiness () const; // [0-1]
  virtual double vapor_pressure () const; // [Pa]
  virtual double wind () const;	// [m/s]

  // Light distribution.
  double day_length () const;
  double day_cycle () const;

  // Communication with external model.
  virtual void put_precipitation (double prec);// [mm/d]
  virtual void put_air_temperature (double T); // [°C]
  virtual void put_reference_evapotranspiration (double ref); // [mm/d]

  // Utility.
public:
  static double day_length (double Latitude, const Time& t);

  // Average temperature.
public:
  double average () const;
  double amplitude () const;
  double omega () const;
  double max_Ta_yday () const;

  // Create and Destroy.
protected:
  Weather (const AttributeList& al);
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual ~Weather ();
};

static Librarian<Weather> Weather_init ("weather");

#endif WEATHER_H
