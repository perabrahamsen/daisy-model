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
  virtual double AirTemperature () const = 0;
  virtual double GlobalRadiation () const = 0;
  virtual double DailyRadiation () const = 0;
  virtual double ReferenceEvapotranspiration () const = 0;
  virtual double Rain () const;
  virtual double Snow () const;
  virtual IM Deposit () const; // [g [stuff] /cm²/h]
  virtual double Cloudiness () const; // [0-1]
  virtual double VaporPressure () const; // [Pa]
  virtual double Wind () const;	// [m/s]

  // Light distribution.
  double DayLength () const;
  double DayCycle () const;

  // Communication with external model.
  virtual void put_precipitation (double prec);// [mm/d]
  virtual void put_air_temperature (double T); // [°C]
  virtual void put_reference_evapotranspiration (double ref); // [mm/d]

  // Utility.
public:
  static double DayLength(double Latitude, const Time& t);

  // Average temperature.
public:
  double average () const;
  double amplitude () const;
  double omega () const;
  double omega_offset () const;

  // Create and Destroy.
protected:
  Weather (const AttributeList& al);
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual ~Weather ();
};

static Librarian<Weather> Weather_init ("weather");

#endif WEATHER_H
