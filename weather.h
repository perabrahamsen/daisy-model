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
  const Implementation& impl;
public: 
  const string name;
  
  // Simulation.
public:
  virtual void tick (const Time& time) = 0;
  virtual void output (Log&, Filter&) const;

  // Communication with Biocliamte.
public:
  virtual double AirTemperature () const = 0;
  virtual double GlobalRadiation () const = 0;
  virtual double DailyRadiation () const = 0;
  virtual double ReferenceEvapotranspiration () const = 0;
  virtual double Rain () const = 0;
  virtual double Snow () const = 0;
  virtual IM Deposit () const; // [g [stuff] /cm²/h]

  // Light distribution.
  double DayLength (const Time& time) const;
  double DayCycle (const Time& time) const;

  // Utility.
public:
  static double DayLength(double Latitude, const Time& t);

    // Create and Destroy.
protected:
  Weather (const AttributeList& al);
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual ~Weather ();
};

static Librarian<Weather> Weather_init ("weather");

#endif WEATHER_H
