// weather.h

#ifndef WEATHER_H
#define WEATHER_H

#include "time.h"

class AttributeList;
class Input;
class Library;
class Syntax;

class Weather
{
  // Content.
protected:
  const Time& time;
  const double Latitude;

  // Simulation.
public:
  virtual void tick () = 0;
  virtual double AirTemperature () const = 0;
  virtual double GlobalRadiation () const = 0;
  double DayLength () const;

  // Utility.
public:
  static double DayLength(double Latitude, const Time& t);

  // Library.
public:
  static const Library& library ();
  static Weather& create (const Time&, const AttributeList&);
  typedef Weather& (*constructor) (const Time&, const AttributeList&);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);

    // Create and Destroy.
protected:
  Weather (const Time&, double latitude);
public:
  virtual ~Weather ();
};

// Ensure the Weather library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Weather_init
{
  static int count;
public:
  Weather_init ();
  ~Weather_init ();
} Weather_init;

#endif WEATHER_H
