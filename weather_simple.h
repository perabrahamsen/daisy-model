// weather_simple.h

#ifndef WEATHER_SIMPLE_H
#define WEATHER_SIMPLE_H

#include "weather.h"

class WeatherSimple : public Weather
{
    // Simulation.
public:
  void tick ();
  double AirTemperature () const;
  double GlobalRadiation () const;

    // Create and Destroy.
private:
  friend class WeatherSimpleSyntax;
  static Weather& make (const Time&, const AttributeList&);
  WeatherSimple (const Time&, const AttributeList&);
public:
  ~WeatherSimple ();
};

#endif WEATHER_SIMPLE_H
