// weather.h

#ifndef WEATHER_H
#define WEATHER_H

#include "time.h"

class AttributeList;
class Input;

class Weather
{
    // Content.
private:
    struct Implementation;
    Implementation& impl;
    Time time;

    // Simulation.
public:
    void tick ();
    double AirTemperature () const;
    double GlobalRadiation () const;
    double DayLength () const;

    // Utility.
public:
    static double DayLength(double Latitude, const Time& t);
    // Create and Destroy.
private:
    friend Input; // Only create from Input.
    void set (const Time&);
    Weather (const AttributeList&);
public:
    ~Weather ();
};

#endif WEATHER_H
