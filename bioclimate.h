// bioclimate.h

#ifndef BIOCLIMATE_H
#define BIOCLIMATE_H

#include "time.h"

class AttributeList;
class Input;

class Bioclimate
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
    void add (const AttributeList&);
    void set (const Time&);
    Bioclimate ();
public:
    ~Bioclimate ();
};

#endif BIOCLIMATE_H
