// crop.h

#ifndef CROP_H
#define CROP_H

#include "daisy.h"

struct ValueList;

class Crop 
{
    // Content.
public:
    struct Parameters;
    struct Variables;
protected:
    const Parameters& par;
    Variables& var;
    Log& log;
public:
    const string name;
    const Column& column;

    // Simulation.
protected:
    // <insert calculation functions here>
public:
    void tick (const Wheather& wheater, int day, int hour);

    // Create and Destroy.
public:
    Crop (Log&, const string, const ValueList*, Column&);
    ~Crop ();
};

#endif CROP_H
