// crop.h

#ifndef CROP_H
#define CROP_H

#include "daisy.h"

struct ValueList;

class Crop 
{
    // Content.
    struct Implementation;
    Implementation& impl;
    Log& log;
public:
    const string name;
    const Column& column;

    // Simulation.
public:
    void tick (const Wheather& wheater, int day, int hour);

    // Create and Destroy.
public:
    Crop (Log&, const string, const ValueList*, Column&);
    ~Crop ();
};

#endif CROP_H
