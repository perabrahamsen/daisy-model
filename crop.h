// crop.h

#ifndef CROP_H
#define CROP_H

#include "daisy.h"

struct AttributeList;

class Crop 
{
    // Content.
public:
    struct Parameters;
    struct Variables;
protected:
    const Parameters& par;
    Variables& var;
public:
    const string name;

    // Simulation.
protected:
    // <insert calculation functions here>
public:
    void tick (const Wheather& wheater, int day, int hour);
    void output (Log&, const Filter*) const;

    // Create and Destroy.
public:
    Crop (const string, const AttributeList& pl);
    Crop (const string, const AttributeList& pl, const AttributeList& pl);
    ~Crop ();
};

#endif CROP_H
