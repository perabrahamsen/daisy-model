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
    virtual void tick (const Bioclimate&, const Time&);
    virtual void output (Log&, const Filter*) const;

    // Create and Destroy.
public:
    Crop (const string, const AttributeList& pl);
    Crop (const string, const AttributeList& pl, const AttributeList& pl);
    virtual ~Crop ();
};

#endif CROP_H
