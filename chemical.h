// chemical.h

#ifndef CHEMICAL_H
#define CHEMICAL_H

#include "librarian.h"

class Soil;
class SoilWater;
class SoilHeat;
class SoilNO3;
class SoilNH4;
class Groundwater;

class Chemical
{
  // Content.
public:
  const string name;

  // Queries.
public:
  virtual double crop_uptake_reflection_factor () const	= 0; // [0-1]
  virtual double canopy_dissipation_rate_coefficient () const = 0; // [h^-1]
  virtual double canopy_washoff_coefficient () const = 0; // [mm]

  // Create and Destroy.
protected:
  Chemical (const AttributeList&);
public:
  virtual ~Chemical ();
};

static Librarian<Chemical> Chemical_init ("chemical");

#endif CHEMICAL_H
