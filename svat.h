// svat.h --- Soil, Vegetation and ATmostphere.

#ifndef SVAT_H
#define SVAT_H

#include "librarian.h"

class Soil;
class SoilHeat;
class SoilWater;
class Weather;
class Vegetation;
class Surface;
class Pet;

class SVAT
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (const Weather&, const Vegetation&,
		     const Surface&, const Soil&, const SoilHeat&, 
		     const SoilWater&, const Pet&,
		     double canopy_ea, double snow_ea,
		     double pond_ea, double soil_ea, double crop_ea,
                     double crop_ep) = 0;
  virtual void output (Log&) const;
  virtual double production_stress () const = 0; // []

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
protected:
  SVAT (const AttributeList&);
public:
  virtual ~SVAT ();
};

static Librarian<SVAT> SVAT_init ("svat");

#endif SVAT_H

// svat.h ends here.
