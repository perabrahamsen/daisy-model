// pet.h -- Potential evopotranspiration

#ifndef PET_H
#define PET_H

#include "librarian.h"

class Soil;
class SoilHeat;
class SoilWater;
class Weather;
class Vegetation;
class Surface;

class Pet
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Utilities.
public:
  static double reference_to_potential (const Vegetation&, const Surface&, 
					double ref);

  // Simulation.
public:
  virtual void tick (const Weather&, const Vegetation&,
		     const Surface&, const Soil&, const SoilHeat&, 
		     const SoilWater&) = 0;
  virtual double wet () const = 0; // [mm/h]
  virtual double dry () const; // [mm/h]
  virtual void output (Log&) const;

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
protected:
  Pet (const AttributeList&);
public:
  virtual ~Pet ();
};

static Librarian<Pet> Pet_init ("pet");

#endif // PET_H
