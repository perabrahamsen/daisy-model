// pet.h -- Potential evopotranspiration

#ifndef PET_H
#define PET_H

#include "librarian.h"

class Soil;
class SoilHeat;
class Weather;
class CropList;

class Pet
{
  // Content.
public:
  const string name;

  // Simulation.
public:
  virtual void tick (const Weather&, const CropList&,
		     const Soil&, const SoilHeat&) = 0;
  virtual double wet () const = 0;
  virtual double dry () const;
  virtual void output (Log&, Filter&) const;

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
protected:
  Pet (const AttributeList&);
public:
  virtual ~Pet ();
};

static Librarian<Pet> Pet_init ("pet");

#endif PET_H
