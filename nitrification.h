// nitrification.h

#ifndef NITRIFICATION_H
#define NITRIFICATION_H

#include "librarian.h"

class Soil;
class SoilWater;
class SoilHeat;
class SoilNO3;
class SoilNH4;
class Groundwater;

class Nitrification
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (const Soil&, const SoilWater&, const SoilHeat&,
		     SoilNO3&, SoilNH4&, const Groundwater&) = 0;
  virtual void output (Log&, Filter&) const = 0;

  // Create and Destroy.
protected:
  Nitrification (const string& name);
public:
  virtual ~Nitrification ();
};

static Librarian<Nitrification> Nitrification_init ("nitrification");

#endif NITRIFICATION_H
