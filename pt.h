// pt.h --- Potential transpiration

#ifndef PT_H
#define PT_H

#include "librarian.h"

class Soil;
class SoilHeat;
class SoilWater;
class Weather;
class CropList;
class Surface;
class Pet;

class PT
{
  // Content.
public:
  const string name;

  // Simulation.
public:
  virtual void tick (const Weather&, const CropList&,
		     const Surface&, const Soil&, const SoilHeat&, 
		     const SoilWater&, const Pet&, 
		     double canopy_ea, double snow_ea,
		     double pond_ea, double soil_ea) = 0;
  virtual double potential_transpiration () const = 0; // [mm/h]
  virtual void output (Log&, Filter&) const;

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
protected:
  PT (const AttributeList&);
public:
  virtual ~PT ();
};

static Librarian<PT> PT_init ("pt");

#endif PT_H

// pt.h ends here.
