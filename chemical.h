// chemical.h

#ifndef CHEMICAL_H
#define CHEMICAL_H

#include "librarian.h"

class Chemical
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Queries.
public:
  virtual double crop_uptake_reflection_factor () const	= 0; // [0-1]
  virtual double canopy_dissipation_rate_coefficient () const = 0; // [h^-1]
  virtual double canopy_washoff_coefficient () const = 0; // [mm]
  virtual double diffusion_coefficient () const = 0; // in free solu. [cm² / h]
  virtual const AttributeList& solute_alist () const = 0;
  virtual double decompose_rate () const = 0; // [h^-1]
  virtual double decompose_heat_factor (double T) const = 0; // [dg C ->]
  virtual double decompose_water_factor (double h) const = 0 ; // [cm ->]
  virtual double decompose_CO2_factor (double CO2) const = 0; // [g C/cm^3 ->]

  // Create and Destroy.
protected:
  Chemical (const AttributeList&);
public:
  virtual ~Chemical ();
};

static Librarian<Chemical> Chemical_init ("chemical");

#endif CHEMICAL_H
