// soil_NH4.h

#ifndef SOIL_NH4_H
#define SOIL_NH4_H

#include "solute.h"

class SoilNH4 : public Solute
{
public:
  // Substance specific constants.
  double beta (const Soil&, const SoilWater&, int i, double C) const; // dA/dC
  double diffusion_coefficient () const; // in free solu. [m² / s]
  double C_to_A (const Soil&, int i, double C) const;
  double C_to_M (const Soil&, double Theta, int i, double C) const;
  double M_to_C (const Soil&, double Theta, int i, double M) const;

public:
  static void load_syntax (Syntax&, AttributeList&);
  SoilNH4 (const Soil&, const SoilWater&, const AttributeList&);
};

#endif SOIL_NH4_H
