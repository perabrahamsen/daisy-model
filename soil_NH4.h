// soil_NH4.h

#ifndef SOIL_NH4_H
#define SOIL_NH4_H

#include "solute.h"

class SoilNH4 : public Solute
{
public:
  // Substance specific constants.
  double diffusion_coefficient () const; // in free solu. [cm² / h]

public:
  static void load_syntax (Syntax&, AttributeList&);
  SoilNH4 (const AttributeList&);
};

#endif SOIL_NH4_H
