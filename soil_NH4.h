// soil_NH4.h

#ifndef SOIL_NH4_H
#define SOIL_NH4_H

#include "solute.h"

class SoilNH4
{
public:
  static void load_syntax (Syntax&, AttributeList&);
  SoilNH4 (const Soil&, const SoilWater&, const AttributeList&);
};


#endif SOIL_NH4_H
