// soil_heat.h

#ifndef SOIL_HEAT_H
#define SOIL_HEAT_H

#include "daisy.h"
#include <vector.h>

class AttributeList;
class Log;
class Surface;
class Bioclimate;

class SoilHeat
{
  vector<double> T;
public:
  void tick (const Surface&, const Bioclimate&);
  inline double temperature (int i) const
  { return T[i]; }
  bool check (Log& log, unsigned n) const;
  SoilHeat (const AttributeList& par, const AttributeList& var);
};

#endif SOIL_HEAT_H
