// soil_heat.h

#ifndef SOIL_HEAT_H
#define SOIL_HEAT_H

#include <string>
#include <vector>

class AttributeList;
class Log;
class Surface;
class Bioclimate;
class Syntax;

class SoilHeat
{
  vector<double> T;
public:
  void tick (const Surface&, const Bioclimate&);
  inline double temperature (int i) const
  { return T[i]; }
  bool check (unsigned n) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilHeat (const AttributeList&);
};

#endif SOIL_HEAT_H
