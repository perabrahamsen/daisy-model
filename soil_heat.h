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
  struct Implementation;
  Implementation& impl;
public:
  void tick (const Surface&, const Bioclimate&);
  double temperature (int i) const;
  bool check (unsigned n) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilHeat (const AttributeList&);
  ~SoilHeat ();
};

#endif SOIL_HEAT_H
