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
class Soil;
class SoilWater;
class Groundwater;
class Time;
class Filter;

class SoilHeat
{
  struct Implementation;
  Implementation& impl;
public:
  void tick (const Time&, const Soil&, const SoilWater&, 
	     const Surface&, const Groundwater&);
  double T (int i) const;
  void output (Log&, const Filter&) const;
  bool check (unsigned n) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilHeat (const AttributeList&);
  ~SoilHeat ();
};

#endif SOIL_HEAT_H
