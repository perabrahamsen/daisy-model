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
  double energy (const Soil&, const SoilWater&, double from, double to) const;
  void set_energy (const Soil&, const SoilWater&, 
		   double from, double to, double energy);
  void swap (const Soil&, double from, double middle, double to);
  double T (int i) const;
  void output (Log&, Filter&) const;
  bool check (unsigned n) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilHeat (const AttributeList&);
  void initialize (const AttributeList& al, 
		   const Soil& soil, const Time& time);
  ~SoilHeat ();
};

#endif SOIL_HEAT_H
