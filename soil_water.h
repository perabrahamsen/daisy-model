// soil_water.h

#ifndef SOIL_WATER_H
#define SOIL_WATER_H

#include <std/string.h>
#include <vector.h>

class AttributeList;
class UZmodel;
class Surface;
class Groundwater;
class Log;
class Soil;
class Syntax;
class Filter;

class SoilWater
{
  // Content.
  vector<double> S;
  vector<double> Theta_old;
  vector<double> h_old;
  vector<double> Theta;
  vector<double> h;
  vector<double> Xi;
  vector<double> q;
  UZmodel *const top;
  UZmodel *const bottom;
  const int bottom_start;

  // Simulation.
public:
  void tick (Surface&, const Groundwater&, const Soil&);
  bool check (Log&, unsigned n) const;
  void output (Log&, const Filter*) const;

  // Creation.
  static const Syntax* SoilWater::parameter_syntax ();
  static const Syntax* SoilWater::variable_syntax ();
  SoilWater (const Soil&, const AttributeList& par, const AttributeList& var);
  ~SoilWater ();
};

#endif SOIL_WATER_H
