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
  vector<double> h_;
  vector<double> Xi;
  vector<double> q;
  UZmodel *const top;
  UZmodel *const bottom;
  const int bottom_start;

  // Simulation.
public:
  void clear ();
  void add_to_sink (const vector<double>&);
  double h (int i) const
  { return h_[i]; }
  void tick (Surface&, Groundwater&, const Soil&);
  bool check (Log&, unsigned n) const;
  void output (Log&, const Filter*) const;

  // Communication with surface.
  double MaxExfiltration (const Soil&) const;

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  SoilWater (const Soil&, const AttributeList&);
  ~SoilWater ();
};

#endif SOIL_WATER_H
