// soil_water.h

#ifndef SOIL_WATER_H
#define SOIL_WATER_H

#include "common.h"
#include <vector>

class AttributeList;
class UZmodel;
class Surface;
class Groundwater;
class Log;
class Soil;
class Syntax;
class Filter;
class Geometry;

class SoilWater
{
  // Content.
  vector<double> S;
  vector<double> Theta_old_;
  vector<double> h_old;
  vector<double> Theta_;
  vector<double> h_;
  vector<double> Xi;
  vector<double> q_;
  UZmodel *const top;
  UZmodel *const bottom;
  const int bottom_start;

  // Sink.
public:
  void clear (const Geometry&);
  void add_to_sink (const vector<double>&);
  void add_to_sink (const vector<double>&, const Soil&);

  // Queries
public:
  double h (int i) const
  { return h_[i]; }
  double pF (int i) const;
  double Theta (int i) const
  { return Theta_[i]; }
  double Theta_old (int i) const
  { return Theta_old_[i]; }
  double q (int i) const
  { return q_[i]; }

  // Simulation.
public:
  void tick (Surface&, Groundwater&, const Soil&);
  void mix (const Soil&, double from, double to);
  void swap (const Soil&, double from, double middle, double to);
  bool check (unsigned n) const;
  void output (Log&, Filter&) const;

  // Communication with surface.
  double MaxExfiltration (const Soil&) const;

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  SoilWater (const Soil&, const AttributeList&);
  void initialize (const Soil& soil, const Groundwater& groundwater);
    ~SoilWater ();
};

#endif SOIL_WATER_H
