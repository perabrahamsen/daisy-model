// soil.h

#ifndef SOIL_H
#define SOIL_H

#include "horizon.h"
#include <vector.h>

struct AttributeList;

class Soil
{
  vector<double> zplus_;	// Lower boundary of each interval.
  vector<double> z_;		// (c) Center of each interval.
  vector<double> dz_;		// (c) Size of each interval.
  vector<const Horizon*> horizon_;
  int size_;
public:
  // Geometry.
  inline int size () const
  { return size_; }
  inline double zplus (int i) const
  { return zplus_[i]; }
  inline double z (int i) const
  { return z_[i]; }
  inline double dz (int i) const
  { return dz_[i]; }
  int interval (double z) const;

  // Horizons.
  inline double K (int i, double h) const
  { return horizon_[i]->K (h); }
  inline double Cw1 (int i, double h) const
  { return horizon_[i]->Cw1 (h); }
  inline double Cw2 (int i, double h) const
  { return horizon_[i]->Cw2 (h); }
  inline double Theta (int i, double h) const
  { return horizon_[i]->Theta (h); }
  inline bool compact (int i) const
  { return horizon_[i]->compact (); }
  
  // Calculations.
  double MaxRootingDepth () const;

  // Simulation.
  bool check (Log& log) const;

  Soil (const AttributeList&);
  ~Soil ();
};

#endif SOIL_H
