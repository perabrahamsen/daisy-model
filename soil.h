// soil.h

#ifndef SOIL_H
#define SOIL_H

#include "horizon.h"
#include <vector.h>

struct AttributeList;
struct Log;

class Soil
{
  const vector<double> zplus_;	// Lower boundary of each interval.
  vector<double> z_;		// (c) Center of each interval.
  vector<double> dz_;		// (c) Size of each interval.
  vector<const Horizon*> horizon_;
  const int size_;
  const double EpFactor_;
  const double EpInterchange_;
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
  int interval_plus (double z) const;
  int interval (double z) const;

  // Horizons.
  inline double K (int i, double h) const
  { return horizon_[i]->K (h); }
  inline double Cw1 (int i, double h) const
  { return Theta (i, h) - Cw2 (i, h) * h; }
  inline double Cw2 (int i, double h) const
  { return horizon_[i]->Cw2 (h); }
  inline double Theta (int i, double h) const
  { return horizon_[i]->Theta (h); }
  inline double Theta_res (int i) const
  { return horizon_[i]->Theta_res (); }
  inline double h (int i, double h) const
  { return horizon_[i]->h (h); }
  inline double M (int i, double h) const
  { return horizon_[i]->M (h); }
  inline bool compact (int i) const
  { return horizon_[i]->compact (); }
  inline double lambda (int i) const
  { return horizon_[i]->lambda; }
  
  // Calculations.
  double MaxRootingDepth () const;
  double EpFactor () const;
  double EpInterchange () const;

  // Simulation.
  bool check () const;

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  Soil (const AttributeList&);
  ~Soil ();
};

#endif SOIL_H
