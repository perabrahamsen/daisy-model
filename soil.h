// soil.h

#ifndef SOIL_H
#define SOIL_H

#include "horizon.h"
#include "hydraulic.h"
#include <vector>

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
  const double MaxRootingDepth_;
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

  // Water.
  inline double K (int i, double h) const
  { return horizon_[i]->hydraulic.K (h); }
  inline double Cw1 (int i, double h) const
  { return Theta (i, h) - Cw2 (i, h) * h; }
  inline double Cw2 (int i, double h) const
  { return horizon_[i]->hydraulic.Cw2 (h); }
  inline double Theta (int i, double h) const
  { return horizon_[i]->hydraulic.Theta (h); }
  inline double Theta_res (int i) const
  { return horizon_[i]->hydraulic.Theta_res; }
  inline double h (int i, double Theta) const
  { return horizon_[i]->hydraulic.h (Theta); }
  inline double M (int i, double h) const
  { return horizon_[i]->hydraulic.M (h); }
  inline bool compact (int i) const
  { return horizon_[i]->hydraulic.compact (); }
  inline double lambda (int i) const
  { return horizon_[i]->hydraulic.lambda; }
  
  // Absorbtion.
  inline double v_planar (int i) const
  { return horizon_[i]->v_planar (); }
  inline double v_edge (int i) const
  { return horizon_[i]->v_edge (); }
  inline double K_planar (int i) const
  { return horizon_[i]->v_planar (); }
  inline double K_edge (int i) const
  { return horizon_[i]->v_edge (); }
  double tortuosity_factor (int i, double Theta) const
  { return horizon_[i]->tortuosity_factor (Theta); }

  // Thermic.
  double heat_conductivity (int i, double Theta, double Ice = 0.0) const
  { return horizon_[i]->heat_conductivity (Theta, Ice); }
  double heat_capacity (int i, double Theta, double Ice = 0.0) const
  { return horizon_[i]->heat_capacity (Theta, Ice); }
  
  // Content.
  inline double clay (int i) const
  { return horizon_[i]->clay (); }
  inline double initial_C (int i) const
  { return horizon_[i]->C (); }
  inline double initial_N (int i) const
  { return horizon_[i]->N (); }
  
  // Calculations.
  double MaxRootingDepth () const;
  double EpFactor () const;
  double EpInterchange () const;

  // Simulation.
  bool check () const;
  
  // Vector operations.
  void mix (vector<double>& v, double from, double to) const;
  void add (vector<double>& v, double from, double to, double amount) const;
  double extract (vector<double>& v, double from, double to) const;
  void set (vector<double>& v, double from, double to, double amount) const;
  void swap (vector<double>& v, double from, double middle, double to) const;
  double total (const vector<double>& v) const;

  // Debug.
  void make_table (int i);

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  Soil (const AttributeList&);
  ~Soil ();
};

#endif SOIL_H
