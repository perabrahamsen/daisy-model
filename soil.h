// soil.h

#ifndef SOIL_H
#define SOIL_H

#include "horizon.h"
#include "hydraulic.h"
#include "tortuosity.h"
#include "geometry.h"

struct AttributeList;

class Soil : public Geometry
{
  vector<const Horizon*> horizon_;
  const double EpFactor_;
  const double EpInterchange_;
  const double MaxRootingDepth_;
  double dispersivity_;

public:
  // Water.
  inline double K (int i, double h) const
  { return horizon_[i]->hydraulic.K (h); }
  inline double Cw1 (int i, double h) const
  { return Theta (i, h) - Cw2 (i, h) * h; }
  double Cw2 (int i, double h) const;
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
  inline double dispersivity (int) const
  { return dispersivity_; }
  
  // Texture.
  inline double tortuosity_factor (int i, double Theta) const
    { return horizon_[i]->tortuosity.factor (horizon_[i]->hydraulic, Theta); }
  inline double dry_bulk_density (int i) const
    { return horizon_[i]->dry_bulk_density (); }
  inline double clay (int i) const
  { return horizon_[i]->clay (); }
  inline double SOM_C (int i, unsigned int pool) const
  { return horizon_[i]->SOM_C (pool); }
  inline double SOM_C_per_N (int i, unsigned int pool) const
  { return horizon_[i]->SOM_C_per_N (pool); }

  // Thermic.
  double heat_conductivity (int i, double Theta, double Ice = 0.0) const
  { return horizon_[i]->heat_conductivity (Theta, Ice); }
  double heat_capacity (int i, double Theta, double Ice = 0.0) const
  { return horizon_[i]->heat_capacity (Theta, Ice); }
  
  // Calculations.
  double MaxRootingDepth () const;
  double EpFactor () const;
  double EpInterchange () const;

  // Simulation.
  bool check () const;
  
  // Debug.
  void make_table (int i);

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  Soil (const AttributeList&);
  ~Soil ();
};

#endif SOIL_H
