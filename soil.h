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
  const double MaxRootingDepth_;
  double dispersivity_;

public:
  // Water.
  double K (int i, double h, double h_ice) const;
  inline double Cw1 (int i, double h, double h_ice) const
  { return Theta (i, h, h_ice) - Cw2 (i, h) * h; }
  double Cw2 (int i, double h) const;
  double Theta (int i, double h, double h_ice) const;
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
  inline double humus (int i) const
  { return horizon_[i]->humus (); }
  inline double SOM_C (int i, unsigned int pool) const
  { return horizon_[i]->SOM_C (pool); }
  inline double SOM_C_per_N (int i, unsigned int pool) const
  { return horizon_[i]->SOM_C_per_N (pool); }

  // Thermic.
  double heat_conductivity (int i, double Theta, double Ice) const
  { return horizon_[i]->heat_conductivity (Theta, Ice); }
  double heat_capacity (int i, double Theta, double Ice) const
  { return horizon_[i]->heat_capacity (Theta, Ice); }
  
  // Calculations.
  double MaxRootingDepth () const;

  // Debug.
  void make_table (int i);

  // Creation.
  bool check () const;
  static void load_syntax (Syntax&, AttributeList&);
  Soil (const AttributeList&);
  ~Soil ();
};

#endif SOIL_H
