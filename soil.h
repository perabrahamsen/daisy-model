// soil.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef SOIL_H
#define SOIL_H

#include "horizon.h"
#include "hydraulic.h"
#include "tortuosity.h"
#include "geometry.h"

struct AttributeList;

class Soil : public Geometry
{
  // Content.
  struct Implementation;
  Implementation& impl;
  // Cache for fast inline access.
  /* const */ std::vector<Horizon*> horizon_;

public:
  // Water.
  double K (int i, double h, double h_ice, double T) const;
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
  double dispersivity (int) const;
  void set_porosity (int i, double Theta)
  { horizon_[i]->hydraulic.set_porosity (Theta); }
  
  // Texture.
  inline double tortuosity_factor (int i, double Theta) const
  { return horizon_[i]->tortuosity.factor (horizon_[i]->hydraulic, Theta); }
  inline double dry_bulk_density (int i) const
  { return horizon_[i]->dry_bulk_density (); }
  inline double clay (int i) const
  { return horizon_[i]->clay (); }
  inline double humus (int i) const
  { return horizon_[i]->humus (); }
  inline double humus_C (int i) const
  { return horizon_[i]->humus_C (); }
  inline const std::vector<double>& SOM_fractions (int i) const
  { return horizon_[i]->SOM_fractions (); }
  inline const std::vector<double>& SOM_C_per_N (int i) const
  { return horizon_[i]->SOM_C_per_N (); }

  // Thermic.
  double heat_conductivity (int i, double Theta, double Ice) const
  { return horizon_[i]->heat_conductivity (Theta, Ice); }
  double heat_capacity (int i, double Theta, double Ice) const
  { return horizon_[i]->heat_capacity (Theta, Ice); }
  
  // Chemistry.
  bool has_attribute (const std::string& name) const;
  bool has_attribute (int i, const std::string& name) const
  { return horizon_[i]->has_attribute (name); }
  double get_attribute (int i, const std::string& name) const
  { return horizon_[i]->get_attribute (name); }

  // Simulation.
public:
  void output (Log&) const;

  // Calculations.
  double MaxRootingDepth () const;
  double end_of_first_horizon () const;

  // Debug.
  void make_table (int i);

  // Creation.
  bool check (int som_size, Treelog&) const;
  static void load_syntax (Syntax&, AttributeList&);
  Soil (const AttributeList&);
  void initialize (const Groundwater&, Treelog&);
  ~Soil ();
};

#endif // SOIL_H
