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

#include "geometry.h"
#include "horizon.h" // Needed for initialization order.

struct AttributeList;
struct Log;

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
  double Cw1 (int i, double h, double h_ice) const;
  double Cw2 (int i, double h) const;
  double Theta (int i, double h, double h_ice) const;
  double Theta_res (int i) const;
  double h (int i, double Theta) const;
  double M (int i, double h) const;
  double dispersivity (int) const;
  void set_porosity (int i, double Theta);
  
  // Texture.
  double tortuosity_factor (int i, double Theta) const;
  double anisotropy (int i) const;
  double dry_bulk_density (int i) const;
  double clay (int i) const;
  double texture_below (int i, double size /* [um] */) const;
  double humus (int i) const;
  double humus_C (int i) const;
  const std::vector<double>& SOM_fractions (int i) const;
  const std::vector<double>& SOM_C_per_N (int i) const;
  double C_per_N (int i) const;
  double turnover_factor (int i) const;

  // Thermic.
  double heat_conductivity (int i, double Theta, double Ice) const;
  double heat_capacity (int i, double Theta, double Ice) const;
  
  // Chemistry.
  bool has_attribute (const std::string& name) const;
  bool has_attribute (int i, const std::string& name) const;
  double get_attribute (int i, const std::string& name) const;
  std::string get_dimension (int i, const std::string& name) const;

  // 

  // Simulation.
public:
  void output (Log&) const;
  void nitrification (const size_t i,
                      const double M, const double C, 
                      const double M_left,
                      const double h, const double T,
                      double& NH4, double& N2O, double& NO3) const;

  // Calculations.
  double MaxRootingDepth () const;
  double end_of_first_horizon () const;

  // Debug.
  void make_table (int i);

  // Creation.
  bool check (int som_size, Treelog&) const;
  static void load_syntax (Syntax&, AttributeList&);
  Soil (const AttributeList&);
  void initialize (Groundwater&, int som_size, Treelog&);
  ~Soil ();
};

#endif // SOIL_H
