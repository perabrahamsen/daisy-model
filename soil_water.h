// soil_water.h
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


#ifndef SOIL_WATER_H
#define SOIL_WATER_H

#include "macro.h"		// Must be included to intitalize the library.
#include "common.h"
#include <vector>

class AttributeList;
class UZmodel;
class Surface;
class Groundwater;
class Log;
class Soil;
class Syntax;
class Geometry;

class SoilWater
{
  // Content.
  vector<double> S_sum_;
  vector<double> S_root_;
  vector<double> S_drain_;
  vector<double> S_p_;
  vector<double> S_permanent_;
  vector<double> Theta_old_;
  vector<double> h_old;
  vector<double> Theta_;
  vector<double> h_;
  vector<double> S_ice_;
  vector<double> X_ice_;
  vector<double> X_ice_buffer;
  vector<double> h_ice_;
  vector<double> q_;
  vector<double> q_p_;
  UZmodel *const top;
  UZmodel *const bottom;
  const int bottom_start;
  UZmodel *const reserve;
  Macro& macro;

  // Sink.
public:
  void clear (const Geometry&);
  void root_uptake (const vector<double>&);
  void freeze (const Soil&, const vector<double>&);
  
  // Queries
public:
  double h (int i) const
  { return h_[i]; }
  double pF (int i) const;
  double Theta (int i) const
  { return Theta_[i]; }
  double Theta_left (int i) const
  { return Theta_[i] - S_sum_[i]; }
  double Theta_old (int i) const

  { return Theta_old_[i]; }
  double q (int i) const
  { return q_[i]; }
  double q_p (int i) const
  { return q_p_[i]; }
  double S_sum (int i) const
  { return S_sum_[i]; }
  double S_root (int i) const
  { return S_root_[i]; }
  double S_drain (int i) const
  { return S_drain_[i]; }
  double S_ice (int i) const
  { return S_ice_[i]; }
  double S_p (int i) const
  { return S_p_[i]; }
  double h_ice (int i) const
  { return h_ice_[i]; }
  double X_ice (int i) const
  { return X_ice_[i]; }
  double X_ice_total (int i) const
  { return X_ice_[i] + X_ice_buffer[i]; }

  unsigned int first_groundwater_node () const;
    
  // Ice modified lookups.
  double Theta (const Soil&, int i, double h) const;
 
  // Simulation.
public:
  void macro_tick (const Soil&, Surface&);
  void tick (const Soil&, Surface&, Groundwater&);
  void set_external_source (const Geometry&, 
			    double amount, double from, double to);
  void mix (const Soil&, double from, double to);
  void swap (const Soil&, double from, double middle, double to);
  void set_Theta (const Soil& soil, 
		  unsigned int from, unsigned int to, double Theta);
  bool check (unsigned n, Treelog& err) const;
  void output (Log&) const;


  // Communication with surface.
  double MaxExfiltration (const Soil&) const;

  // Communication with external model.
  void put_h (const Soil& soil, const vector<double>& v); // [cm]
  void get_sink (vector<double>& v) const // [cm^3/cm^3/h]
    { v = S_sum_; }

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  SoilWater (const AttributeList&);
  void initialize (const AttributeList&, 
		   const Soil& soil, const Groundwater& groundwater);
    ~SoilWater ();
};

#endif // SOIL_WATER_H
