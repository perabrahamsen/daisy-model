// soil_water1d.h
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


#ifndef SOIL_WATER1D_H
#define SOIL_WATER1D_H

#include "soil_water.h"
#include "macro.h"		// Must be initialized.

class Geometry1D;
class UZmodel;

class SoilWater1D : public SoilWater
{
  // Content.
  std::vector<double> S_p_;
  std::vector<double> S_permanent_;
  std::vector<double> h_old_;
  std::vector<double> S_ice_;
  std::vector<double> X_ice_;
  std::vector<double> X_ice_buffer_;
  std::vector<double> h_ice_;
  std::vector<double> q_;
  std::vector<double> q_p_;
  std::auto_ptr<UZmodel> top;
  std::auto_ptr<UZmodel> reserve;
  std::auto_ptr<Macro> macro;

  // Sink.
public:
  void clear ();
  void drain (const std::vector<double>&);
  void freeze (const Soil&, const std::vector<double>&);
  
  // Queries
public:
  double pF (size_t i) const;
  double q (size_t i) const
  { return q_[i]; }
  double q_p (size_t i) const
  { return q_p_[i]; }
  double S_sum (size_t i) const
  { return S_sum_[i]; }
  double S_ice (size_t i) const
  { return S_ice_[i]; }
  double S_p (size_t i) const
  { return S_p_[i]; }
  double h_ice (size_t i) const
  { return h_ice_[i]; }
  double X_ice (size_t i) const
  { return X_ice_[i]; }
  double X_ice_total (size_t i) const
  { return X_ice_[i] + X_ice_buffer_[i]; }
  double top_flux () const
  { return q (0); }
    
  // Simulation.
public:
  void macro_tick (const Geometry1D&, const Soil&, Surface&, Treelog&);
  void tick (const Geometry1D& geo,
             const Soil&, const SoilHeat&, Surface&, Groundwater&, Treelog&);
  bool check (size_t n, Treelog& err) const;
  void output (Log&) const;


  // Communication with surface.
  double MaxExfiltration (const Geometry& geo,
                          const Soil&, double T) const;

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  SoilWater1D (Block&);
  void initialize (const AttributeList&, 
		   const Geometry1D& geo,
                   const Soil& soil, const Groundwater& groundwater,
		   Treelog&);
  ~SoilWater1D ();
};

#endif // SOIL_WATER1D_H
