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

#include "macro.h"		// Must be initialized.
#include <vector>

class AttributeList;
class Surface;
class Groundwater;
class Log;
class Soil;
class Syntax;
class Geometry;
class SoilHeat;
class Treelog;

class SoilWater
{
  // Content.
  struct Implementation;
  Implementation& impl;

  // Sink.
public:
  void clear (const Geometry&);
  void root_uptake (const std::vector<double>&);
  void drain (const std::vector<double>&);
  void freeze (const Soil&, const std::vector<double>&);
  
  // Queries
public:
  double h (int i) const;
  double pF (int i) const;
  double Theta (int i) const;
  double Theta_left (int i) const;
  double Theta_old (int i) const;
  double content (const Geometry&, double from, double to) const; // [cm]
  double q (int i) const;
  double q_p (int i) const;
  double S_sum (int i) const;
  double S_root (int i) const;
  double S_drain (int i) const;
  double S_ice (int i) const;
  double S_p (int i) const;
  double h_ice (int i) const;
  double X_ice (int i) const;
  double X_ice_total (int i) const;

  unsigned int first_groundwater_node () const;
    
  // Ice modified lookups.
  double Theta (const Soil&, int i, double h) const;
 
  // Simulation.
public:
  void macro_tick (const Soil&, Surface&, Treelog&);
  void tick (const Soil&, const SoilHeat&, Surface&, Groundwater&, Treelog&);
  void set_external_source (const Geometry&, 
			    double amount, double from, double to);
  void incorporate (const Geometry&, double amount, double from, double to);
  void mix (const Soil&, double from, double to);
  void swap (Treelog&, const Soil&, double from, double middle, double to);
  void set_Theta (const Soil& soil, 
		  unsigned int from, unsigned int to, double Theta);
  bool check (unsigned n, Treelog& err) const;
  void output (Log&) const;


  // Communication with surface.
  double MaxExfiltration (const Soil&, double T) const;

  // Communication with external model.
  void put_h (const Soil& soil, const std::vector<double>& v); // [cm]
  void get_sink (std::vector<double>& v) const; // [cm^3/cm^3/h]

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  SoilWater (const AttributeList&);
  void initialize (const AttributeList&, 
		   const Soil& soil, const Groundwater& groundwater,
		   Treelog&);
  ~SoilWater ();
};

#endif // SOIL_WATER_H
