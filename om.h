// om.h --- A single organic matter pool.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 Per Abrahamsen and KVL.
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


#ifndef OM_H
#define OM_H

#include "common.h"
#include "plf.h"
#include <vector>

class AttributeList;
class Syntax;
class Log;
class Geometry;
class SOM;
class SMB;
class DOM;

class OM
{ 
  // Parameters
public:
  static const double Unspecified;// No initial fraction specified.
  const double initial_fraction; // Relative fraction for this om.
  const double initial_C_per_N;	// Initial value for C/N.
  /* const */ vector<double> C_per_N_goal; // Prefered C/N value.
  /* const */ PLF heat_factor;
  /* const */ PLF water_factor;

  // Content.
public:
  vector<double> C;		// Carbon in each node.
  vector<double> N;		// Nitrogen in each node.
  const double turnover_rate;	// How fast this is it turned over?
  const vector<double> efficiency;	// How digestible is this?
  const vector<double> fractions;	// How much is turned into SMB and SOM?

  // Simulation.
public:
  void output (Log&) const;
  void mix (const Geometry&, double from, double to);
  void swap (const Geometry&, double from, double middle, double to);
  double soil_C (const Geometry& geometry) const;
  double soil_N (const Geometry& geometry) const;
  double goal_C_per_N (unsigned int at) const; // Desired C/N ratio.

public:
  void tick (unsigned int size, const double* turnover_factor, 
	     const double* N_soil, double* N_used,
	     double* CO2, const vector<SMB*>& smb, const vector<SOM*>&som,
	     const vector<DOM*>& dom); // Used by SMB and SOM, but not AOM.
protected:
  void tock (unsigned int size, const double* rate,
	     double factor, double efficiency, 
	     const double* N_soil, double* N_used,
	     double* CO2, OM& om);

  // Utilities.
public:
  static double get_initial_C_per_N (const AttributeList&);

  // Create & Destroy.
public:
  void grow (unsigned int size);
protected:
  static void load_syntax (Syntax&, AttributeList&);
  OM (const AttributeList& al);
  ~OM ();
};

#endif // OM_H
