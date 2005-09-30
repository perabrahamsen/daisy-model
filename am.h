// am.h
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


#ifndef AM_H
#define AM_H

#include "librarian.h"
#include <vector>

class Geometry;
class Soil;
class Time;
class AOM;
class Treelog;
class VCheck;

class AM
{
  // Content.
  struct Implementation;
  Implementation& impl;
  friend class Implementation;

public:
  const AttributeList alist;	// Remember attributes for checkpoint.
  const symbol name;
  static const char *const description;
  void append_to (std::vector<AOM*>& added);
  symbol real_name () const;

  // Simulation.
public:
  void output (Log&) const;
  bool check (Treelog& err) const;
  void mix (const Geometry&, double from, double to, double penetration,
            double& tillage_N_top, double& tillage_C_top,
            std::vector<double>& tillage_N_soil,
            std::vector<double>& tillage_C_soil);
  void swap (const Geometry&, double from, double middle, double to,
             std::vector<double>& tillage_N_soil,
             std::vector<double>& tillage_C_soil);
  double total_C (const Geometry& geometry) const; // [g C/cm^2]
  double total_N (const Geometry& geometry) const; // [g N/cm^2]
  double C_at (unsigned int at) const;
  double N_at (unsigned int at) const;
  void pour (std::vector<double>& cc, std::vector<double>& nn);
  void add (double C, double N);// Add dead leafs.
  void add (const Geometry& geometry, AM& other); // Merge AOMs.
  void add (const Geometry&,	// Add dead roots.
	    double C, double N, 
	    const std::vector<double>& density);
  double top_C () const;	// [g C/cm^2]
  double top_N () const;	// [g N/cm^2]
  void multiply_top (double fraction);

  // Crop Locks.
  enum lock_type { Unlocked, Locked };
  void unlock ();		// Crop died.
  bool locked () const;		// Test if this AM can be safely removed.
  symbol crop_name () const;	// Name of locked crop.
  symbol crop_part_name () const; // Name of locked crop part.

  // Create and Destroy.
public:
  static const VCheck& check_om_pools ();
  // Initialization & Fertilizer.
  static AM& create (const AttributeList&, const Soil&);
  // Crop part.
  static AM& create (const Geometry&, const Time&,
		     const std::vector<AttributeList*>&,
		     symbol sort, symbol part, lock_type lock = Unlocked);
  void initialize (const Soil&);
  static const std::vector<AttributeList*>& default_AM ();
  static const AttributeList& default_root ();
  static double get_NO3 (const AttributeList&);	// [g N/cm^2]
  static double get_NH4 (const AttributeList&);	// [g N/cm^2]
  static double get_volatilization (const AttributeList&);	// [kg N/ha]
  static double get_DM (const AttributeList&);	// [Mg DM/ha]
  static double get_water (const AttributeList&);	// [mm]
  static void set_utilized_weight (AttributeList& am,
				   const double weight /* [kg N/ha] */);
  static double utilized_weight (const AttributeList& am); // [kg N/ha]
  static double second_year_utilization (const AttributeList& am); // [kg N/ha]

private:
  friend class AM_Syntax;
  AM (const AttributeList&);
public:
  virtual ~AM ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<AM>::Content* Librarian<AM>::content;
#endif

static Librarian<AM> AM_init ("am");

#endif // AM_H
