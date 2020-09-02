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

#include "model_framed.h"
#include "im.h"
#include <vector>
#include <memory>
#include <boost/shared_ptr.hpp>

class Log;
class Geometry;
class Volume;
class Time;
class AOM;
class Treelog;
class VCheck;
class Units;
class Frame;
class FrameModel;
class Metalib;

class AM : public ModelFramed
{
  // Content.
public:
  struct Implementation;
  std::unique_ptr<Implementation> impl;
  friend struct Implementation;
  static const char *const component;
  symbol library_id () const;
  void append_to (std::vector<AOM*>& added);
  symbol real_name () const;
  static bool compare_CN (const AM* a, const AM* b);
  
  // Simulation.
public:
  void output (Log&) const;
  bool check (Treelog& err) const;
  void mix (const Geometry&, double from, double to, double penetration,
            double& tillage_N_top, double& tillage_C_top,
            std::vector<double>& tillage_N_soil,
            std::vector<double>& tillage_C_soil);
  void mix (const Geometry&, const Volume&, double penetration,
            double& tillage_N_top, double& tillage_C_top,
            std::vector<double>& tillage_N_soil,
            std::vector<double>& tillage_C_soil);
  void swap (const Geometry&, double from, double middle, double to,
             std::vector<double>& tillage_N_soil,
             std::vector<double>& tillage_C_soil);
  double total_C (const Geometry& geometry) const; // [g C/cm^2]
  double total_N (const Geometry& geometry) const; // [g N/cm^2]
  double C_at (size_t at) const;
  double N_at (size_t at) const;
  void pour (std::vector<double>& cc, std::vector<double>& nn);
  void add (double C, double N);// Add dead leafs.
  void add (const Geometry& geometry, AM& other); // Merge AOMs.
  void add_surface (const Geometry&,	// Add dead roots.
                    double C, double N, 
                    const std::vector<double>& density);
  double top_DM () const;	// [g DM/cm^2]
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
  static AM& create (const Metalib&, const FrameModel&, const Geometry&, 
                     const Time&, Treelog& msg);
  // Crop part.
  static AM& create (const Metalib&, const Geometry&, const Time&,
		     const std::vector<boost::shared_ptr<const FrameModel>/**/>&,
		     symbol sort, symbol part, lock_type lock, Treelog& msg);
  void initialize (const Geometry&, const double max_rooting_depth);
  virtual void initialize_derived (const Geometry&, 
                                   const double max_rooting_depth) = 0;
  static const std::vector<symbol>& default_AM ();

  static double get_NO3 (const Metalib&, const FrameModel&);	// [g N/cm^2]
  static double get_NH4 (const Metalib&, const FrameModel&);	// [g N/cm^2]
  static IM get_IM (const Metalib&, const Unit&, const FrameModel&);
  static double get_volatilization (const Metalib&,
                                    const FrameModel&);	// [g N/m^2]
  static double get_DM (const Metalib&, const FrameModel&);	// [Mg DM/ha]
  static double get_water (const Metalib&, const FrameModel&);	// [mm]
  static void set_utilized_weight (const Metalib&, FrameModel& am,
				   const double weight /* [kg N/ha] */);
  static double utilized_weight (const Metalib&,
                                 const FrameModel& am); // [kg N/ha]
  static double second_year_utilization (const Metalib&,
                                         const FrameModel& am); // [kg N/ha]
  static void set_mineral (const Metalib&, FrameModel&, 
                           double NH4, double NO3);// [kg N/ha]

  static bool is_fertilizer (const Metalib&, const FrameModel&);
  static bool is_mineral (const Metalib&, const FrameModel&);
  static bool is_organic (const Metalib&, const FrameModel&);
protected:
  AM (const BlockModel&);
public:
  virtual ~AM ();
};

#endif // AM_H
