// crop.h
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


#ifndef CROP_H
#define CROP_H

#include "time.h"
#include "librarian.h"
#include <vector>

struct Log;
struct Time;
struct AttributeList;
struct Bioclimate;
struct PLF;
struct Library;
struct Syntax;
struct SoilWater;
struct Soil;
struct Geometry;
struct OrganicMatter;
struct SoilHeat;
struct SoilNH4;
struct SoilNO3;
struct Column;
struct Harvest;
struct AM;

class Crop 
{
  // Content.
public:
  const AttributeList alist;	// Remember attributes for checkpoint.
  const symbol name;
  static const char *const description;

  // Communication with Bioclimate.
public:
#if 0
  virtual double water_stress () const = 0;	// [0-1] (0 = full production)
  virtual double nitrogen_stress () const = 0; // [0-1] (1 = no production)
  virtual double rs_min () const; // Minimum transpiration resistance.
  virtual double rs_max () const; // Maximum transpiration resistance.
#endif
  virtual double height () const = 0;
  virtual double LAI () const = 0;
  virtual double SimLAI () const;
  virtual const PLF& LAIvsH () const = 0;
  virtual double PARext () const = 0;
  virtual double PARref () const = 0;
  virtual double EPext () const = 0;
  virtual double IntcpCap () const = 0; // Interception Capacity.
  virtual double EpFac () const = 0; // Convertion to potential evapotransp.
  virtual double albedo () const;
  virtual void CanopyStructure () = 0;
  virtual double ActualWaterUptake (double Ept, const Soil&, SoilWater&, 
				    double EvapInterception, 
				    double day_fraction, Treelog&) = 0;
  virtual void force_production_stress  (double pstress);

  // Simulation.
public:
  virtual void tick (const Time& time, const Bioclimate&, const Soil&,
		     OrganicMatter*, const SoilHeat&, const SoilWater&,
		     SoilNH4*, SoilNO3*, 
		     double& residuals_DM,
		     double& residuals_N_top, double& residuals_C_top,
		     vector<double>& residuals_N_soil, 
		     vector<double>& residuals_C_soil,
		     double ForcedCAI,
		     Treelog&) = 0;
  virtual const Harvest& harvest (symbol column_name,
				  const Time&, const Geometry&, 
				  Bioclimate& bioclimate,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest,
				  bool kill_off,
				  vector<AM*>& residuals,
				  double& residuals_DM,
				  double& residuals_N_top,
				  double& residuals_C_top,
				  vector<double>& residuals_N_soil,
				  vector<double>& residuals_C_soil,
				  Treelog&) = 0;
  void kill (symbol, const Time&, const Geometry&, Bioclimate&,
	     vector<AM*>& residuals, 
	     double& residuals_DM, 
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil, 
	     vector<double>& residuals_C_soil,
	     Treelog&);
  virtual void output (Log&) const = 0;
  
  // Queries.
public:
  static bool ds_remove (const Crop*);
  virtual double DS () const = 0; // Development stage, [-1:2] or DSremove.
  static const double DSremove;
  virtual double DM () const = 0; // Shoot dry matter, [kg DM/ha].
  virtual double total_N () const = 0; // N content [kg N/ha]

  // Create and Destroy.
public:
  virtual void initialize_organic (Treelog&, const Geometry&, OrganicMatter&);
  virtual void initialize_inorganic (Treelog&, const Geometry&) = 0;
protected:
  Crop (const AttributeList& al);
public:
  virtual ~Crop ();
};

#if !defined (__BORLANDC__)
EMPTY_TEMPLATE
Librarian<Crop>::Content* Librarian<Crop>::content;
#endif

static Librarian<Crop> Crop_init ("crop");

#endif // CROP_H
