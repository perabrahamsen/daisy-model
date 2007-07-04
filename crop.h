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
#include "alist.h"
#include "model.h"
#include <vector>

class Log;
class Weather;
class Time;
class AttributeList;
class Bioclimate;
class Raddist;
class PLF;
class Library;
class Syntax;
class SoilWater;
class Soil;
class Geometry;
class OrganicMatter;
class SoilHeat;
class SoilNH4;
class SoilNO3;
class Column;
class Harvest;
class AM;
class Treelog;
class Block;

class Crop : public Model 
{
  // Content.
public:
  const AttributeList alist;	// Remember attributes for checkpoint.
  const symbol name;
  static const char *const component;

  // Communication with Bioclimate.
public:
  virtual double minimum_light_fraction () const;
#if 0
  virtual double water_stress () const = 0;	// [0-1] (0 = full production)
  virtual double nitrogen_stress () const = 0; // [0-1] (1 = no production)
#endif
  virtual double rs_min () const; // Minimum transpiration resistance.
  virtual double rs_max () const; // Maximum transpiration resistance.
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
  virtual double ActualWaterUptake (double Ept, const Geometry& geo,
                                    const Soil&, SoilWater&, 
				    double EvapInterception, 
				    double day_fraction, 
                                    double dt, Treelog&) = 0;
  virtual void force_production_stress  (double pstress);

  // Simulation.
public:

  virtual void tick (const Time& time, double relative_humidity,
		     const double CO2_atm,
                     const Bioclimate&, 
		     const Geometry& geo, const Soil&, OrganicMatter&, 
		     const SoilHeat&, const SoilWater&,
		     SoilNH4&, SoilNO3&, 
		     double& residuals_DM,
		     double& residuals_N_top, double& residuals_C_top,
		     std::vector<double>& residuals_N_soil, 
		     std::vector<double>& residuals_C_soil,
		     double ForcedCAI,
		     double dt, Treelog&) = 0;
  virtual void emerge () = 0;
  virtual const Harvest& harvest (symbol column_name,
				  const Time&, const Geometry&, 
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest,
				  bool kill_off,
				  std::vector<AM*>& residuals,
				  double& residuals_DM,
				  double& residuals_N_top,
				  double& residuals_C_top,
				  std::vector<double>& residuals_N_soil,
				  std::vector<double>& residuals_C_soil,
                                  bool combine,
				  Treelog&) = 0;
  void kill (symbol, const Time&, const Geometry&,
	     std::vector<AM*>& residuals, 
	     double& residuals_DM, 
	     double& residuals_N_top, double& residuals_C_top,
	     std::vector<double>& residuals_N_soil, 
	     std::vector<double>& residuals_C_soil,
	     Treelog&);
  virtual double sorg_height () const = 0; // [cm]
  virtual void output (Log&) const = 0;
  
  // Queries.
public:
  static bool ds_remove (const Crop*);
  virtual double DS () const = 0; // Development stage, [-1:2] or DSremove.
  static const double DSremove;
  virtual double DM (double height) const = 0; // Shoot dry matter, [kg DM/ha].
  virtual double total_N () const = 0; // N content [kg N/ha]
  virtual double total_C () const = 0; // C content [kg C/ha]

  // Create and Destroy.
public:
  virtual void initialize (const Geometry&, OrganicMatter&, 
                           const Time& now, Treelog&) = 0;
protected:
  Crop (Block&);
public:
  ~Crop ();
};

#endif // CROP_H
