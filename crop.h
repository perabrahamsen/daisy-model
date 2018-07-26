// crop.h -- Stuff that grows on fields.
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

#include "model_framed.h"
#include "time.h"
#include <vector>

class Log;
class Weather;
class Time;
class Bioclimate;
class Raddist;
class PLF;
class Library;
class SoilWater;
class Soil;
class Geometry;
class OrganicMatter;
class Chemistry;
class SoilHeat;
class Column;
class Harvest;
class AM;
class Treelog;
class BlockModel;
class Metalib;
class VCheck;
class Scope;

class Crop : public ModelFramed
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Communication with Bioclimate.
public:
  virtual double minimum_light_fraction () const;
  virtual double rs_min () const; // Minimum transpiration resistance [s/m].
  virtual double rs_max () const; // Maximum transpiration resistance [s/m].
  virtual double shadow_stomata_conductance () const; // [m/s FIELD]
  virtual double sunlit_stomata_conductance () const; // [m/s FIELD]
  virtual double leaf_width () const; // Leaf width 
  virtual double height () const = 0;
  virtual double LAI () const = 0;
  virtual double SimLAI () const;
  virtual const PLF& LAIvsH () const = 0;
  virtual double PARext () const = 0;
  virtual double PARref () const = 0;
  virtual double NIRext () const = 0;
  virtual double NIRref () const = 0;
  virtual double EPext () const = 0;
  virtual double IntcpCap () const = 0; // Interception Capacity.
  virtual double EpFacDry () const = 0; // Convertion to potential evapotransp.
  virtual double EpFacWet () const = 0; // Convertion to potential evapotransp.
  virtual double albedo () const;
  virtual void CanopyStructure () = 0;
  virtual double ActualWaterUptake (double Ept, const Geometry& geo,
                                    const Soil&, const SoilWater&, 
				    double EvapInterception, 
                                    double dt, Treelog&) = 0;
  virtual void force_production_stress  (double pstress);

  // Simulation.
public:
  virtual void find_stomata_conductance (const Time& time, 
                                         const Bioclimate&,
                                         double dt, Treelog&);
  virtual void tick (const Scope&,
                     const Time&, const Bioclimate&, double ForcedCAI, 
                     const Geometry&, const Soil&, const SoilHeat&,
                     SoilWater&, Chemistry&, OrganicMatter&,
                     double& residuals_DM,
                     double& residuals_N_top, double& residuals_C_top,
                     std::vector<double>& residuals_N_soil,
                     std::vector<double>& residuals_C_soil,
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
  virtual const Harvest& pluck (const symbol column_name,
                                const Time& time,
                                const Geometry& geometry,
                                const double stem_harvest,
                                const double leaf_harvest,
                                const double sorg_harvest,
                                std::vector<AM*>& residuals,
                                double& residuals_DM,
                                double& residuals_N_top,
                                double& residuals_C_top,
                                std::vector<double>& residuals_N_soil,
                                std::vector<double>& residuals_C_soil,
                                Treelog& msg);
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
  virtual double SOrg_DM () const = 0;         // SOrg dry matter [kg DM/ha].
  virtual double N_fixated () const = 0; // N content [kg N/ha/h]
  virtual double total_N () const = 0; // N content [kg N/ha]
  virtual double total_C () const = 0; // C content [kg C/ha]
  virtual const std::vector<double>& root_density () const = 0;

  // Create and Destroy.
public:
  static const VCheck& check_all ();
  static const VCheck& check_library ();
  static const VCheck& check_buildable ();
  virtual void initialize (const Scope&, const Geometry&, OrganicMatter&, 
                           double SoilLimit, const Time& now, Treelog&) = 0;
  virtual void initialize (const Scope&, const Geometry&,
                           double row_width, double row_pos, double seed,
                           OrganicMatter&, 
                           double SoilLimit, const Time& now, Treelog&) = 0;
  virtual bool check (const Scope&, const Geometry&, Treelog& msg) const = 0;
protected:
  Crop (const BlockModel&);
public:
  ~Crop ();
};

#endif // CROP_H
