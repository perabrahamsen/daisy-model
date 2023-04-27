// vegetation.h
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


#ifndef VEGETATION_H
#define VEGETATION_H

#include "model_derived.h"
#include "symbol.h"
#include <vector>

class Log;
class Time;
class Weather;
class Bioclimate;
class Geometry;
class Soil;
class OrganicMatter;
class SoilHeat;
class SoilWater;
class Chemistry;
class PLF;
class Harvest;
class AM;
class Treelog;
class BlockModel;
class FrameModel;
class Units;
class Crop;
class Scope;

class Vegetation : public ModelDerived
{ 
  // Content.
public:
  static const char *const component;
  symbol library_id () const;
  const double EpInterchange_;  // Soil to canopy pot-.evap. interchange.
  static symbol all_crops ();         // Symbol matching all crops on field.

  // Canopy queries.
public:
  virtual double shared_light_fraction () const;
  virtual double rs_min () const = 0; // Minimum transpiration resistance [s/m].
  virtual double rs_max () const = 0; // Maximum transpiration resistance [s/m].
  virtual double shadow_stomata_conductance () const = 0; // Actual conductance [m/s].
  virtual double sunlit_stomata_conductance () const = 0; // Actual conductance [m/s].
  virtual double leaf_width () const = 0; // Leaf width [cm].
  virtual double N () const = 0; // Nitrogen content of vegetation [kg N/ha]
  virtual double N_fixated () const = 0; // Nitrogen fixation rate [kg N/ha/h]
  virtual double LAI () const = 0; // Total LAI of all crops [0-]
  virtual double height () const = 0; // Max crop height in canopy [cm]
  virtual double cover () const = 0; // Fraction of soil covered by crops [0-1]
  virtual const PLF& LAIvsH () const = 0; // LAI below height [f: cm -> R]
  virtual const PLF& HvsLAI () const = 0; // Height with LAI below [f: R -> cm]
  virtual double ACExt_PAR () const = 0; // Canopy extinction coefficient of PAR
  virtual double ACRef_PAR () const = 0; // Canopy reflection coefficient of PAR
  virtual double ACExt_NIR () const = 0; // Canopy extinction coefficient of NIR
  virtual double ACRef_NIR () const = 0; // Canopy reflection coefficient of NIR
  virtual double ARExt () const = 0;	  // Radiation Extinction coefficient
  virtual double EpFactorDry () const = 0;   // Reference to pot. evapotransp
  virtual double EpFactorWet () const = 0;   // Reference to pot. evapotransp
  double EpInterchange () const;          // Soil to canopy exchange rate.
  virtual double albedo () const = 0;	  // Another reflection factor
  virtual double interception_capacity () const = 0;// Canopy water cap. [mm]

  // Individual crop queries.
public:
  virtual double DS_by_name (symbol name) const = 0;// [-1:2]/DSremove
  virtual double stage_by_name (symbol name) const = 0; // Phenological stage
  virtual double DM_by_name (symbol name, double height) const = 0;// Shoot DM [kg/ha]
  virtual double SOrg_DM_by_name (symbol name) const = 0;// SOrg DM [kg/ha]
  virtual std::string crop_names () const = 0;
  virtual const std::vector<double>& effective_root_density () const = 0;
  virtual const std::vector<double>& effective_root_density (symbol name) const = 0;
  

  // Simulation
public:
  virtual double transpiration (// Actual trans. [mm/h]
				double potential_transpiration,	
				double canopy_evaporation,
                                const Geometry& geo,
				const Soil& soil, const SoilWater& soil_water,
				double dt, Treelog&) = 0;
  virtual void find_stomata_conductance (const Time& time, 
                                         const Bioclimate&,
                                         double dt, Treelog&) = 0;
  virtual void tick (const Scope&, const Time&, const Bioclimate&, 
                     const Geometry&, const Soil&, const SoilHeat&,
                     SoilWater&, Chemistry&, OrganicMatter&,
                     double& residuals_DM,
                     double& residuals_N_top, double& residuals_C_top,
                     std::vector<double>& residuals_N_soil,
                     std::vector<double>& residuals_C_soil,
                     double dt, Treelog&) = 0;
  virtual void clear () = 0;
  virtual void force_production_stress  (double pstress) = 0;
  virtual void kill_all (symbol, const Time&, const Geometry&,
			 std::vector<AM*>& residuals, 
			 double& residuals_DM,
			 double& residuals_N_top, double& residuals_C_top,
			 std::vector<double>& residuals_N_soil,
			 std::vector<double>& residuals_C_soil,
			 Treelog&) = 0;
  virtual void emerge (symbol crop_name, Treelog&) = 0;
  virtual void harvest (symbol column_name,
			symbol crop_name,
			const Time&, const Geometry&, 
			double stub_length,
			double stem_harvest,
			double leaf_harvest, 
			double sorg_harvest,
			std::vector<const Harvest*>& harvest,
                        double& min_height,
			double& yield_DM, double& yield_N,
			double& harvest_DM, double& harvest_N,
			double& harvest_C,
			std::vector<AM*>& residuals,
			double& residuals_DM,
			double& residuals_N_top, double& residuals_C_top,
			std::vector<double>& residuals_N_soil,
			std::vector<double>& residuals_C_soil,
                        const bool combine,
			Treelog&) = 0;
  virtual void pluck (symbol column_name,
                      symbol crop_name,
                      const Time&, const Geometry&, 
                      double stem_harvest,
                      double leaf_harvest, 
                      double sorg_harvest,
                      std::vector<const Harvest*>& harvest,
		      double& yield_DM, double& yield_N,
                      double& harvest_DM, double& harvest_N,
		      double& harvest_C,
                      std::vector<AM*>& residuals,
                      double& residuals_DM,
                      double& residuals_N_top,
                      double& residuals_C_top,
                      std::vector<double>& residuals_N_soil,
                      std::vector<double>& residuals_C_soil,
                      Treelog&) = 0;
  virtual void sow (const Scope&, const FrameModel& al, 
                    double row_width /* [cm] */, double row_pos /* [cm] */, 
                    double seed /* kg w.w./ha */,
                    const Geometry&, const Soil&, OrganicMatter&, 
                    double& seed_N /* [kg N/ha] */,
                    double& seed_C /* [kg C/ha] */,
                    const Time&, Treelog& msg) = 0;
  virtual void sow (const Scope&, Crop&, 
                    double row_width /* [cm] */, double row_pos /* [cm] */, 
                    double seed /* kg w.w./ha */,
                    const Geometry&, const Soil&, OrganicMatter&, 
                    double& seed_N /* [kg N/ha] */,
                    double& seed_C /* [kg C/ha] */,
                    const Time&, Treelog& msg) = 0;
  virtual void output (Log&) const;
  
  // Create and Destroy.
public:
  virtual void initialize (const Scope&, const Time&, const Geometry& geo,
                           const Soil& soil, OrganicMatter&,
                           Treelog&) = 0;
  virtual bool check (const Scope&, const Geometry&, Treelog& msg) const = 0;
  explicit Vegetation (const BlockModel&);
  ~Vegetation ();
};

#endif // VEGETATION_H
