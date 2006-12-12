// root_system.h -- Root development and uptake.
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

#ifndef ROOT_SYSTEM_H
#define ROOT_SYSTEM_H

#include "rootdens.h"
#include "plf.h"
#include <vector>
#include <memory>

struct Geometry;
struct Soil;
struct SoilWater;
struct Solute;
struct SoilNO3;
struct SoilNH4;

struct Log;
struct AttributeList;
struct Syntax;

class RootSystem
{
  // Components.
private:
  std::auto_ptr<Rootdens> rootdens; // Root density calculation.

  // Parameters.
private:
  const double PenPar1;		// Penetration rate parameter, coefficient
  const double PenPar2;		// Penetration rate parameter, threshold
  const PLF PenClayFac;		// Clay influence on penetration, factor.
  const double MaxPen;		// Max penetration depth
  const double Rad;		// Root radius [cm]
  const double h_wp;		// Matrix potential at wilting
  const double MxNH4Up;		// Max NH4 uptake per unit root length
  const double MxNO3Up;		// Max NO3 uptake per unit root length
  const double Rxylem;		// Transport resistence in xyleme

  // State.
private:
  double PotRtDpt;	        // Potential Root Penetration Depth [cm]
public:
  double Depth;			// Rooting Depth [cm]
  std::vector<double> Density;	// Root density [cm/cm3] in soil layers
private:
  std::vector<double> H2OExtraction; // Extraction of H2O in soil [cm³/cm³/h]
  std::vector<double> NH4Extraction; // Extraction of NH4-N in soil [gN/cm³/h]
  std::vector<double> NO3Extraction; // Extraction of NH4-N in soil [gN/cm³/h]
  double h_x;			// Root extraction at surface.
public:
  double partial_soil_temperature; // Accumaleted soil temperature. [°C]
  double soil_temperature;	// Soil temperature previous day. [°C]

  // Log.
public:
  double water_stress;		// Fraction of requested water we didn't got.
  double water_stress_days;	// Accumulated water stress.
  double production_stress;	// SVAT induced stress, -1 if not applicable.
 
private:
  double Ept;			// Potential evapotranspiration.
  double H2OUpt;		// H2O uptake [mm/h]
  double NH4Upt;		// NH4-N uptake [g/m2/h]
  double NO3Upt;		// NO3-N uptake [g/m2/h]

  // Uptake.
private:
  double potential_water_uptake (double h_x,
                                 const Geometry&,
				 const Soil& soil,
				 const SoilWater& soil_water,
                                 double dt);
public:
  double water_uptake (double Ept,
                       const Geometry&,
		       const Soil& soil, SoilWater& soil_water,
                       double EvapInterception, double day_fraction, double dt,
		       Treelog&);
private:
  double solute_uptake (const Geometry&, 
                        const Soil&, const SoilWater&, Solute&,
			double PotNUpt, std::vector<double>& uptake,
			double i_max, double C_root_min, double dt);
public:
  double nitrogen_uptake (const Geometry&,
                          const Soil& soil,
			  const SoilWater& soil_water,
			  SoilNH4& soil_NH4,
			  double NH4_root_min,
			  SoilNO3& soil_NO3,
			  double NO3_root_min,
			  double PotNUpt,
                          double dt);

  // Simulation.
private:
  static double density_distribution_parameter (double a);
public:
  void tick_hourly (int hour, double T);
  void tick_daily (Treelog&, const Geometry&, const Soil&, 
		   double WRoot, double IncWRoot, double DS);
  void set_density (Treelog&,
		    const Geometry& geometry, double WRoot, double DS);
  void full_grown (Treelog&, const Geometry&, 
                   double max_rooting_depth, double WRoot);
  void output (Log& log) const;

  // Create and Destroy
public:
  void initialize (size_t size);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  RootSystem (Block& al);
  ~RootSystem ();
};

#endif // ROOT_SYSTEM_H
