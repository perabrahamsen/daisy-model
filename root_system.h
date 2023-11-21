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

#include "plf.h"
#include "rootdens.h"
#include <vector>
#include <memory>

class Frame;
class Geometry;
class Soil;
class SoilHeat;
class SoilWater;
class Chemistry;
class Log;
class Metalib;
class Units;
class Block;
class Rootdens;
class ABAProd;
class Solupt;
class Treelog;
class RootSystem
{
  const Metalib& metalib;
  // Components.
private:
  std::unique_ptr<Rootdens> rootdens; // Root density calculation.
  std::unique_ptr<ABAProd> ABAprod;   // Root density calculation.
  std::unique_ptr<Solupt> NH4_uptake; // Ammonium uptake.
  std::unique_ptr<Solupt> NO3_uptake; // Nitrate uptake.

  // Parameters.
private:
  const double PenPar1;		// Penetration rate parameter, coefficient
  const double PenPar2;		// Penetration rate parameter, threshold
  const PLF PenpFFac;		// Mousture  influence on penetration, factor.
  const PLF PenClayFac;		// Clay influence on penetration, factor.
  const PLF PenWaterFac;	// Water influence on penetration, factor.
  const PLF PenDSFac;	// Development stage influence on penetration, factor.
  const PLF DensityDSFac;	// DS influence on homogeneity [DS] -> []
  const double MaxPen;		// Max penetration depth
  const double MaxWidth;        // Max horizontal distance from plant
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
private:
  std::vector<double> Density;	// Root density [cm/cm3] in soil layers
  std::vector<double> EffectiveDensity;	// Effective root density [cm/cm3]
public:
  const std::vector<double>& actual_density () const
  { return Density; }
  const std::vector<double>& effective_density () const
  { return EffectiveDensity; }
  const std::vector<double>& dynamic_root_death () const // [cm/cm^3/h]
  { return rootdens->dynamic_root_death (); }
  double dynamic_root_death_DM () const // [g DM/h]
  { return rootdens->dynamic_root_death_DM (); }
private:
  std::vector<double> H2OExtraction; // Extraction of H2O in soil [cm³/cm³/h]
  std::vector<double> NH4Extraction; // Extraction of NH4-N in soil [gN/cm³/h]
  std::vector<double> NO3Extraction; // Extraction of NH4-N in soil [gN/cm³/h]
  std::vector<double> ABAExtraction; // Extraction of ABA in soil [g ABA/cm³/h]
public:
  double ABAConc;		// ABA concentration in uptake [g/cm^3]
private:
  double h_x;			// Root extraction at surface
public:
  double partial_soil_temperature; // Accumaleted soil temperature [°C]
  double partial_day;           // Accuumalted time [h]
  double soil_temperature;	// Soil temperature previous day [°C]

  // Log.
public:
  double water_stress;		// Fraction of requested water we didn't got
  double water_stress_days;	// Accumulated water stress
  double production_stress;	// SVAT induced stress, -1 if not applicable
 
private:
  double Ept;			// Potential evapotranspiration
  double H2OUpt;		// H2O uptake [mm/h]
  double NH4Upt;		// NH4-N uptake [g/m2/h]
  double NO3Upt;		// NO3-N uptake [g/m2/h]

public:
  double crown_potential () const; // [cm]

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
		       const Soil& soil, const SoilWater& soil_water,
                       double EvapInterception, double dt, Treelog&);
public:
  double nitrogen_uptake (const Geometry&,
                          const Soil& soil,
			  const SoilWater& soil_water,
			  Chemistry& chemistry,
			  double NH4_root_min,
			  double NO3_root_min,
			  double PotNUpt);

  // Simulation.
private:
  static double density_distribution_parameter (double a);
public:
  void tick_dynamic (const Geometry& geo, const SoilHeat&, SoilWater&,
		     const double day_fraction, const double dt, Treelog&);
  void tick_daily (const Geometry&, const Soil&, const SoilWater&,
		   double WRoot, bool root_growth, double DS, Treelog&);
  void set_density (const Geometry& geometry, const Soil& soil,
                    double WRoot, double DS, Treelog&);
  void full_grown (const Geometry&, const Soil&, double WRoot, Treelog&);
  void output (Log& log) const;

  // Create and Destroy
public:
  void initialize (const Geometry& geo, const Soil& soil, 
                   double row_width, double row_pos, const double DS,
		   Treelog& msg);
  void initialize (const Geometry& geo, const Soil&, const double DS,
		   Treelog& msg);
  bool check (const Geometry& geo, Treelog& msg) const;
  static void load_syntax (Frame&);
  RootSystem (const Block& al);
  ~RootSystem ();
};

#endif // ROOT_SYSTEM_H
