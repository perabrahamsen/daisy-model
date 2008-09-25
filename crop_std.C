// crop_std.C
// 
// Copyright 1996-2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2004 KVL.
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

#define BUILD_DLL
#include "crop.h"
#include "chemistry.h"
#include "seed.h"
#include "root_system.h"
#include "canopy_std.h"
#include "harvesting.h"
#include "production.h"
#include "phenology.h"
#include "partition.h"
#include "vernalization.h"
#include "photo.h"
#include "crpn.h"
#include "wse.h"
#include "log.h"
#include "time.h"
#include "timestep.h"
#include "bioclimate.h"
#include "plf.h"
#include "soil_water.h"
#include "geometry.h"
#include "soil.h"
#include "organic_matter.h"
#include "soil_heat.h"
#include "am.h"
#include "submodeler.h"
#include "mathlib.h"
#include "librarian.h"
#include "memutils.h"
#include "check.h"
#include <sstream>
#include <numeric>

struct CropStandard : public Crop
{
  // Content.
  const double initial_weight;  // [g w.w./m^2]
  const std::auto_ptr<Seed> seed;
  const std::auto_ptr<RootSystem> root_system;
  CanopyStandard canopy;
  std::auto_ptr<Harvesting> harvesting;
  Production production;
  std::auto_ptr<Time> last_time;
  std::auto_ptr<Phenology> development;
  const Partition partition;
  Vernalization vernalization;
  const std::auto_ptr<Photo> photo;
  CrpN nitrogen;
  const std::auto_ptr<WSE> water_stress_effect;
  const bool enable_N_stress;
  const double min_light_fraction;

  // Communication with Bioclimate.
  double minimum_light_fraction () const
  { return min_light_fraction; }

  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }
  double stomata_conductance () const // Current stomata_conductance [m/s].
  {
    // Stomata conductance
    const double gs = photo->stomata_conductance();//[m s^-1]
    if(gs < 0)
      return 1.0/rs_min(); // Photo_GL have no stomata conductance.
    return gs; 
  }                     
  double leaf_width () const
  { return canopy.leaf_width (DS ()); }

  double height () const	// Crop height [cm]
  { return canopy.Height; }
  double LAI () const
  { return canopy.CAI; }
  double SimLAI () const
  { return canopy.SimCAI; }
  const PLF& LAIvsH () const
  { return canopy.LAIvsH; }
  double PARext () const
  { return canopy.PARext; }
  double PARref () const
  { return canopy.PARref; }
  double NIRext () const
  { return canopy.PARext; }
  double NIRref () const
  { return canopy.PARref; }
  double EPext () const
  { return canopy.EPext; }
  double IntcpCap () const	// Interception Capacity.
  { return canopy.IntcpCap; }
  double EpFac () const		// Convertion to potential evapotransp.
  { return canopy.EpFactor (DS ()); }
  void CanopyStructure ()
  { canopy.CanopyStructure (development->DS); }
  double ActualWaterUptake (const Units& units, double Ept, 
                            const Geometry& geo,
			    const Soil& soil, const SoilWater& soil_water,
			    const double EvapInterception, const double dt, 
			    Treelog& msg)
  { return root_system->water_uptake (units, Ept, geo, soil, soil_water, 
                                      EvapInterception, 
                                      dt, msg); }
  void force_production_stress  (double pstress)
  { root_system->production_stress = pstress; }

  // Simulation.
  void find_stomata_conductance (const Units&, const Time& time, 
                                 const Bioclimate&, double dt, Treelog&);
  void tick (const Time& time, const Bioclimate&, double ForcedCAI,
             const Geometry& geo, const Soil&, const SoilHeat&,
             SoilWater&, Chemistry&, OrganicMatter&,
             double& residuals_DM,
             double& residuals_N_top, double& residuals_C_top,
             std::vector<double>& residuals_N_soil,
             std::vector<double>& residuals_C_soil,
             double dt, Treelog&);
  void emerge ()
  { development->DS = -1e-10; }
  const Harvest& harvest (symbol column_name,
			  const Time&, const Geometry&,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off,
			  std::vector<AM*>& residuals,
			  double& residuals_DM,
			  double& residuals_N_top, double& residuals_C_top,
			  std::vector<double>& residuals_N_soil,
			  std::vector<double>& residuals_C_soil,
                          const bool combine,
			  Treelog&);
  const Harvest& pluck (const symbol column_name,
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
  
  double sorg_height () const 
  { return harvesting->sorg_height; }
  void output (Log&) const;

  // Queries.
  double DS () const
  { return development->DS; }
  double DM (double height) const;
  double SOrg_DM () const;
  double total_N () const
  { return production.total_N (); }
  double total_C () const
  { return production.total_C (); }
  const std::vector<double>& root_density () const
  { return root_system->Density; }

  // Create and Destroy.
  void initialize (const Units&, 
                   const Geometry& geometry, double row_width, OrganicMatter&, 
                   double SoilLimit,
                   const Time&, Treelog&);
  void initialize (const Units&, const Geometry&, OrganicMatter&, 
                   double SoilLimit, const Time&, Treelog&);
  void initialize_shared (const Geometry&, OrganicMatter&, 
                          double SoilLimit, const Time&, Treelog&);
  bool check (const Units&, Treelog&) const;
  CropStandard (Block& vl);
  ~CropStandard ();
};

double 
CropStandard::DM (const double height) const
{
  const double stem_harvest = bound (0.0, (1.0 - height / canopy.Height), 1.0);
  const double leaf_harvest = (canopy.CAI > 0.0 && height < canopy.Height)
    ? bound (0.0, (1.0 - canopy.LAIvsH (height)  / canopy.CAI), 1.0)
    : 0.0;
  const double sorg_harvest = (height < harvesting->sorg_height) ? 1.0 : 0.0;
  const double total = stem_harvest * (production.WStem + production.WDead)
    + leaf_harvest * production.WLeaf 
    + sorg_harvest * production.WSOrg;

#if 0
  std::ostringstream tmp;
  tmp << "height = " << height << ", CAI (height) = "  
      << ((leaf_harvest > 0.0) ? canopy.LAIvsH (height) : 0.0 )
      << ", CAI = " << canopy.CAI 
      << ", leaf_harvest = " << leaf_harvest 
      << ", sorg_harvest = " << sorg_harvest << ", total = " << total * 10.0;
  Assertion::message (tmp.str ());
#endif

  return total * 10.0;          // [g/m^2 -> kg/ha]
}

double 
CropStandard::SOrg_DM () const
{ return production.WSOrg * 10.0 /* [g/m^2 -> kg/ha] */;}

void
CropStandard::initialize (const Units& units,
                          const Geometry& geo, const double row_width,
                          OrganicMatter& organic_matter,
                          const double SoilLimit,
                          const Time& now, Treelog& msg)
{
  root_system->initialize (units, geo, row_width, msg);
  initialize_shared (geo, organic_matter, SoilLimit, now, msg);
}

void
CropStandard::initialize (const Units& units, const Geometry& geo, 
                          OrganicMatter& organic_matter,
                          const double SoilLimit,
                          const Time& now, Treelog& msg)
{
  root_system->initialize (units, geo, msg);
  initialize_shared (geo, organic_matter, SoilLimit, now, msg);
}

void
CropStandard::initialize_shared (const Geometry& geo, 
                                 OrganicMatter& organic_matter,
                                 const double SoilLimit,
                                 const Time& now, Treelog& msg)
{
  if (!last_time.get ())
    last_time.reset (new Time (now));
  seed->initialize (initial_weight);
  production.initialize (seed->initial_N (initial_weight));

  const double DS = development->DS;
  if (DS >= 0)
    {
      // Dead organic matter.
      production.initialize (name, harvesting->Root, harvesting->Dead,
                             geo, organic_matter);
      
      // Update derived state content.
      const double WLeaf = production.WLeaf;
      const double SpLAI = canopy.specific_LAI (DS);
      const double seed_CAI = seed->forced_CAI (WLeaf, SpLAI, DS);
      canopy.tick (WLeaf, production.WSOrg, production.WStem, DS, 
		   // We don't save the forced CAI, use simulated CAI
		   //  until midnight (small error).
		   seed_CAI);
      root_system->set_density (geo, SoilLimit, production.WRoot, DS, msg);
      nitrogen.content (DS, production, msg);
    }
}

bool
CropStandard::check (const Units& units, Treelog& msg) const
{
  Treelog::Open nest (msg, library_id () + ": " + name);

  bool ok = true;
  if (!seed->check (msg))
    ok = false;
  if (!root_system->check (units, msg))
    ok = false;
  if (seed->initial_N (initial_weight) <= 0.0
      && production.NCrop <= 0.0)
    {
      ok = false;
      msg.error ("\
You must specify initial N content in either 'Prod' or 'Seed'");
    }
  return ok;
}

void
CropStandard::find_stomata_conductance (const Units& units, const Time& time,
                                        const Bioclimate& bioclimate,
                                        const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

  // Check age.
  const double DS = development->DS;
  if (DS <= 0.0 || development->mature ())
    return;
  
  // Clear data from previous iteration.
  photo->clear ();

  // Boundary conditions.
  const double relative_humidity = bioclimate.atmospheric_relative_humidity ();
  const double CO2_atm = bioclimate.atmospheric_CO2 ();
  const std::vector<double>& total_PAR = bioclimate.PAR (); 
  const std::vector<double>& sun_PAR = bioclimate.sun_PAR ();
  daisy_assert (sun_PAR.size () > 1);
  daisy_assert (total_PAR.size () == sun_PAR.size ());
  const double total_LAI = bioclimate.LAI ();
  const std::vector<double>& fraction_sun_LAI = bioclimate.sun_LAI_fraction ();
  const std::vector<double>& PAR_height = bioclimate.height ();

  // Calculate shadow PAR.
  std::vector<double> shadow_PAR;
  for(int i = 0; i < total_PAR.size (); i++) 
    shadow_PAR.push_back(total_PAR[i] - sun_PAR[i]);
      
  // Accumulate.
  std::vector<double> fraction_shadow_LAI;
  std::vector<double> fraction_total_LAI;
  double Ass = 0.0;

  // Loop.
  bool top_crop = true;  // True, if we haven't reached the top of the crop yet.
  const int No = fraction_sun_LAI.size ();
  for(int i = 0; i < No; i++) 
    {
      const double f_sun = fraction_sun_LAI[i]; 
      fraction_shadow_LAI.push_back(1.0-f_sun);
      fraction_total_LAI.push_back(1.0);
      daisy_assert (f_sun <= 1.0);
      daisy_assert (f_sun >= 0.0);

      const double height = PAR_height[i+1];
      daisy_assert (height < PAR_height[i]);

      if (top_crop && height <= canopy.Height)
        {
          // We count day hours at the top of the crop.
          top_crop = false;
          if (total_PAR[i] > 0.5 * 25.0)      //W/m2
            development->light_time (dt);
        }
    }

  // We use non-functional (Nf) and critical limits (Cr) to
  // estimate how much N is used in photosynthesis.  The
  // non-functional is considered structural N, and not used in
  // photosynthesis.  N content above critical is considered
  // luxury, and also not used in photosynthesis.
  const double N_at_Nf 
    = canopy.corresponding_WLeaf (DS) * nitrogen.NfLeafCnc (DS);
  const double N_at_Cr
    = canopy.corresponding_WLeaf (DS) * nitrogen.CrLeafCnc (DS);
  const double N_above_Nf = production.NLeaf - N_at_Nf;
  const double rubiscoN = bound (0.0, N_above_Nf, N_at_Cr - N_at_Nf);
  daisy_assert (rubiscoN >= 0.0);
      
  const double ABA_xylem = root_system->ABAConc;
  daisy_assert (std::isfinite (ABA_xylem));
  daisy_assert (ABA_xylem >= 0.0);
  
  const double T_canopy = bioclimate.canopy_temperature ();
  const double T_leaf_sun = bioclimate.sun_leaf_temperature ();
  const double T_leaf_shadow = bioclimate.shadow_leaf_temperature ();

  if (bioclimate.shared_light_fraction () > 1e-10)
    {
      // Shared light.
      Ass += photo->assimilate (units,
                                ABA_xylem, relative_humidity, CO2_atm,
                                bioclimate.daily_air_temperature(),
                                T_leaf_shadow,
                                rubiscoN, shadow_PAR, PAR_height,
                                total_LAI, fraction_shadow_LAI, dt,
                                canopy, *development, msg)
        * bioclimate.shared_light_fraction ();

      Ass += photo->assimilate (units,
                                ABA_xylem, relative_humidity, CO2_atm,
                                bioclimate.daily_air_temperature(),
                                T_leaf_sun,
                                rubiscoN, sun_PAR,  PAR_height,
                                total_LAI, fraction_sun_LAI, dt,
                                canopy, *development, msg)
        * bioclimate.shared_light_fraction ();
    }

  if (min_light_fraction > 1e-10)// Only total PAR, not sun shadow
    {
      // Private light.
      const int No = 30;
      std::vector<double> PAR (No + 1, 0.0);
      Bioclimate::radiation_distribution 
        (No, LAI (), PARref (), bioclimate.global_radiation (),
         PARext (), PAR); 
      Ass += photo->assimilate (units,
                                ABA_xylem, relative_humidity, CO2_atm,
                                bioclimate.daily_air_temperature (), 
                                bioclimate.canopy_temperature(),
                                rubiscoN, PAR, PAR_height,
                                bioclimate.LAI (), fraction_total_LAI, dt,
                                canopy, *development, msg)
        * min_light_fraction;
    }
  daisy_assert (std::isfinite (Ass));
  production.PotCanopyAss = Ass;
}

void
CropStandard::tick (const Time& time, const Bioclimate& bioclimate, 
                    const double ForcedCAI,
                    const Geometry& geo, const Soil& soil, 
                    const SoilHeat& soil_heat,
                    SoilWater& soil_water, Chemistry& chemistry,
                    OrganicMatter& organic_matter,
                    double& residuals_DM,
                    double& residuals_N_top, double& residuals_C_top,
                    std::vector<double>& residuals_N_soil,
                    std::vector<double>& residuals_C_soil,
                    const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

  // Update cut stress.
  harvesting->tick (time);

  // Update average soil temperature.
  const double day_fraction = bioclimate.day_fraction (dt);
  const double T_soil 
    = geo.content_height (soil_heat, &SoilHeat::T, -root_system->Depth);
  root_system->tick (T_soil, day_fraction, soil_water, dt);

  // Clear nitrogen.
  nitrogen.clear ();

  // Update age.
  development->DAP += dt/24.0;

  // New day?
  daisy_assert (last_time.get ());
  const Timestep daystep = time - *last_time;
  const bool new_day = (time.yday () != last_time->yday ()
                        || time.year () != last_time->year ());
  if (new_day)
    *last_time = time;

  // Emergence.
  const double& DS = development->DS;
  if (DS <= 0)
    {
      if (!new_day)
	return;

      daisy_assert (ForcedCAI < 0.0);

      const double h_middle 
        = geo.content_height (soil_water, &SoilWater::h,
                              -root_system->Depth/2.);
      development->emergence (h_middle, root_system->soil_temperature, 
                              daystep.total_hours ());
      if (DS >= 0)
	{
	  msg.message ("Emerging");
          const double WLeaf = production.WLeaf;
          const double SpLAI = canopy.specific_LAI (DS);
          const double seed_CAI = seed->forced_CAI (WLeaf, SpLAI, DS);
	  canopy.tick (production.WLeaf, production.WSOrg,
		       production.WStem, DS, seed_CAI);
	  nitrogen.content (DS, production, msg);
	  root_system->tick_daily (geo, soil, production.WRoot, 0.0,
                                   DS, msg);

	  static const symbol root_symbol ("root");
	  static const symbol dead_symbol ("dead");

          if (!production.AM_root)
            {
              production.AM_root
                = &AM::create (geo.cell_size (), time, harvesting->Root,
                               name, root_symbol, AM::Locked);
              organic_matter.add (*production.AM_root);
            }
          if (!production.AM_leaf)
            {
              production.AM_leaf
                = &AM::create (geo.cell_size (), time, harvesting->Dead,
                               name, dead_symbol, AM::Locked);
              organic_matter.add (*production.AM_leaf);
	    }
	  else
	    {
	      if (!production.AM_root)
		production.AM_root
		  = &AM::create (geo.cell_size (), time, harvesting->Root,
				 name, root_symbol, AM::Unlocked);
	      if (!production.AM_leaf)
		production.AM_leaf
		  = &AM::create (geo.cell_size (), time, harvesting->Dead,
				 name, dead_symbol, AM::Unlocked);
	    }
	}
      return;
    }
  if (development->mature ())
    return;

  daisy_assert (production.AM_root);
  daisy_assert (production.AM_leaf);
  
  nitrogen.update (production.NCrop, DS, 
                   geo, soil, soil_water, chemistry,
                   bioclimate.day_fraction (dt),
                   *root_system, dt);

  const double nitrogen_stress = nitrogen.nitrogen_stress;
  const double water_stress = root_system->water_stress;

  double Ass = production.PotCanopyAss;
  if (root_system->production_stress >= 0.0)
    Ass *= (1.0 - root_system->production_stress);
  else 
    Ass *= water_stress_effect->factor (water_stress);
  if (enable_N_stress)
    Ass *= (1.0 - nitrogen_stress);
  Ass *= (1.0 - harvesting->cut_stress);
  production.CanopyAss = Ass;
  daisy_assert (Ass >= 0.0);

  const double T_soil_3 
    = geo.content_height (soil_heat, &SoilHeat::T, -root_system->Depth/3.0);
  daisy_assert (std::isfinite (T_soil_3));
  const double seed_C = seed->release_C (dt);
  production.tick (bioclimate.daily_air_temperature (), T_soil_3,
		   root_system->Density, geo, DS, 
		   canopy.CAImRat, nitrogen, nitrogen_stress, seed_C, 
                   partition, 
		   residuals_DM, residuals_N_top, residuals_C_top,
		   residuals_N_soil, residuals_C_soil, dt, msg);
  nitrogen.content (DS, production, msg);
  if (!new_day)
    return;

  const double WLeaf = production.WLeaf;
  const double SpLAI = canopy.specific_LAI (DS);
  const double seed_CAI = seed->forced_CAI (WLeaf, SpLAI, DS);
  canopy.tick (WLeaf, production.WSOrg, production.WStem, 
               DS, ForcedCAI < 0.0 ? seed_CAI : ForcedCAI);

  development->tick_daily (bioclimate.daily_air_temperature (), 
                           production.leaf_growth (), production, 
                           vernalization, harvesting->cut_stress, msg);
  root_system->tick_daily (geo, soil, 
                           production.WRoot, production.root_growth (),
                           DS, msg);
  production.tick_daily ();
}

const Harvest&
CropStandard::harvest (const symbol column_name,
		       const Time& time,
		       const Geometry& geometry,
		       const double stub_length,
		       const double stem_harvest_frac,
		       const double leaf_harvest_frac,
		       const double sorg_harvest_frac,
		       const bool kill_off,
		       std::vector<AM*>& residuals,
		       double& residuals_DM,
		       double& residuals_N_top, double& residuals_C_top,
		       std::vector<double>& residuals_N_soil,
		       std::vector<double>& residuals_C_soil,
                       const bool combine,
		       Treelog& msg)
{
  Treelog::Open nest (msg, name + " harvest");

  // Update nitrogen content.
  nitrogen.content (development->DS, production, msg);

  // Leave stem and leaf below stub alone.
  double stem_harvest;
  double leaf_harvest;
  if (stub_length < canopy.Height)
    {
      stem_harvest = (1.0 - stub_length / canopy.Height);

      if (canopy.CAI > 0.0)
	{
	  const double stub_CAI = canopy.LAIvsH (stub_length);
	  leaf_harvest = (1.0 - stub_CAI / canopy.CAI);
	}
      else 
	leaf_harvest = 0.0;
    }
  else
    {
      stem_harvest = 0.0;
      leaf_harvest = 0.0;
    }

  const Harvest& harvest 
    = harvesting->harvest (column_name, name, 
                           root_system->Density,
                           time, geometry, production, development->DS,
                           stem_harvest, leaf_harvest, 1.0,
                           stem_harvest_frac, leaf_harvest_frac,
                           sorg_harvest_frac,
                           kill_off, residuals, residuals_DM,
                           residuals_N_top, residuals_C_top, 
                           residuals_N_soil, residuals_C_soil,
                           combine,
                           root_system->water_stress_days, 
                           nitrogen.nitrogen_stress_days);

  if (!approximate (development->DS, DSremove))
    {
      nitrogen.cut (development->DS); // Stop fixation.

      if (development->DS > 0.0)
	{
	  // Revert development.
	  if (harvesting->DSnew < 0.0)
	    {
	      // We want a cut to always put back development, even
	      // for a crop which is so retarded that it has no stem
	      // worth speaking about, and hence, no height.
	      const double DS_height = canopy.DS_at_height (stub_length);
	      if (development->DS > DS_height)
		development->DS = std::max (DS_height, 0.01);
	    }
	  else if (development->DS > harvesting->DSnew)
	    development->DS = harvesting->DSnew;
	  
	  // Cut canopy.
	  canopy.cut (production.WStem, development->DS, stub_length);

	  daisy_assert (approximate (canopy.CropHeight (production.WStem,
							development->DS), 
				     canopy.Height));
	  canopy.CropCAI (production.WLeaf, production.WSOrg,
			  production.WStem, development->DS);
	  if (LAI () > 0.0)
	    CanopyStructure ();
	  else
	    msg.warning ("No CAI after harvest");
	}
    }
  return harvest;
}

const Harvest&
CropStandard::pluck (const symbol column_name,
                     const Time& time,
                     const Geometry& geometry,
                     const double stem_harvest,
                     const double leaf_harvest,
                     const double sorg_harvest,
                     std::vector<AM*>& residuals,
                     double& residuals_DM,
                     double& residuals_N_top, double& residuals_C_top,
                     std::vector<double>& residuals_N_soil,
                     std::vector<double>& residuals_C_soil,
                     Treelog& msg)
{
  Treelog::Open nest (msg, "Plucking " + name);
  
  // Update nitrogen content.
  nitrogen.content (development->DS, production, msg);

  // Harvest.
  const Harvest& harvest 
    = harvesting->harvest (column_name, name, 
                           root_system->Density,
                           time, geometry, production, development->DS,
                           stem_harvest, leaf_harvest, sorg_harvest,
                           1.0, 1.0, 1.0,
                           false, residuals, residuals_DM,
                           residuals_N_top, residuals_C_top, 
                           residuals_N_soil, residuals_C_soil,
                           false,
                           root_system->water_stress_days, 
                           nitrogen.nitrogen_stress_days);

  // Phenology may be affected.
  if (!approximate (development->DS, DSremove))
    {
      nitrogen.cut (development->DS); // Stop fixation.

      if (development->DS > 0.0)
	{
	  // Revert development.
          if (harvesting->DSnew > 0.0 && development->DS > harvesting->DSnew)
	    development->DS = harvesting->DSnew;
	  
	  // Reset canopy.
	  canopy.CropCAI (production.WLeaf, production.WSOrg,
			  production.WStem, development->DS);
	  if (LAI () > 0.0)
	    CanopyStructure ();
	  else
	    msg.warning ("No canopy after harvest");
	}
    }
  return harvest;
}

void
CropStandard::output (Log& log) const
{
  output_derived (seed, "Seed", log);
  output_submodule (*root_system, "Root", log);
  output_submodule (canopy, "Canopy", log);
  output_submodule (*harvesting, "Harvest", log);
#if 1
  static const symbol Prod_symbol ("Prod");
  if (log.check_interior (Prod_symbol))
    {
      Log::Open open (log, Prod_symbol);
      production.output (log);
    }
#else
  output_submodule (production, "Prod", log);
#endif
  daisy_assert (last_time.get ());
  output_submodule (*last_time, "last_time", log);
  output_derived (development, "Devel", log);
  if (vernalization.required)	// Test needed for checkpoint.
    output_submodule (vernalization, "Vernal", log);
  output_derived (photo, "LeafPhot", log);
  output_submodule (nitrogen, "CrpN", log);
}

CropStandard::CropStandard (Block& al)
  : Crop (al),
    initial_weight (al.number ("weight", -42.42e42)),
    seed (Librarian::build_item<Seed> (al, "Seed")),
    root_system (submodel<RootSystem> (al, "Root")),
    canopy (al.alist ("Canopy")),
    harvesting (submodel<Harvesting> (al, "Harvest")),
    production (al.alist ("Prod")),
    last_time (al.check ("last_time")
               ? new Time (al.alist ("last_time"))
               : NULL),
    development (Librarian::build_item<Phenology> (al, "Devel")),
    partition (al.alist ("Partit")),
    vernalization (al.check ("Vernal")
                   ? al.alist ("Vernal")
                   : Vernalization::no_vernalization ()),
    photo (Librarian::build_item<Photo> (al, "LeafPhot")),
    nitrogen (al.alist ("CrpN")),
    water_stress_effect (al.check ("water_stress_effect")
                         ? std::auto_ptr<WSE> (Librarian::build_item<WSE> 
                                               (al, "water_stress_effect"))
                         : (photo->handle_water_stress ()
                            ? WSE::create_none ()
                            : WSE::create_full ())),
    enable_N_stress (al.flag ("enable_N_stress", !photo->handle_N_stress ())),
    min_light_fraction (al.number ("min_light_fraction"))
{ }

CropStandard::~CropStandard ()
{ }

static struct CropStandardSyntax
{
  static Model& make (Block& al)
  { return *new CropStandard (al); }
  CropStandardSyntax ();
} standard_crop_syntax;

CropStandardSyntax::CropStandardSyntax ()
{
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this parameterization."); 
  alist.add ("description", "Standard Daisy crop model.  Hansen, 1999.");

  syntax.add ("weight", "g w.w./m^2", Check::positive (), Syntax::OptionalConst,
              "Amount of seeds applied when sowing.");
  syntax.add_object ("Seed", Seed::component, 
                     "Initial crop growth.");
  alist.add ("Seed", Seed::default_model ());
  syntax.add_submodule ("Root", alist, Syntax::State, 
			"Root system.", RootSystem::load_syntax);
  syntax.add_submodule ("Canopy", alist, Syntax::State,
			"Canopy.", CanopyStandard::load_syntax);
  syntax.add_submodule ("Harvest", alist, Syntax::State,
			"Harvest parameters.", Harvesting::load_syntax);
  syntax.add_submodule ("Prod", alist, Syntax::State,
			"Production.", Production::load_syntax);
  syntax.add_submodule ("last_time", alist, Syntax::OptionalState,
			"The time of the previous timestep.",
                        Time::load_syntax);
  syntax.add_object ("Devel", Phenology::component, 
                     "Development and phenology.");
  syntax.add_submodule ("Partit", alist, Syntax::Const,
			"Assimilate partitioning.", Partition::load_syntax);
  syntax.add_submodule ("Vernal", alist, Syntax::OptionalState, 
			"Vernalization.", Vernalization::load_syntax);
  syntax.add_object ("LeafPhot", Photo::component,
                     Syntax::Const, Syntax::Singleton,
                     "Leaf photosynthesis.");
  alist.add ("LeafPhot", Photo::default_model ());
  syntax.add_submodule ("CrpN", alist, Syntax::State,
			"Nitrogen parameters.", CrpN::load_syntax);

  syntax.add_object ("water_stress_effect", WSE::component, 
                     Syntax::OptionalConst, Syntax::Singleton,
                     "Effect of water stress on production.\n\
By default, this will be 'none' iff the selected photosynthesis model\n\
does handle water stress implicitly, and 'full' otherwise.");
  syntax.add ("enable_N_stress", Syntax::Boolean, Syntax::OptionalConst,
	      "Set this true to let nitrogen stress limit production.\n\
By default, it will be true iff the selected photosynthesis model does\n \
handle nitrogen stress implicitly.");
  syntax.add_fraction ("min_light_fraction", Syntax::Const, "\n\
When multiple crops are competing for light, this parameter specifies\n\
a minumum amount of the light this crop will receive.  The idea is\n\
that the field has patches where one crop is dominating, as specified\n\
by this parameter, and in these patches the crop will not have to\n\
compete for light.  The crop still needs LAI in order to catch the\n\
light though.  Competition for water and nutrients are unaffected.");
  alist.add ("min_light_fraction", 0.0);
  Librarian::add_type (Crop::component, "default", alist, syntax, &make);
}

// crop_std.C ends here.
