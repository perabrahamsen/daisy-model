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
#include "organic.h"
#include "soil_heat.h"
#include "am.h"
#include "submodeler.h"
#include "mathlib.h"
#include "librarian.h"
#include "memutils.h"
#include "check.h"
#include "treelog.h"
#include "frame.h"
#include "block_model.h"
#include "rubiscoN.h"
#include "metalib.h"
#include "cstage.h"
#include <sstream>
#include <numeric>

struct CropStandard : public Crop
{
  const Metalib& metalib;
  
  // Content.
  const std::unique_ptr<Seed> seed;
  const std::unique_ptr<RootSystem> root_system;
  const std::unique_ptr<CanopyStandard> canopy;
  std::unique_ptr<Harvesting> harvesting;
  Production production;
  Time last_time;
  Time sow_time;
  Time emerge_time;
  Time flowering_time;
  Time ripe_time;
  std::unique_ptr<Phenology> development;
  std::unique_ptr<CStage> cstage;
  Partition partition;
  std::unique_ptr<Vernalization> vernalization;
  const std::unique_ptr<Photo> shadow;
  const std::unique_ptr<Photo> sunlit;
  const std::unique_ptr<Photo> reserved;
  const std::unique_ptr<CrpN> nitrogen;
  const std::unique_ptr<WSE> water_stress_effect;
  const bool enable_N_stress;
  const double min_light_fraction;
  const std::unique_ptr<RubiscoN> rubiscoN;
  
  // Communication with Bioclimate.
  double minimum_light_fraction () const
  { return min_light_fraction; }

  double rs_min () const	// Minimum transpiration resistance.
  { return canopy->rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy->rs_max; }
  double shadow_stomata_conductance () const // Current gs [m/s FIELD].
  {
    
    // Stomata conductance
    const double gs = shadow->stomata_conductance ();//[m s^-1]
    if(gs < 0)
      return 1.0/rs_min(); // Photo_GL have no stomata conductance.
    return gs; 
  }                     
  double sunlit_stomata_conductance () const // Current gs [m/s FIELD].
  {
    
    // Stomata conductance
    const double gs = sunlit->stomata_conductance ();//[m s^-1]
    if(gs < 0)
      return 1.0/rs_min(); // Photo_GL have no stomata conductance.
    return gs; 
  }
  double leaf_width () const
  { return canopy->leaf_width (DS ()); }

  double height () const	// Crop height [cm]
  { return canopy->Height; }
  double LAI () const
  { return canopy->CAI; }
  double SimLAI () const
  { return canopy->SimCAI; }
  const PLF& LAIvsH () const
  { return canopy->LAIvsH; }
  double PARext () const
  { return canopy->PARext; }
  double PARref () const
  { return canopy->PARref; }
  double NIRext () const
  { return canopy->PARext; }
  double NIRref () const
  { return canopy->PARref; }
  double EPext () const
  { return canopy->EPext; }
  double IntcpCap () const	// Interception Capacity.
  { return canopy->IntcpCap; }
  double EpFacDry () const		// Convertion to potential evapotransp.
  { return canopy->EpFactorDry (DS ()); }
  double EpFacWet () const		// Convertion to potential evapotransp.
  { return canopy->EpFactorWet (DS ()); }
  void CanopyStructure ()
  { canopy->CanopyStructure (development->DS); }
  double ActualWaterUptake (double Ept, 
                            const Geometry& geo,
			    const Soil& soil, const SoilWater& soil_water,
			    const double EvapInterception, const double dt, 
			    Treelog& msg)
  { return root_system->water_uptake (Ept, geo, soil, soil_water, 
                                      EvapInterception, 
                                      dt, msg); }
  void force_production_stress  (double pstress)
  { root_system->production_stress = pstress; }

  // Simulation.
  void find_stomata_conductance (const Time& time, 
                                 const Bioclimate&, double dt, Treelog&);
  void tick (const Scope&, const Time& time, const Bioclimate&, double ForcedCAI,
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
  double stage () const
  { return cstage->stage (); }
  double DM (double height) const;
  double SOrg_DM () const;
  double N_fixated () const
  { 
    // kg/ha -> g/m^2
    const double conv = 1000.0 / (100.0 * 100.0);
    return nitrogen->Fixated / conv; 
  }
  double total_N () const
  { return production.total_N (); }
  double total_C () const
  { return production.total_C (); }
  const std::vector<double>& effective_root_density () const
  { return root_system->effective_density (); }

  // Create and Destroy.
  void initialize (const Scope&, const Geometry& geometry, const Soil&,
                   double row_width, double row_pos, double seed,
                   OrganicMatter&, const Time&, Treelog&);
  void initialize (const Scope&, const Geometry&, const Soil&, OrganicMatter&, 
                   const Time&, Treelog&);
  void initialize_shared (const Scope&,
                          const Geometry&, const Soil&, OrganicMatter&, 
                          const Time&, Treelog&);
  bool check (const Scope&, const Geometry&, Treelog&) const;
  CropStandard (const BlockModel& vl);
  ~CropStandard ();
};

double 
CropStandard::DM (const double height) const
{
  if (!std::isnormal (canopy->Height))
    return 0.0;
  const double stem_harvest = bound (0.0, (1.0 - height / canopy->Height), 1.0);
  const double leaf_harvest = (canopy->CAI > 0.0 && height < canopy->Height)
    ? bound (0.0, (1.0 - canopy->LAIvsH (height)  / canopy->CAI), 1.0)
    : 0.0;
  const double sorg_harvest = (height < harvesting->sorg_height) ? 1.0 : 0.0;
  const double total = stem_harvest * (production.WStem + production.WDead)
    + leaf_harvest * production.WLeaf 
    + sorg_harvest * production.WSOrg;

#if 0
  std::ostringstream tmp;
  tmp << "height = " << height << ", CAI (height) = "  
      << ((leaf_harvest > 0.0) ? canopy->LAIvsH (height) : 0.0 )
      << ", CAI = " << canopy->CAI 
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
CropStandard::initialize (const Scope& scope, const Geometry& geo,
			  const Soil& soil,
                          const double row_width, const double row_pos, 
                          const double seed_w,
                          OrganicMatter& organic_matter,
                          const Time& now, Treelog& msg)
{
  TREELOG_MODEL (msg);
  root_system->initialize (geo, soil, row_width, row_pos, DS (), msg);
  seed->initialize (seed_w, msg);
  initialize_shared (scope, geo, soil, organic_matter, now, msg);
}

void
CropStandard::initialize (const Scope& scope,
			  const Geometry& geo, const Soil& soil, 
                          OrganicMatter& organic_matter,
                          const Time& now, Treelog& msg)
{
  TREELOG_MODEL (msg);
  root_system->initialize (geo, soil, DS (), msg);
  seed->initialize (-42.42e42, msg);
  initialize_shared (scope, geo, soil, organic_matter, now, msg);
}

void
CropStandard::initialize_shared (const Scope& scope, const Geometry& geo,
				 const Soil& soil,
                                 OrganicMatter& organic_matter,
                                 const Time& now, Treelog& msg)
{
  if (sow_time == Time::null ())
    sow_time = now;
  if (last_time == Time::null ())
    last_time = now;
  production.initialize (seed->initial_N ());

  const double DS = development->DS;
  if (DS >= 0)
    {
      // Dead organic matter.
      production.initialize (metalib, objid, harvesting->Root, harvesting->Dead,
                             geo, organic_matter, msg);
      
      // Update derived state content.
      const double WLeaf = production.WLeaf;
      const double SpLAI = canopy->specific_LAI (DS);
      const double seed_CAI = seed->forced_CAI (WLeaf, SpLAI, DS);
      canopy->tick (WLeaf, production.WSOrg, production.WStem, DS, 
		   // We don't save the forced CAI, use simulated CAI
		   //  until midnight (small error).
		   seed_CAI);
      root_system->set_density (geo, soil, production.WRoot, DS, msg);
      nitrogen->content (DS, production, msg);
    }
}

bool
CropStandard::check (const Scope&, const Geometry& geo, 
                     Treelog& msg) const
{
  TREELOG_MODEL (msg);

  bool ok = true;
  if (!seed->check (msg))
    ok = false;
  if (!root_system->check (geo, msg))
    ok = false;
  if (seed->initial_N () <= 0.0 && production.NCrop <= 0.0)
    {
      ok = false;
      msg.error ("\
You must specify initial N content in either 'Prod' or 'Seed'");
    }
  return ok;
}

void
CropStandard::find_stomata_conductance (const Time& time,
                                        const Bioclimate& bioclimate,
                                        const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

  // No production yet.
  production.PotCanopyAss = 0.0;

  // Check age.
  const double DS = development->DS;
  if (DS <= 0.0 || development->mature ())
    return;
  
  // Clear data from previous iteration.
  shadow->clear ();
  sunlit->clear ();
  reserved->clear ();
  
  // Boundary conditions.
  const double canopy_vapour_pressure = bioclimate.canopy_vapour_pressure ();
  const double CO2_atm = bioclimate.atmospheric_CO2 ();
  const double O2_atm = bioclimate.atmospheric_O2 ();
  const std::vector<double>& total_PAR = bioclimate.PAR (); 
  const std::vector<double>& sun_PAR = bioclimate.sun_PAR ();
  daisy_assert (sun_PAR.size () > 1);
  daisy_assert (total_PAR.size () == sun_PAR.size ());
  const double total_LAI = bioclimate.LAI ();
  const std::vector<double>& fraction_sun_LAI = bioclimate.sun_LAI_fraction ();
  const std::vector<double>& PAR_height = bioclimate.height ();
  const double sun_LAI_fraction_total = bioclimate.sun_LAI_fraction_total ();

  // Anything do do?
  if (total_PAR[0] < shadow->min_PAR ())
    return;

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

      if (top_crop && height <= canopy->Height)
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
    = canopy->corresponding_WLeaf (DS) * nitrogen->NfLeafCnc (DS);
  const double N_at_Cr
    = canopy->corresponding_WLeaf (DS) * nitrogen->CrLeafCnc (DS);
  const double N_at_Pt
    = canopy->corresponding_WLeaf (DS) * nitrogen->PtLeafCnc (DS);
  const double rubisco_N = rubiscoN->value (total_LAI, production.NLeaf,
					    N_at_Nf, N_at_Cr, N_at_Pt);
  daisy_assert (rubisco_N >= 0.0);
      
  const double ABA_xylem = root_system->ABAConc;
  daisy_assert (std::isfinite (ABA_xylem));
  daisy_assert (ABA_xylem >= 0.0);
  
  const double T_canopy = bioclimate.canopy_temperature ();
  const double T_leaf_sun = bioclimate.sun_leaf_temperature ();
  const double T_leaf_shadow = bioclimate.shadow_leaf_temperature ();
  const double Ptot = bioclimate.air_pressure ();
  daisy_assert (sun_LAI_fraction_total < 1.0);
  const double crown_potential = root_system->crown_potential ();

  if (bioclimate.shared_light_fraction () > 1e-10)
    {
      // Shared light.
      if (sun_LAI_fraction_total < 1.0)
        {
          const double gbw_shadow       // [m/s leaf]
            = bioclimate.shadow_boundary_layer_water_conductivity ()
            / (total_LAI * (1.0 - sun_LAI_fraction_total));
          
          Ass += shadow->assimilate (metalib.units (),
				     ABA_xylem, crown_potential, 
                                     canopy_vapour_pressure, gbw_shadow,
                                     CO2_atm, O2_atm, Ptot,
                                     bioclimate.daily_air_temperature(), 
                                     T_canopy, T_leaf_shadow,
                                     rubisco_N, shadow_PAR, PAR_height,
                                     total_LAI, fraction_shadow_LAI, dt,
                                     *canopy, *development, msg)
          * bioclimate.shared_light_fraction ();
        }

      if (sun_LAI_fraction_total > 0.0)
        {
          const double gbw_sun          // [m/s leaf]
            = bioclimate.sun_boundary_layer_water_conductivity ()
            / (total_LAI * sun_LAI_fraction_total);
          Ass += sunlit->assimilate (metalib.units (),
                                     ABA_xylem, crown_potential, 
                                     canopy_vapour_pressure, gbw_sun, 
                                     CO2_atm, O2_atm, Ptot,
                                     bioclimate.daily_air_temperature(),
                                     T_canopy, T_leaf_sun,
                                     rubisco_N, sun_PAR,  PAR_height,
                                     total_LAI, fraction_sun_LAI, dt,
                                     *canopy, *development, msg)
            * bioclimate.shared_light_fraction ();
        }
    }

  if (min_light_fraction > 1e-10)// Only total PAR, not sun shadow
    {
      // Private light.
      const int No = 30;
      std::vector<double> PAR (No + 1, 0.0);
      Bioclimate::radiation_distribution 
        (No, LAI (), PARref (), bioclimate.global_radiation (),
         PARext (), PAR); 
      const double gbw_total       // [m/s leaf]
        = (bioclimate.shadow_boundary_layer_water_conductivity ()
           + bioclimate.sun_boundary_layer_water_conductivity ())
        / total_LAI;
      Ass += reserved->assimilate (metalib.units (),
                                   ABA_xylem, crown_potential, 
                                   canopy_vapour_pressure, gbw_total,
                                   CO2_atm, O2_atm, Ptot,
                                   bioclimate.daily_air_temperature (), 
                                   T_canopy, bioclimate.canopy_temperature(),
                                   rubisco_N, PAR, PAR_height,
                                   bioclimate.LAI (), fraction_total_LAI, dt,
                                   *canopy, *development, msg)
        * min_light_fraction;
    }
  daisy_assert (std::isfinite (Ass));
  production.PotCanopyAss = Ass;
}

void
CropStandard::tick (const Scope& scope, 
                    const Time& time, const Bioclimate& bioclimate, 
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

  // For summarizing light hours.
  development->tick ();

  // Update cut stress.
  harvesting->tick (time);

  // Update average soil temperature.
  const double day_fraction = bioclimate.day_fraction (dt);
  const double T_soil 
    = geo.content_height (soil_heat, &SoilHeat::T, -root_system->Depth);
  root_system->tick_dynamic (T_soil, day_fraction, soil_water, dt);

  // Clear nitrogen.
  nitrogen->clear ();

  // Update age.
  development->DAP += dt/24.0;

  // New day?
  const Timestep daystep = time - last_time;
  const bool new_day = (time.yday () != last_time.yday ()
                        || time.year () != last_time.year ());
  if (new_day)
    last_time = time;

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
      development->emergence (scope, h_middle, root_system->soil_temperature, 
                              daystep.total_hours (), msg);
      cstage->tick (DS, msg);
      if (DS >= 0)
	{
          emerge_time = time;
          const double WLeaf = production.WLeaf;
          const double SpLAI = canopy->specific_LAI (DS);
          const double seed_CAI = seed->forced_CAI (WLeaf, SpLAI, DS);
	  canopy->tick (production.WLeaf, production.WSOrg,
		       production.WStem, DS, seed_CAI);
	  nitrogen->content (DS, production, msg);
	  root_system->tick_daily (geo, soil, soil_water, production.WRoot, false,
                                   DS, msg);

	  static const symbol root_symbol ("root");
	  static const symbol dead_symbol ("dead");

          if (!production.AM_root)
            {
              production.AM_root
                = &AM::create (metalib, geo, time, harvesting->Root,
                               objid, root_symbol, AM::Locked, msg);
              organic_matter.add (*production.AM_root);
            }
          if (!production.AM_leaf)
            {
              production.AM_leaf
                = &AM::create (metalib, geo, time, harvesting->Dead,
                               objid, dead_symbol, AM::Locked, msg);
              organic_matter.add (*production.AM_leaf);
	    }
	  else
	    {
	      if (!production.AM_root)
		production.AM_root
		  = &AM::create (metalib, geo, time, harvesting->Root,
				 objid, root_symbol, AM::Unlocked, msg);
	      if (!production.AM_leaf)
		production.AM_leaf
		  = &AM::create (metalib, geo, time, harvesting->Dead,
				 objid, dead_symbol, AM::Unlocked, msg);
	    }
	}
      return;
    }
  if (development->mature ())
    {
      production.none ();
      return;
    }

  daisy_assert (production.AM_root);
  daisy_assert (production.AM_leaf);
  
  nitrogen->update (production.NCrop, DS, 
                   geo, soil, soil_water, chemistry,
                   bioclimate.day_fraction (dt),
                   *root_system, dt);

  harvesting->water_use (bioclimate.total_ea () * dt);

  const double nitrogen_stress = nitrogen->nitrogen_stress;
  const double water_stress = root_system->water_stress;
  const double NNI = nitrogen->NNI;

  double Ass = production.PotCanopyAss;
  if (root_system->production_stress >= 0.0)
    Ass *= (1.0 - root_system->production_stress);
  else 
    Ass *= water_stress_effect->factor (water_stress);
  if (enable_N_stress)
    Ass *= (1.0 - nitrogen_stress);
  Ass *= (1.0 - harvesting->cut_stress);
  if (!(Ass >= 0.0))
    {
      std::ostringstream tmp;
      tmp << "Ass = " << Ass << "; PotCanopyAss = " << production.PotCanopyAss
          << "; water stress = " << water_stress
          << "; water stress effect = "
               << water_stress_effect->factor (water_stress)
          << ";nitrogen stress = " << nitrogen_stress
          << ";cut stress = " << harvesting->cut_stress;
      msg.bug (tmp.str ());
      Ass = 0.0;
    }
  daisy_assert (Ass >= 0.0);
  production.CanopyAss = Ass;

  // Root zone heat.
  const double T_soil_3 
    = geo.content_height (soil_heat, &SoilHeat::T, -root_system->Depth/3.0);
  daisy_assert (std::isfinite (T_soil_3));

  const double seed_C = seed->release_C (dt);
  production.tick (bioclimate.daily_air_temperature (), T_soil_3,
		   root_system->actual_density (), geo, soil_water, DS, 
		   canopy->CAImRat, *nitrogen, nitrogen_stress, NNI, seed_C, 
                   partition, 
		   residuals_DM, residuals_N_top, residuals_C_top,
		   residuals_N_soil, residuals_C_soil, dt, msg);
  nitrogen->content (DS, production, msg);
  if (!new_day)
    return;

  const double WLeaf = production.WLeaf;
  const double SpLAI = canopy->specific_LAI (DS);
  const double seed_CAI = seed->forced_CAI (WLeaf, SpLAI, DS);
  canopy->tick (WLeaf, production.WSOrg, production.WStem, 
               DS, ForcedCAI < 0.0 ? seed_CAI : ForcedCAI);

  development->tick_daily (scope, bioclimate.daily_air_temperature (), 
                           production.shoot_growth (), production, 
                           *vernalization, harvesting->cut_stress, msg);
  cstage->tick (DS, msg);
  if (DS >= 1.0 && flowering_time == Time::null ())
    flowering_time = time;
  if (DS >= 2.0 && ripe_time == Time::null ())
    ripe_time = time;
  root_system->tick_daily (geo, soil, soil_water,
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
  TREELOG_MODEL (msg);

  // Update nitrogen content.
  nitrogen->content (development->DS, production, msg);

  // Leave stem and leaf below stub alone.
  double stem_harvest;
  double leaf_harvest;
  if (stub_length < canopy->Height)
    {
      stem_harvest = (1.0 - stub_length / canopy->Height);

      if (canopy->CAI > 0.0)
	{
	  const double stub_CAI = canopy->LAIvsH (stub_length);
	  leaf_harvest = (1.0 - stub_CAI / canopy->CAI);
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
    = harvesting->harvest (column_name, objid, 
                           root_system->actual_density (),
                           sow_time, emerge_time, flowering_time, ripe_time,
                           time, geometry, production, development->DS,
                           stem_harvest, leaf_harvest, 1.0,
                           stem_harvest_frac, leaf_harvest_frac,
                           sorg_harvest_frac,
                           kill_off, residuals, residuals_DM,
                           residuals_N_top, residuals_C_top, 
                           residuals_N_soil, residuals_C_soil,
                           combine,
                           root_system->water_stress_days, 
                           nitrogen->nitrogen_stress_days, msg);

  if (!approximate (development->DS, DSremove))
    {
      nitrogen->cut (development->DS); // Stop fixation.

      if (development->DS > 0.0)
	{
	  // Revert development.
	  if (harvesting->DSnew < 0.0)
	    {
	      // We want a cut to always put back development, even
	      // for a crop which is so retarded that it has no stem
	      // worth speaking about, and hence, no height.
	      const double DS_height = canopy->DS_at_height (stub_length);
	      if (development->DS > DS_height)
		development->DS = std::max (DS_height, 0.01);
	    }
	  else if (development->DS > harvesting->DSnew)
	    development->DS = harvesting->DSnew;

	  cstage->tick (development->DS, msg);
	  
	  // Cut canopy.
	  canopy->cut (production.WStem, development->DS, stub_length);

	  daisy_assert (approximate (canopy->CropHeight (production.WStem,
							development->DS), 
				     canopy->Height));
	  canopy->CropCAI (production.WLeaf, production.WSOrg,
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
  TREELOG_MODEL (msg);
  
  // Update nitrogen content.
  nitrogen->content (development->DS, production, msg);

  // Harvest.
  const Harvest& harvest 
    = harvesting->harvest (column_name, objid, 
                           root_system->actual_density (),
                           sow_time, emerge_time, flowering_time, ripe_time,
                           time, geometry, production, development->DS,
                           stem_harvest, leaf_harvest, sorg_harvest,
                           1.0, 1.0, 1.0,
                           false, residuals, residuals_DM,
                           residuals_N_top, residuals_C_top, 
                           residuals_N_soil, residuals_C_soil,
                           false,
                           root_system->water_stress_days, 
                           nitrogen->nitrogen_stress_days, msg);

  // Phenology may be affected.
  if (!approximate (development->DS, DSremove))
    {
      nitrogen->cut (development->DS); // Stop fixation.

      if (development->DS > 0.0)
	{
	  // Revert development.
          if (harvesting->DSnew > 0.0 && development->DS > harvesting->DSnew)
	    development->DS = harvesting->DSnew;
	  
	  // Reset canopy.
	  canopy->CropCAI (production.WLeaf, production.WSOrg,
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
  output_submodule (*canopy, "Canopy", log);
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
  if (last_time != Time::null ())
    output_submodule (last_time, "last_time", log);
  if (sow_time != Time::null ())
    output_submodule (sow_time, "sow_time", log);
  if (emerge_time != Time::null ())
    output_submodule (emerge_time, "emerge_time", log);
  if (flowering_time != Time::null ())
    output_submodule (flowering_time, "flowering_time", log);
  if (ripe_time != Time::null ())
    output_submodule (ripe_time, "ripe_time", log);
  output_derived (development, "Devel", log);
  output_derived (cstage, "CStage", log);
  output_submodule (partition, "Partit", log);
  output_derived (vernalization, "Vernal", log);
  output_derived (shadow, "LeafPhot", log);
  output_derived (sunlit, "sunlit", log);
  output_derived (reserved, "reserved", log);
  output_submodule (*nitrogen, "CrpN", log);
}

static std::unique_ptr<WSE> 
find_WSE (const BlockModel& al, Photo& photo)
{
  if (al.check ("water_stress_effect"))
    return std::unique_ptr<WSE> 
      (Librarian::build_item<WSE> (al, "water_stress_effect"));
  Treelog& msg = al.msg ();
  if (photo.handle_water_stress ())
    {
      msg.debug ("Implicit water stress in photosynthesis module");
      return WSE::create_none ();
    }
  msg.debug ("Water stress calculated from energy balance");
  return WSE::create_full ();
}

CropStandard::CropStandard (const BlockModel& al)
  : Crop (al),
    metalib (al.metalib ()),
    seed (Librarian::build_item<Seed> (al, "Seed")),
    root_system (submodel<RootSystem> (al, "Root")),
    canopy (submodel<CanopyStandard> (al, "Canopy")),
    harvesting (submodel<Harvesting> (al, "Harvest")),
    production (al.submodel ("Prod")),
    last_time (al.check ("last_time")
               ? Time (al.submodel ("last_time"))
               : Time::null ()),
    sow_time (al.check ("sow_time")
               ? Time (al.submodel ("sow_time"))
               : Time::null ()),
    emerge_time (al.check ("emerge_time")
               ? Time (al.submodel ("emerge_time"))
               : Time::null ()),
    flowering_time (al.check ("flowering_time")
               ? Time (al.submodel ("flowering_time"))
               : Time::null ()),
    ripe_time (al.check ("ripe_time")
               ? Time (al.submodel ("ripe_time"))
               : Time::null ()),
    development (Librarian::build_item<Phenology> (al, "Devel")),
    cstage (Librarian::build_item<CStage> (al, "CStage")),
    partition (al.submodel ("Partit")),
    vernalization (Librarian::build_item<Vernalization> (al, "Vernal")),
    shadow (Librarian::build_item<Photo> (al, "LeafPhot")),
    sunlit (Librarian::build_item<Photo> (al, "LeafPhot")),
    reserved (Librarian::build_item<Photo> (al, "LeafPhot")),
    nitrogen (submodel<CrpN> (al, "CrpN")),
    water_stress_effect (find_WSE (al, *shadow)),
    enable_N_stress (al.flag ("enable_N_stress", !shadow->handle_N_stress ())),
    min_light_fraction (al.number ("min_light_fraction")),
    rubiscoN (Librarian::build_item<RubiscoN> (al, "RubiscoN"))
{ 
  if (!al.check ("enable_N_stress"))
    {
      Treelog& msg = al.msg ();
      if (shadow->handle_N_stress ())
        msg.debug ("Nitrogen stress handled by photosynthesis module");
      else
        msg.debug ("Nitrogen stress handled by crop nitrogen module");
    }
}

CropStandard::~CropStandard ()
{ }

static struct CropStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new CropStandard (al); }
  CropStandardSyntax ()
    : DeclareModel (Crop::component, "default",
                    "Standard Daisy crop model.  Hansen, 1999.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("Seed", Seed::component, 
                       "Initial crop growth.");
    frame.set ("Seed", "LAI");
    frame.declare_submodule ("Root", Attribute::State, 
                          "Root system.", RootSystem::load_syntax);
    frame.declare_submodule ("Canopy", Attribute::State,
                          "Canopy.", CanopyStandard::load_syntax);
    frame.declare_submodule ("Harvest", Attribute::State,
                          "Harvest parameters.", Harvesting::load_syntax);
    frame.declare_submodule ("Prod", Attribute::State,
                          "Production.", Production::load_syntax);
    frame.declare_submodule ("last_time", Attribute::OptionalState,
                             "The time of the previous timestep.\n\
Don't set, calculated by Daisy.",
                             Time::load_syntax);
    frame.declare_submodule ("sow_time", Attribute::OptionalState,
                             "The time the crop was sown.\n\
Don't set, calculated by Daisy.",
                             Time::load_syntax);
    frame.declare_submodule ("emerge_time", Attribute::OptionalState,
                             "The time the crop emerged.\n\
Don't set, calculated by Daisy.",
                             Time::load_syntax);
    frame.declare_submodule ("flowering_time", Attribute::OptionalState,
                             "The time the crop flowered.\n\
Don't set, calculated by Daisy.",
                             Time::load_syntax);
    frame.declare_submodule ("ripe_time", Attribute::OptionalState,
                             "The time the crop became ripe.\n\
Don't set, calculated by Daisy.",
                             Time::load_syntax);
    frame.declare_object ("Devel", Phenology::component, 
                       "Development and phenology.");
    frame.declare_object ("CStage", CStage::component, 
			  "Phenological messages.");
    frame.set ("CStage", "Daisy");
    frame.declare_submodule ("Partit", Attribute::State,
                          "Assimilate partitioning.", Partition::load_syntax);
    frame.declare_object ("Vernal", Vernalization::component, 
                      Attribute::State, Attribute::Singleton, "\
Vernalization.");
    frame.set ("Vernal", "none");
    frame.declare_object ("LeafPhot", Photo::component,
                          Attribute::Const, Attribute::Singleton,
                          "Leaf photosynthesis.\n\
Note that if the selected radiation distribution model distinguishes\n\
between sunlit and shadow leaves, only the shadow leaves will be ");
    frame.set ("LeafPhot", "GL");
    frame.declare_object ("sunlit", Photo::component,
                          Attribute::LogOnly, Attribute::Singleton,
                          "Leaf photosynthesis for sunlit leaves.\n\
This will be zero if the selected radiation distribution model does not\n\
distinguish between sunlit and shadow leafs.");
    frame.declare_object ("reserved", Photo::component,
                          Attribute::LogOnly, Attribute::Singleton,
                          "Leaf photosynthesis for reserved leaves.\n\
This is used for simulating \"patches\" in multi-crop systems, such as\n\
a clover-grass mixture.  This is controled by the 'min_light_fraction'\n\
parameter.");
    frame.declare_submodule ("CrpN", Attribute::State,
                          "Nitrogen parameters.", CrpN::load_syntax);

    frame.declare_object ("water_stress_effect", WSE::component, 
                       Attribute::OptionalConst, Attribute::Singleton,
                       "Effect of water stress on production.\n\
By default, this will be 'none' iff the selected photosynthesis model\n\
does handle water stress implicitly, and 'full' otherwise.");
    frame.declare_boolean ("enable_N_stress", Attribute::OptionalConst,
                "Set this true to let nitrogen stress limit production.\n\
By default, it will be true iff the selected photosynthesis model does\n \
handle nitrogen stress implicitly.");
    frame.declare_fraction ("min_light_fraction", Attribute::Const, "\n\
When multiple crops are competing for light, this parameter specifies\n\
a minumum amount of the light this crop will receive.  The idea is\n\
that the field has patches where one crop is dominating, as specified\n\
by this parameter, and in these patches the crop will not have to\n\
compete for light.  The crop still needs LAI in order to catch the\n\
light though.  Competition for water and nutrients are unaffected.");
    frame.set ("min_light_fraction", 0.0);
    frame.declare_object ("RubiscoN", RubiscoN::component, "\
Fraction of N in leaves that is photosynthetically active.");
    frame.set ("RubiscoN", "default");
  }
} standard_crop_syntax;


// crop_std.C ends here.
