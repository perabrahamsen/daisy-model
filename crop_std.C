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

#include "crop.h"
#include "chemicals.h"
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
#include <sstream>
#include <numeric>

class CropStandard : public Crop
{
  // Content.
public:
  std::auto_ptr<RootSystem> root_system;
  CanopyStandard canopy;
  Harvesting harvesting;
  Production production;
  std::auto_ptr<Phenology> development;
  const Partition partition;
  Vernalization vernalization;
  std::auto_ptr<Photo> photo;
  CrpN nitrogen;
  const std::auto_ptr<WSE> water_stress_effect;
  const bool enable_N_stress;
  const double min_light_fraction;

  // Communication with Bioclimate.
public:
  double minimum_light_fraction () const
  { return min_light_fraction; }

#if 0
  double water_stress () const // [0-1] (0 = full production)
  { return root_system->water_stress; }
  double nitrogen_stress () const // [0-1] (1 = no production)
  { return root_system->nitrogen_stress; }
#endif
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }

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
  double EPext () const
  { return canopy.EPext; }
  double IntcpCap () const	// Interception Capacity.
  { return canopy.IntcpCap; }
  double EpFac () const		// Convertion to potential evapotransp.
  { return canopy.EpFactor (DS ()); }
  void CanopyStructure ()
  { canopy.CanopyStructure (development->DS); }
  double ActualWaterUptake (double Ept, 
                            const Geometry& geo,
			    const Soil& soil, SoilWater& soil_water,
			    double EvapInterception, double day_fraction, 
                            const double dt, 
			    Treelog& msg)
  { return root_system->water_uptake (Ept, geo, soil, soil_water, 
                                      EvapInterception, day_fraction,
                                      dt, msg); }
  void force_production_stress  (double pstress)
  { root_system->production_stress = pstress; }

  // Simulation.
public:
  void tick (const Time& time, double relative_humidity,
	     const Bioclimate&, const Geometry& geo,
             const Soil&,
	     OrganicMatter*,
	     const SoilHeat&,
	     const SoilWater&,
	     SoilNH4*,
	     SoilNO3*, 
	     double& residuals_DM,
	     double& residuals_N_top, double& residuals_C_top,
	     std::vector<double>& residuals_N_soil,
	     std::vector<double>& residuals_C_soil,
	     double ForcedCAI,
             double dt,
	     Treelog&);
  void emerge ()
  { development->DS = -1e-10; }
  const Harvest& harvest (symbol column_name,
			  const Time&, const Geometry&,
			  Bioclimate& bioclimate,
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
  double sorg_height () const 
  { return harvesting.sorg_height; }
  void output (Log&) const;

  // Queries.
  double DS () const
  { return development->DS; }
  double DM (double height) const;
  double total_N () const
  { return production.total_N (); }
  double total_C () const
  { return production.total_C (); }

  // Create and Destroy.
public:
  void initialize (Treelog&, const Geometry& geometry, OrganicMatter *const);
  CropStandard (Block& vl);
  ~CropStandard ();
};

double 
CropStandard::DM (double height) const
{
  if (canopy.Height < height)
    return 0.0;

  const double stem_harvest = (1.0 - height / canopy.Height);
  const double stub_CAI = (canopy.CAI > 0.0)
    ? canopy.LAIvsH (height)
    : 0.0;
  const double leaf_harvest = (1.0 - stub_CAI / canopy.CAI);
  const double total = stem_harvest * (production.WStem + production.WDead)
    + leaf_harvest * production.WLeaf 
    + production.WSOrg;      // We shouldn't add this for root fruits.

  return total * 10.0;          // [g/m^2 -> kg/ha]
}

void
CropStandard::initialize (Treelog& msg, const Geometry& geo,
                          OrganicMatter *const organic_matter)
{
  root_system->initialize (geo.cell_size ());
  production.initialize (nitrogen.SeedN);

  if (development->DS >= 0)
    {
      // Dead organic matter.
      if (organic_matter)
        production.initialize (name, harvesting.Root, harvesting.Dead,
                               geo, *organic_matter);
      
      // Update derived state content.
      canopy.tick (production.WLeaf, production.WSOrg, 
		   production.WStem, development->DS, 
		   // We don't save the forced CAI, use simulated CAI
		   //  until midnight (small error).
		   -1.0);
      root_system->set_density (msg, 
			       geo, production.WRoot, development->DS);
      nitrogen.content (development->DS, production);
    }
}

void
CropStandard::tick (const Time& time, const double relative_humidity,
		    const Bioclimate& bioclimate,
                    const Geometry& geo,
		    const Soil& soil,
		    OrganicMatter* organic_matter,
		    const SoilHeat& soil_heat,
		    const SoilWater& soil_water,
		    SoilNH4* soil_NH4,
		    SoilNO3* soil_NO3, 
		    double& residuals_DM,
		    double& residuals_N_top, double& residuals_C_top,
		    std::vector<double>& residuals_N_soil,
		    std::vector<double>& residuals_C_soil,
		    const double ForcedCAI,
                    const double dt,
		    Treelog& msg)
{
  Treelog::Open nest (msg, name);

  // Update cut stress.
  harvesting.tick (time);

  // Update average soil temperature.
  const double T_soil = geo.content_at (soil_heat, &SoilHeat::T,
                                        -root_system->Depth);
  root_system->tick_hourly (time.hour (), T_soil);

  // Clear nitrogen.
  nitrogen.clear ();
  // Clear photo
  photo->clear ();

  // Update age.
  development->DAP += 1.0/24.0;

  // Emergence.
  if (time.hour () == 0 && development->DS <= 0)
    {
      daisy_assert (ForcedCAI < 0.0);

      const double h_middle = geo.content_at (soil_water, &SoilWater::h,
                                              -root_system->Depth/2.);
      development->emergence (h_middle, root_system->soil_temperature);
      if (development->DS >= 0)
	{
	  msg.message ("Emerging");
	  canopy.tick (production.WLeaf, production.WSOrg,
		       production.WStem, development->DS, -1.0);
	  nitrogen.content (development->DS, production);
	  root_system->tick_daily (msg, geo, soil, production.WRoot, 0.0,
                                   development->DS);

	  static const symbol root_symbol ("root");
	  static const symbol dead_symbol ("dead");
	  if (organic_matter)
	    {
	      if (!production.AM_root)
		{
		  production.AM_root
		    = &AM::create (geo.cell_size (), time, harvesting.Root,
				   name, root_symbol, AM::Locked);
		  organic_matter->add (*production.AM_root);
		}
	      if (!production.AM_leaf)
		{
		  production.AM_leaf
		    = &AM::create (geo.cell_size (), time, harvesting.Dead,
				   name, dead_symbol, AM::Locked);
		  organic_matter->add (*production.AM_leaf);
		}
	    }
	  else
	    {
	      if (!production.AM_root)
		production.AM_root
		  = &AM::create (geo.cell_size (), time, harvesting.Root,
				 name, root_symbol, AM::Unlocked);
	      if (!production.AM_leaf)
		production.AM_leaf
		  = &AM::create (geo.cell_size (), time, harvesting.Dead,
				 name, dead_symbol, AM::Unlocked);
	    }
	}
      return;
    }
  if (development->DS <= 0 || development->mature ())
    return;

  if (soil_NO3)
    {
      daisy_assert (soil_NH4);
      nitrogen.update (time.hour (), production.NCrop, development->DS,
		       enable_N_stress,
		       geo, soil, soil_water, *soil_NH4, *soil_NO3,
                       bioclimate.day_fraction (),
		       *root_system, dt);
    }
  else
    {
      daisy_assert (!soil_NH4);
      production.NCrop = nitrogen.PtNCnt;
    }  
  const double nitrogen_stress = nitrogen.nitrogen_stress;
  const double water_stress = root_system->water_stress;

  if (bioclimate.hourly_global_radiation () > 1e-10 && canopy.CAI > 0)
    {
      double Ass = 0.0;
      const std::vector<double>& total_PAR = bioclimate.PAR (); 
      const std::vector<double>& sun_PAR = bioclimate.sun_PAR ();
      daisy_assert (sun_PAR.size () > 1);
      daisy_assert (total_PAR.size () == sun_PAR.size ());
      std::vector<double> shadow_PAR;
      
      for(int i = 0; i < total_PAR.size (); i++) 
	shadow_PAR.push_back(total_PAR[i] - sun_PAR[i]);
      
      const double total_LAI = bioclimate.LAI ();
      const std::vector<double>& fraction_sun_LAI = bioclimate.sun_LAI_fraction ();

      std::vector<double> fraction_shadow_LAI;
      std::vector<double> fraction_total_LAI;
      const int No = fraction_sun_LAI.size ();

      for(int i = 0; i < No; i++) 
	{
	  const double f_sun = fraction_sun_LAI[i]; 
	  fraction_shadow_LAI.push_back(1.0-f_sun);
	  fraction_total_LAI.push_back(1.0);
	  daisy_assert (f_sun <= 1.0);
	  daisy_assert (f_sun >= 0.0);
	}
      
      const double cropN = production.NLeaf;
      daisy_assert (cropN >= 0.0);
      
      const double ABA_xylem = 0.0;

      if (bioclimate.shared_light_fraction () > 1e-10)
        {
          // Shared light.
	  Ass += photo->assimilate (ABA_xylem, relative_humidity, 
				    bioclimate.daily_air_temperature (),
				    production.NLeaf, shadow_PAR, bioclimate.height (),
                                    total_LAI, fraction_shadow_LAI,
                                    canopy, *development, msg)
            * bioclimate.shared_light_fraction ();

          Ass += photo->assimilate (ABA_xylem, relative_humidity,
				    bioclimate.daily_air_temperature (),
				    production.NLeaf, sun_PAR, bioclimate.height (),
                                    total_LAI, fraction_sun_LAI,
                                    canopy, *development, msg)
            * bioclimate.shared_light_fraction ();
        }

      if (min_light_fraction > 1e-10)// Only total PAR, not sun shadow
        {
          // Private light.
          const int No = 30;
          std::vector<double> PAR (No + 1, 0.0);
          Bioclimate::radiation_distribution 
            (No, LAI (), PARref (), bioclimate.hourly_global_radiation (),
             PARext (), PAR); 
          Ass += photo->assimilate (ABA_xylem, relative_humidity,
				    bioclimate.daily_air_temperature (), 
				    production.NLeaf, PAR, bioclimate.height (),
				    bioclimate.LAI (), fraction_total_LAI,
				    canopy, *development, msg)
            * min_light_fraction;
        }

      production.PotCanopyAss = Ass;
      if (root_system->production_stress >= 0.0)
	Ass *= (1.0 - root_system->production_stress);
      else 
	Ass *= water_stress_effect->factor (water_stress);
      if (enable_N_stress)
	Ass *= (1.0 - nitrogen_stress);
      Ass *= (1.0 - harvesting.cut_stress);
      production.CanopyAss = Ass;
      production.CH2OPool += Ass;
      daisy_assert (Ass >= 0.0);
    }
  else
    production.PotCanopyAss = production.CanopyAss = 0.0;

  const double T_soil_3 = geo.content_at (soil_heat, &SoilHeat::T,
                                        -root_system->Depth/3.0);
  production.tick (bioclimate.daily_air_temperature (), T_soil_3,
		   root_system->Density, geo, development->DS, 
		   canopy.CAImRat, nitrogen, nitrogen_stress, partition, 
		   residuals_DM, residuals_N_top, residuals_C_top,
		   residuals_N_soil, residuals_C_soil, msg);
  nitrogen.content (development->DS, production);
  if (time.hour () != 0)
    return;

  canopy.tick (production.WLeaf, production.WSOrg,
	       production.WStem, development->DS, ForcedCAI);

  development->tick_daily (bioclimate.daily_air_temperature (), 
                           production.WLeaf, production, vernalization,
                           harvesting.cut_stress, msg);
  root_system->tick_daily (msg, geo, soil, 
                           production.WRoot, production.IncWRoot,
                           development->DS);
}

const Harvest&
CropStandard::harvest (const symbol column_name,
		       const Time& time,
		       const Geometry& geometry,
		       Bioclimate& bioclimate,
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
  nitrogen.content (development->DS, production);

  // Removed chemicals.
  Chemicals chemicals;
  
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
	  bioclimate.harvest_chemicals (chemicals, canopy.CAI - stub_CAI);
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
    = harvesting (column_name, name, 
		  root_system->Density,
		  time, geometry, production, development->DS,
		  stem_harvest, leaf_harvest, chemicals,
		  stem_harvest_frac, leaf_harvest_frac, sorg_harvest_frac,
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
	  if (harvesting.DSnew < 0.0)
	    {
	      // We want a cut to always put back development, even
	      // for a crop which is so retarded that it has no stem
	      // worth speaking about, and hence, no height.
	      const double DS_height = canopy.DS_at_height (stub_length);
	      if (development->DS > DS_height)
		development->DS = std::max (DS_height, 0.01);
	    }
	  else if (development->DS > harvesting.DSnew)
	    development->DS = harvesting.DSnew;
	  
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

void
CropStandard::output (Log& log) const
{
  output_submodule (*root_system, "Root", log);
  output_submodule (canopy, "Canopy", log);
  output_submodule (harvesting, "Harvest", log);
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
  output_derived (development, "Devel", log);
  if (vernalization.required)	// Test needed for checkpoint.
    output_submodule (vernalization, "Vernal", log);
  output_derived (photo, "LeafPhot", log);
  output_submodule (nitrogen, "CrpN", log);
}

CropStandard::CropStandard (Block& al)
  : Crop (al),
    root_system (submodel<RootSystem> (al, "Root")),
    canopy (al.alist ("Canopy")),
    harvesting (al.alist ("Harvest")),
    production (al.alist ("Prod")),
    development (Librarian<Phenology>::build_item (al, "Devel")),
    partition (al.alist ("Partit")),
    vernalization (al.check ("Vernal")
                   ? al.alist ("Vernal")
                   : Vernalization::no_vernalization ()),
    photo (Librarian<Photo>::build_item (al, "LeafPhot")),
    nitrogen (al.alist ("CrpN")),
    water_stress_effect (Librarian<WSE>::build_item (al, 
                                                     "water_stress_effect")),
    enable_N_stress (al.flag ("enable_N_stress")),
    min_light_fraction (al.number ("min_light_fraction"))
{ }

CropStandard::~CropStandard ()
{ }

static struct CropStandardSyntax
{
  static Crop& make (Block& al)
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

  syntax.add_submodule ("Root", alist, Syntax::State, 
			"Root system.", RootSystem::load_syntax);
  syntax.add_submodule ("Canopy", alist, Syntax::State,
			"Canopy.", CanopyStandard::load_syntax);
  syntax.add_submodule ("Harvest", alist, Syntax::State,
			"Harvest parameters.", Harvesting::load_syntax);
  syntax.add_submodule ("Prod", alist, Syntax::State,
			"Production.", Production::load_syntax);
  syntax.add ("Devel", Librarian<Phenology>::library (), 
	      "Development and phenology.");
  syntax.add_submodule ("Partit", alist, Syntax::Const,
			"Assimilate partitioning.", Partition::load_syntax);
  syntax.add_submodule ("Vernal", alist, Syntax::OptionalState, 
			"Vernalization.", Vernalization::load_syntax);
  syntax.add ("LeafPhot", Librarian<Photo>::library (),
              Syntax::Const, Syntax::Singleton,
              "Leaf photosynthesis.");
  alist.add ("LeafPhot", Photo::default_model ());
  syntax.add_submodule ("CrpN", alist, Syntax::State,
			"Nitrogen parameters.", CrpN::load_syntax);

  syntax.add ("water_stress_effect", Librarian<WSE>::library (), 
              Syntax::Const, Syntax::Singleton,
	      "Effect of water stress on production.");
  alist.add ("water_stress_effect", WSE::default_model ());
  syntax.add ("enable_N_stress", Syntax::Boolean, Syntax::Const,
	      "Set this true to let nitrogen stress limit production.");
  alist.add ("enable_N_stress", true);
  syntax.add_fraction ("min_light_fraction", Syntax::Const, "\n\
When multiple crops are competing for light, this parameter specifies\n\
a minumum amount of the light this crop will receive.  The idea is\n\
that the field has patches where one crop is dominating, as specified\n\
by this parameter, and in these patches the crop will not have to\n\
compete for light.  The crop still needs LAI in order to catch the\n\
light though.  Competition for water and nutrients are unaffected.");
  alist.add ("min_light_fraction", 0.0);
  Librarian<Crop>::add_type ("default", alist, syntax, &make);
}
