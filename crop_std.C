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
#include "photosynthesis.h"
#include "crpn.h"
#include "wse.h"
#include "log.h"
#include "time.h"
#include "bioclimate.h"
#include "common.h"
#include "plf.h"
#include "soil_water.h"
#include "soil.h"
#include "organic_matter.h"
#include "soil_heat.h"
#include "am.h"
#include "mathlib.h"

using namespace std;

class CropStandard : public Crop
{
  // Content.
public:
  RootSystem root_system;
  CanopyStandard canopy;
  Harvesting harvesting;
  Production production;
  auto_ptr<Phenology> development;
  const Partition partition;
  Vernalization vernalization;
  const Photosynthesis photosynthesis;
  CrpN nitrogen;
  const auto_ptr<WSE> water_stress_effect;
  const bool enable_N_stress;
  const double min_light_fraction;

  // Communication with Bioclimate.
public:
  double minimum_light_fraction () const
  { return min_light_fraction; }

#if 0
  double water_stress () const // [0-1] (0 = full production)
  { return root_system.water_stress; }
  double nitrogen_stress () const // [0-1] (1 = no production)
  { return root_system.nitrogen_stress; }
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
			    const Soil& soil, SoilWater& soil_water,
			    double EvapInterception, double day_fraction, 
			    Treelog& msg)
  { return root_system.water_uptake (Ept, soil, soil_water, EvapInterception, 
				     day_fraction, msg);}
  void force_production_stress  (double pstress)
  { root_system.production_stress = pstress; }

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Soil&,
	     OrganicMatter*,
	     const SoilHeat&,
	     const SoilWater&,
	     SoilNH4*,
	     SoilNO3*, 
	     double& residuals_DM,
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil,
	     vector<double>& residuals_C_soil,
	     double ForcedCAI,
	     Treelog&);
  const Harvest& harvest (symbol column_name,
			  const Time&, const Geometry&,
			  Bioclimate& bioclimate,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off,
			  vector<AM*>& residuals,
			  double& residuals_DM,
			  double& residuals_N_top, double& residuals_C_top,
			  vector<double>& residuals_N_soil,
			  vector<double>& residuals_C_soil,
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
  CropStandard (const AttributeList& vl);
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
CropStandard::initialize (Treelog& msg, const Geometry& geometry,
                          OrganicMatter *const organic_matter)
{
  root_system.initialize (geometry.size ());
  production.initialize (nitrogen.SeedN);

  if (development->DS >= 0)
    {
      // Dead organic matter.
      if (organic_matter)
        production.initialize (name, harvesting.Root, harvesting.Dead,
                               geometry, *organic_matter);
      
      // Update derived state content.
      canopy.tick (production.WLeaf, production.WSOrg, 
		   production.WStem, development->DS, 
		   // We don't save the forced CAI, use simulated CAI
		   //  until midnight (small error).
		   -1.0);
      root_system.set_density (msg, 
			       geometry, production.WRoot, development->DS);
      nitrogen.content (development->DS, production);
    }
}

void
CropStandard::tick (const Time& time,
		    const Bioclimate& bioclimate,
		    const Soil& soil,
		    OrganicMatter* organic_matter,
		    const SoilHeat& soil_heat,
		    const SoilWater& soil_water,
		    SoilNH4* soil_NH4,
		    SoilNO3* soil_NO3, 
		    double& residuals_DM,
		    double& residuals_N_top, double& residuals_C_top,
		    vector<double>& residuals_N_soil,
		    vector<double>& residuals_C_soil,
		    double ForcedCAI,
		    Treelog& msg)
{
  Treelog::Open nest (msg, name);

  // Update cut stress.
  harvesting.tick (time);

  // Update average soil temperature.
  const double T_soil = soil_heat.T (soil.interval_plus (-root_system.Depth));
  root_system.tick_hourly (time.hour (), T_soil);

  // Clear nitrogen.
  nitrogen.clear ();

  // Emergence.
  if (time.hour () == 0 && development->DS <= 0)
    {
      daisy_assert (ForcedCAI < 0.0);

      development->emergence (soil.interval_plus (-root_system.Depth/2.),
                              root_system.soil_temperature);
      if (development->DS >= 0)
	{
	  msg.message ("Emerging");
	  canopy.tick (production.WLeaf, production.WSOrg,
		       production.WStem, development->DS, -1.0);
	  nitrogen.content (development->DS, production);
	  root_system.tick_daily (msg, soil, production.WRoot, 0.0,
				  development->DS);

	  static const symbol root_symbol ("root");
	  static const symbol dead_symbol ("dead");
	  if (organic_matter)
	    {
	      if (!production.AM_root)
		{
		  production.AM_root
		    = &AM::create (soil, time, harvesting.Root,
				   name, root_symbol, AM::Locked);
		  organic_matter->add (*production.AM_root);
		}
	      if (!production.AM_leaf)
		{
		  production.AM_leaf
		    = &AM::create (soil, time, harvesting.Dead,
				   name, dead_symbol, AM::Locked);
		  organic_matter->add (*production.AM_leaf);
		}
	    }
	  else
	    {
	      if (!production.AM_root)
		production.AM_root
		  = &AM::create (soil, time, harvesting.Root,
				 name, root_symbol, AM::Unlocked);
	      if (!production.AM_leaf)
		production.AM_leaf
		  = &AM::create (soil, time, harvesting.Dead,
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
		       soil, soil_water, *soil_NH4, *soil_NO3,
                       bioclimate.day_fraction (),
		       root_system);
    }
  else
    {
      daisy_assert (!soil_NH4);
      production.NCrop = nitrogen.PtNCnt;
    }  
  const double nitrogen_stress = nitrogen.nitrogen_stress;
  const double water_stress = root_system.water_stress;

  if (bioclimate.hourly_global_radiation () > 1e-10)
    {
      double Ass = 0.0;

      if (bioclimate.shared_light_fraction () > 1e-10)
        {
          // Shared light.
          const vector<double>& PAR = bioclimate.PAR ();
          daisy_assert (PAR.size () > 1);
          Ass += photosynthesis (bioclimate.daily_air_temperature (),
                                 PAR, bioclimate.height (),
                                 bioclimate.LAI (),
                                 canopy, *development, msg)
            * bioclimate.shared_light_fraction ();
        }
      if (min_light_fraction > 1e-10)
        {
          // Private light.
          const int No = 30;
          vector<double> PAR (No + 1, 0.0);
          Bioclimate::radiation_distribution 
            (No, LAI (), PARref (), bioclimate.hourly_global_radiation (),
             PARext (), PAR); 
          Ass += photosynthesis (bioclimate.daily_air_temperature (),
                                 PAR, bioclimate.height (),
                                 bioclimate.LAI (),
                                 canopy, *development, msg)
            * min_light_fraction;
        }

      production.PotCanopyAss = Ass;
      if (root_system.production_stress >= 0.0)
	Ass *= (1.0 - root_system.production_stress);
      else 
	Ass *= water_stress_effect->factor (water_stress);
      if (enable_N_stress)
	Ass *= (1.0 - nitrogen_stress);
      Ass *= (1.0 - harvesting.cut_stress);
      production.CanopyAss = Ass;
      production.CH2OPool += Ass;
    }
  else
    production.PotCanopyAss = production.CanopyAss = 0.0;

  production.tick (bioclimate.daily_air_temperature (),
		   soil_heat.T (soil.interval_plus (-root_system.Depth / 3.0)),
		   root_system.Density, soil, development->DS, 
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
  root_system.tick_daily (msg, soil, production.WRoot, production.IncWRoot,
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
		       vector<AM*>& residuals,
		       double& residuals_DM,
		       double& residuals_N_top, double& residuals_C_top,
		       vector<double>& residuals_N_soil,
		       vector<double>& residuals_C_soil,
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
		  root_system.Density,
		  time, geometry, production, development->DS,
		  stem_harvest, leaf_harvest, chemicals,
		  stem_harvest_frac, leaf_harvest_frac, sorg_harvest_frac,
		  kill_off, residuals, residuals_DM,
		  residuals_N_top, residuals_C_top, 
		  residuals_N_soil, residuals_C_soil);

  if (development->DS != DSremove)
    {
      nitrogen.cut (development->DS); // Stop fixation.

      if (development->DS > 0.0)
	{
	  // Revert development.
	  if (harvesting.DSnew < 0.0)
	    {

#if 0
	      // Negative value means revert to canopy 
	      if (stub_length < canopy.Height)
		development->DS = canopy.DS_at_height (stub_length);
#else
	      // We want a cut to always put back development, even
	      // for a crop which is so retarded that it has no stem
	      // worth speaking about, and hence, no height.
	      const double DS_height = canopy.DS_at_height (stub_length);
	      if (development->DS > DS_height)
		development->DS = DS_height;
#endif
	      
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
  output_submodule (root_system, "Root", log);
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
  output_submodule (nitrogen, "CrpN", log);
}

CropStandard::CropStandard (const AttributeList& al)
  : Crop (al),
    root_system (al.alist ("Root")),
    canopy (al.alist ("Canopy")),
    harvesting (al.alist ("Harvest")),
    production (al.alist ("Prod")),
    development (Librarian<Phenology>::create (al.alist ("Devel"))),
    partition (al.alist ("Partit")),
    vernalization (al.check ("Vernal")
                   ? al.alist ("Vernal")
                   : Vernalization::no_vernalization ()),
    photosynthesis (al.alist ("LeafPhot")),
    nitrogen (al.alist ("CrpN")),
    water_stress_effect (Librarian<WSE>::create 
                         (!al.flag ("enable_water_stress")
                          ? WSE::none_model ()
                          : al.alist ("water_stress_effect"))),
    enable_N_stress (al.flag ("enable_N_stress")),
    min_light_fraction (al.number ("min_light_fraction"))
{ }

CropStandard::~CropStandard ()
{ }

static struct CropStandardSyntax
{
  static Crop& make (const AttributeList& al)
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
  syntax.add_submodule ("LeafPhot", alist, Syntax::Const,
			"Leaf photosynthesis.", Photosynthesis::load_syntax);
  syntax.add_submodule ("CrpN", alist, Syntax::State,
			"Nitrogen parameters.", CrpN::load_syntax);

  syntax.add ("enable_water_stress", Syntax::Boolean, Syntax::Const,
	      "Set this to true to let water stress limit production.\n\
OBSOLETE: Use water_stress_effect none instead.");
  alist.add ("enable_water_stress", true);
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
