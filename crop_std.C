// crop_std.C
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

class CropStandard : public Crop
{
  // Content.
public:
  RootSystem& root_system;
  CanopyStandard& canopy;
  Harvesting& harvesting;
  Production& production;
  Phenology& development;
  const Partition& partition;
  Vernalization& vernalization;
  const Photosynthesis& photosynthesis;
  CrpN& nitrogen;
  const bool enable_water_stress;
  const bool enable_N_stress;

  // Communication with Bioclimate.
public:
#if 0
  double water_stress () const // [0-1] (0 = full production)
  { return root_system.water_stress; }
  double nitrogen_stress () const // [0-1] (1 = no production)
  { return root_system.nitrogen_stress; }
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }
#endif
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
  { canopy.CanopyStructure (development.DS); }
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
  const Harvest& harvest (const string& column_name,
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
  void output (Log&) const;

  // Queries.
  double DS () const
  { return development.DS; }
  double DM () const
  { return production.DM (); }
  double total_N () const
  { return production.total_N (); }

  // Create and Destroy.
public:
  void initialize_organic (Treelog&, const Geometry& geometry, OrganicMatter&);
  void initialize_inorganic (Treelog&, const Geometry& geometry);
  CropStandard (const AttributeList& vl);
  ~CropStandard ();
};

void
CropStandard::initialize_organic (Treelog& msg, const Geometry& geometry,
				  OrganicMatter& organic_matter)
{
  root_system.initialize (geometry.size ());
  production.initialize (nitrogen.SeedN);

  if (development.DS >= 0)
    {
      // Dead organic matter.
      production.initialize (name, harvesting.Root, harvesting.Dead,
			     geometry, organic_matter);
      
      // Update derived state content.
      canopy.tick (production.WLeaf, production.WSOrg, 
		   production.WStem, development.DS, 
		   // We don't save the forced CAI, use simulated CAI
		   //  until midnight (small error).
		   -1.0);
      root_system.set_density (msg, 
			       geometry, production.WRoot, development.DS);
      nitrogen.content (development.DS, production);
    }
}

void
CropStandard::initialize_inorganic (Treelog&, const Geometry& geometry)
{
  root_system.initialize (geometry.size ());
  production.initialize (nitrogen.SeedN);

  if (development.DS >= 0)
    nitrogen.content (development.DS, production);
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

  // Update partial_soil_temperature and pressure potential.
  development.partial_soil_temperature +=
    soil_heat.T (soil.interval_plus (-root_system.DptEmr));
  development.soil_h =
    soil_water.h (soil.interval_plus (-root_system.DptEmr/2.));

  if (time.hour () == 0 && development.DS <= 0)
    {
      daisy_assert (ForcedCAI < 0.0);
      // Calculate average soil temperature.
      development.soil_temperature =
	development.partial_soil_temperature / 24.0;
      development.partial_soil_temperature = 0.0;

      development.emergence ();
      if (development.DS >= 0)
	{
	  msg.message ("==> emerging");
	  canopy.tick (production.WLeaf, production.WSOrg,
		       production.WStem, development.DS, -1.0);
	  nitrogen.content (development.DS, production);
	  root_system.tick (msg, soil, soil_heat, 
			    production.WRoot, 0.0, development.DS);

	  if (organic_matter)
	    {
	      if (!production.AM_root)
		{
		  production.AM_root
		    = &AM::create (soil, time, harvesting.Root,
				   name, "root", AM::Locked);
		  organic_matter->add (*production.AM_root);
		}
	      if (!production.AM_leaf)
		{
		  production.AM_leaf
		    = &AM::create (soil, time, harvesting.Dead,
				   name, "dead", AM::Locked);
		  organic_matter->add (*production.AM_leaf);
		}
	    }
	  else
	    {
	      if (!production.AM_root)
		production.AM_root
		  = &AM::create (soil, time, harvesting.Root,
				 name, "root", AM::Unlocked);
	      if (!production.AM_leaf)
		production.AM_leaf
		  = &AM::create (soil, time, harvesting.Dead,
				 name, "dead", AM::Unlocked);
	    }
	}
      return;
    }
  if (development.DS <= 0 || development.DS >= 2)
    return;

  if (soil_NO3)
    {
      daisy_assert (soil_NH4);
      nitrogen.update (time.hour (), production.NCrop, development.DS,
		       enable_N_stress,
		       soil, soil_water, *soil_NH4, *soil_NO3,
		       root_system);
    }
  else
    {
      daisy_assert (!soil_NH4);
      production.NCrop = nitrogen.PtNCnt;
    }  
  const double nitrogen_stress = root_system.nitrogen_stress;
  const double water_stress = root_system.water_stress;

  if (bioclimate.PAR (bioclimate.NumberOfIntervals () - 1) > 0)
    {
      double Ass = photosynthesis (bioclimate, canopy, development, msg);
      production.PotCanopyAss = Ass;
      if (root_system.production_stress >= 0.0)
	Ass *= (1.0 - root_system.production_stress);
      else if (enable_water_stress)
	Ass *= (1.0 - water_stress);
      if (enable_N_stress)
	Ass *= (1.0 - nitrogen_stress);
      Ass *= (1.0 - harvesting.cut_stress);
      production.CanopyAss = Ass;
      const double ProdLim = (1.0 - production.GrowthRateRedFac);
      production.CH2OPool += ProdLim * Ass;
    }
  else
      production.CanopyAss = 0.0;

  production.tick (bioclimate.daily_air_temperature (),
		   soil_heat.T (soil.interval_plus (-root_system.Depth / 3.0)),
		   root_system.Density, soil, development.DS, 
		   canopy.CAImRat, nitrogen, partition, 
		   residuals_DM, residuals_N_top, residuals_C_top,
		   residuals_N_soil, residuals_C_soil, msg);
  nitrogen.content (development.DS, production);
  if (time.hour () != 0)
    return;

  canopy.tick (production.WLeaf, production.WSOrg,
	       production.WStem, development.DS, ForcedCAI);

  development.tick_daily (bioclimate.daily_air_temperature (), 
			  production.WLeaf, production, vernalization,
			  harvesting.cut_stress, msg);
  root_system.tick (msg, soil, soil_heat, 
		    production.WRoot, production.IncWRoot, development.DS);
}

const Harvest&
CropStandard::harvest (const string& column_name,
		       const Time& time,
		       const Geometry& geometry,
		       Bioclimate& bioclimate,
		       double stub_length,
		       double stem_harvest_frac,
		       double leaf_harvest_frac,
		       double sorg_harvest_frac,
		       bool kill_off,
		       vector<AM*>& residuals,
		       double& residuals_DM,
		       double& residuals_N_top, double& residuals_C_top,
		       vector<double>& residuals_N_soil,
		       vector<double>& residuals_C_soil,
		       Treelog& msg)
{
  Treelog::Open nest (msg, name + " harvest");

  // Update nitrogen content.
  nitrogen.content (development.DS, production);

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
		  time, geometry, production, development.DS,
		  stem_harvest, leaf_harvest, chemicals,
		  stem_harvest_frac, leaf_harvest_frac, sorg_harvest_frac,
		  kill_off, residuals, residuals_DM,
		  residuals_N_top, residuals_C_top, 
		  residuals_N_soil, residuals_C_soil);

  if (development.DS != DSremove)
    {
      nitrogen.cut (development.DS); // Stop fixation.

      if (development.DS > 0.0)
	{
	  // Revert development.
	  if (harvesting.DSnew < 0.0)
	    {

#if 0
	      // Negative value means revert to canopy 
	      if (stub_length < canopy.Height)
		development.DS = canopy.DS_at_height (stub_length);
#else
	      // We want a cut to always put back development, even
	      // for a crop which is so retarded that it has no stem
	      // worth speaking about, and hence, no height.
	      const double DS_height = canopy.DS_at_height (stub_length);
	      if (development.DS > DS_height)
		development.DS = DS_height;
#endif
	      
	    }
	  else if (development.DS > harvesting.DSnew)
	    development.DS = harvesting.DSnew;
	  
	  // Cut canopy.
	  canopy.cut (production.WStem, development.DS, stub_length);

	  daisy_assert (approximate (canopy.CropHeight (production.WStem,
							development.DS), 
				     canopy.Height));
	  canopy.CropCAI (production.WLeaf, production.WSOrg,
			  production.WStem, development.DS);
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
  if (log.check_member ("Prod"))
    {
      Log::Open open (log, "Prod");
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
    root_system (*new RootSystem (al.alist ("Root"))),
    canopy (*new CanopyStandard (al.alist ("Canopy"))),
    harvesting (*new Harvesting (al.alist ("Harvest"))),
    production (*new Production (al.alist ("Prod"))),
    development (Librarian<Phenology>::create (al.alist ("Devel"))),
    partition (*new Partition (al.alist ("Partit"))),
    vernalization (*new Vernalization (al.check ("Vernal") 
				       ? al.alist ("Vernal")
				       : Vernalization::no_vernalization ())),
    photosynthesis (*new Photosynthesis (al.alist ("LeafPhot"))),
    nitrogen (*new CrpN (al.alist ("CrpN"))),
    enable_water_stress (al.flag ("enable_water_stress")),
    enable_N_stress (al.flag ("enable_N_stress"))
{ }

CropStandard::~CropStandard ()
{
  delete &root_system;
  delete &canopy;
  delete &harvesting;
  delete &production;
  delete &development;
  delete &partition;
  delete &vernalization;
  delete &photosynthesis;
  delete &nitrogen;
}

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
	      "Set this to true to let water stress limit production.");
  alist.add ("enable_water_stress", true);
  syntax.add ("enable_N_stress", Syntax::Boolean, Syntax::Const,
	      "Set this true to let nitrogen stress limit production.");
  alist.add ("enable_N_stress", true);

  Librarian<Crop>::add_type ("default", alist, syntax, &make);
}
