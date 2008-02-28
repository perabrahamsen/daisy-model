// harvesting.C -- Harvest parameters for the default crop model.
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

#define BUILD_DLL

#include "harvesting.h"
#include "production.h"
#include "am.h"
#include "aom.h"
#include "crop.h"		// for Crop::DSremove.
#include "harvest.h"
#include "geometry.h"
#include "log.h"
#include "timestep.h"
#include "mathlib.h"
#include "submodel.h"
#include "check_range.h"
#include "submodeler.h"
#include <numeric>

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;
// Based on Penning de Vries et al. 1989, page 63
// E is the assimilate conversion effiency
static double DM_to_C_factor (double E)
{
   return 12.0/30.0 * (1.0 - (0.5673 - 0.5327 * E)) / E;
}

const Harvest&
Harvesting::harvest (const symbol column_name,
                     const symbol crop_name,
                     const std::vector<double>& density,
                     const Time& time,
                     const Geometry& geo,
                     Production& production,
                     double& DS,
                     const double stem_harvest,
                     const double leaf_harvest,
                     const double sorg_harvest,
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
                     double& water_stress_days,
                     double& nitrogen_stress_days)
{
  const double old_DM = production.DM ();

  // Import DM from production.
  const double total_old_W = production.WStem + production.WLeaf
    + production.WSOrg + production.WDead  + production.WRoot;
  
  const double total_old_N = production.NStem + production.NLeaf
    + production.NSOrg + production.NDead  + production.NRoot;
  daisy_assert (approximate (total_old_N,
                             production.NCrop + production.NDead));

  // Find C concentrations.
  const double C_C_Stem = DM_to_C_factor (production.E_Stem);
  const double C_C_Leaf = DM_to_C_factor (production.E_Leaf);
  const double C_C_Dead = C_C_Leaf;
  const double C_C_SOrg = DM_to_C_factor (production.E_SOrg);
  const double C_C_Root = DM_to_C_factor (production.E_Root);
  const double total_old_C = production.WSOrg * C_C_SOrg
    + production.WStem * C_C_Stem
    + production.WLeaf * C_C_Leaf
    + production.WDead * C_C_Dead
    + production.WRoot * C_C_Root;

  daisy_assert (approximate (production.CCrop,
                             total_old_C + production.CH2OPool * 12./30.));

  // Part of crop we attempt to harvest.
  const double dead_harvest = leaf_harvest;

  // Harvested yield and losses left in the field at harvest
  const double Stem_W_Yield
    = stem_harvest_frac * stem_harvest * production.WStem;
  const double Dead_W_Yield
    = stem_harvest_frac * dead_harvest * production.WDead;
  const double Leaf_W_Yield
    = leaf_harvest_frac * leaf_harvest * production.WLeaf;
  const double SOrg_W_Yield
    = sorg_harvest_frac * sorg_harvest * production.WSOrg;
  const double Stem_C_Yield = C_C_Stem * Stem_W_Yield;
  const double Dead_C_Yield = C_C_Dead * Dead_W_Yield;
  const double Leaf_C_Yield = C_C_Leaf * Leaf_W_Yield;
  const double SOrg_C_Yield = C_C_SOrg * SOrg_W_Yield;
  const double Stem_N_Yield
    = stem_harvest_frac * stem_harvest * production.NStem;
  const double Dead_N_Yield
    = stem_harvest_frac * dead_harvest * production.NDead;
  const double Leaf_N_Yield
    = leaf_harvest_frac * leaf_harvest * production.NLeaf;
  const double SOrg_N_Yield
    = sorg_harvest_frac * sorg_harvest * production.NSOrg;

  // Part of economic yield removed at harvest.
  const double WEYRm
    = EconomicYield_W * SOrg_W_Yield; 
  const double NEYRm
    = EconomicYield_N * SOrg_N_Yield; 
  const double CEYRm		// EY_W is used for both DM and C.
    = EconomicYield_W * SOrg_C_Yield;
  
  // The uneconomic yield is assumed to stay on field as loss. 
  const double Crop_W_Yield
    = Stem_W_Yield + Dead_W_Yield + Leaf_W_Yield + WEYRm;
  const double Crop_C_Yield
    = Stem_C_Yield + Dead_C_Yield + Leaf_C_Yield + CEYRm;
  const double Crop_N_Yield
    = Stem_N_Yield + Dead_N_Yield + Leaf_N_Yield + NEYRm;

  // Find losses.
  double Stem_W_Loss
    = (1.0 - stem_harvest_frac) * stem_harvest * production.WStem;
  double Dead_W_Loss
    = (1.0 - stem_harvest_frac) * dead_harvest * production.WDead;
  double Leaf_W_Loss
    = (1.0 - leaf_harvest_frac) * leaf_harvest * production.WLeaf;
  double SOrg_W_Loss
    = (1.0 - sorg_harvest_frac) * sorg_harvest * production.WSOrg
    + (1.0 - EconomicYield_W) * SOrg_W_Yield;
  double Root_W_Loss = 0.0;;
  double Crop_W_Loss 
    = Stem_W_Loss + Dead_W_Loss + Leaf_W_Loss + SOrg_W_Loss + Root_W_Loss;
  double Stem_C_Loss = Stem_W_Loss * C_C_Stem;
  double Dead_C_Loss = Dead_W_Loss * C_C_Dead;
  double Leaf_C_Loss = Leaf_W_Loss * C_C_Leaf;
  double SOrg_C_Loss = SOrg_W_Loss * C_C_SOrg;
  double Root_C_Loss = 0.0;
  double Crop_C_Loss 
    = Stem_C_Loss + Dead_C_Loss + Leaf_C_Loss + SOrg_C_Loss + Root_C_Loss;
  double Stem_N_Loss
    = (1.0 - stem_harvest_frac) * stem_harvest * production.NStem;
  double Dead_N_Loss
    = (1.0 - stem_harvest_frac) * dead_harvest * production.NDead;
  double Leaf_N_Loss
    = (1.0 - leaf_harvest_frac) * leaf_harvest * production.NLeaf;
  double SOrg_N_Loss
    = (1.0 - sorg_harvest_frac) * sorg_harvest * production.NSOrg 
    + (1.0 - EconomicYield_N) * SOrg_N_Yield;
  double Root_N_Loss = 0.0;
  double Crop_N_Loss 
    = Stem_N_Loss + Dead_N_Loss + Leaf_N_Loss + SOrg_N_Loss + Root_N_Loss;

  production.WStem -= (Stem_W_Yield + Stem_W_Loss);
  production.WDead -= (Dead_W_Yield + Dead_W_Loss);
  production.WLeaf -= (Leaf_W_Yield + Leaf_W_Loss);
  production.WSOrg -= (WEYRm + SOrg_W_Loss);
  production.NStem -= (Stem_N_Yield + Stem_N_Loss);
  production.NDead -= (Dead_N_Yield + Dead_N_Loss);
  production.NLeaf -= (Leaf_N_Yield + Leaf_N_Loss);
  production.NSOrg -= (NEYRm + SOrg_N_Loss);
  production.NCrop -= 
    (Crop_N_Yield + Crop_N_Loss - (Dead_N_Yield + Dead_N_Loss));
  production.CStem -= (Stem_C_Yield + Stem_C_Loss);
  production.CDead -= (Dead_C_Yield + Dead_C_Loss);
  production.CLeaf -= (Leaf_C_Yield + Leaf_C_Loss);
  production.CSOrg -= (CEYRm + SOrg_C_Loss);
  production.CCrop -= 
    (Crop_C_Yield + Crop_C_Loss - (Dead_C_Yield + Dead_C_Loss));

  production.WStem = std::max (0.0, production.WStem);
  production.WDead = std::max (0.0, production.WDead);
  production.WLeaf = std::max (0.0, production.WLeaf);
  production.WSOrg = std::max (0.0, production.WSOrg);
  production.NStem = std::max (0.0, production.NStem);
  production.NDead = std::max (0.0, production.NDead);
  production.NLeaf = std::max (0.0, production.NLeaf);
  production.NSOrg = std::max (0.0, production.NSOrg);
  production.NCrop = std::max (0.0, production.NCrop);

  // Put tiny losses away in the "Stem" pool.
  if (Dead_W_Loss < 0.1)
    {
      Stem_W_Loss += Dead_W_Loss;
      Stem_C_Loss += Dead_C_Loss;
      Stem_N_Loss += Dead_N_Loss;
      Dead_W_Loss = 0.0;
      Dead_C_Loss = 0.0;
      Dead_N_Loss = 0.0;
    }
  if (Leaf_W_Loss < 0.1)
    {
      Stem_W_Loss += Leaf_W_Loss;
      Stem_C_Loss += Leaf_C_Loss;
      Stem_N_Loss += Leaf_N_Loss;
      Leaf_W_Loss = 0.0;
      Leaf_C_Loss = 0.0;
      Leaf_N_Loss = 0.0;
    }
  if (SOrg_W_Loss < 0.1)
    {
      Stem_W_Loss += SOrg_W_Loss;
      Stem_C_Loss += SOrg_C_Loss;
      Stem_N_Loss += SOrg_N_Loss;
      SOrg_W_Loss = 0.0;
      SOrg_C_Loss = 0.0;
      SOrg_N_Loss = 0.0;
    }

  // Add crop remains to the soil.
  static const symbol stem_symbol ("stem");
  AM& AM_stem = AM::create (geo.cell_size (), time, Stem, crop_name, stem_symbol);
  residuals.push_back (&AM_stem);
  if (Stem_W_Loss > 0.0)
    {
      const double C = C_C_Stem * Stem_W_Loss;
      const double N = Stem_N_Loss;
      AM_stem.add (C * m2_per_cm2, N * m2_per_cm2);
      daisy_assert (iszero (C) || N > 0.0);
      production.C_AM += C;
      production.N_AM += N;
    }

  if (!production.AM_leaf)
    {
      static const symbol dead_symbol ("dead");
      production.AM_leaf
	= &AM::create (geo.cell_size (), time, Dead, crop_name, dead_symbol, 
		       AM::Unlocked /* no organic matter */);
    }
  if (Dead_W_Loss > 0.0)
    {
      const double C = C_C_Dead * Dead_W_Loss;
      const double N = Dead_N_Loss;
      production.AM_leaf->add (C * m2_per_cm2, N * m2_per_cm2);
      daisy_assert (iszero (C) || N > 0.0);
      production.C_AM += C;
      production.N_AM += N;
    }

  static const symbol leaf_symbol ("leaf");
  AM& AM_leaf = AM::create (geo.cell_size (),
                            time, Leaf, crop_name, leaf_symbol);
  residuals.push_back (&AM_leaf);
  if (Leaf_W_Loss > 0.0)
    {
      const double C = C_C_Leaf * Leaf_W_Loss;
      const double N = Leaf_N_Loss;
      daisy_assert (iszero (C) || N > 0.0);
      AM_leaf.add ( C * m2_per_cm2, N * m2_per_cm2);
      production.C_AM += C;
      production.N_AM += N;
    }

  static const symbol sorg_symbol ("sorg");
  AM& AM_sorg = AM::create (geo.cell_size (),
                            time, SOrg, crop_name, sorg_symbol);
  residuals.push_back (&AM_sorg);
  if (SOrg_W_Loss > 0.0)
    {
      const double C = C_C_SOrg * SOrg_W_Loss;
      const double N = SOrg_N_Loss;
      daisy_assert (iszero (C) || N > 0.0);
      AM_sorg.add ( C * m2_per_cm2, N * m2_per_cm2);
      production.C_AM += C;
      production.N_AM += N;
    }

  // Check mass balance so far.
  double total_new_W = production.WSOrg + production.WStem
    + production.WLeaf + production.WDead + production.WRoot;
  daisy_assert (approximate (total_old_W,
                             total_new_W + Crop_W_Yield + Crop_W_Loss));
  double total_new_C = production.WSOrg * C_C_SOrg 
    + production.WStem * C_C_Stem
    + production.WLeaf * C_C_Leaf
    + production.WDead * C_C_Dead
    + production.WRoot * C_C_Root;
  daisy_assert (approximate (total_old_C,
                             total_new_C + Crop_C_Yield + Crop_C_Loss));
  double total_new_N = production.NCrop + production.NDead;
  daisy_assert (approximate (total_old_N,
                             total_new_N + Crop_N_Yield + Crop_N_Loss));

  // Dead or alive?
  if (!kill_off && DS < DSmax
      // It may survive by canibalizing its own roots.
      // && stem_harvest < 0.99
      )
    {
      // Cut delay.
      const double removed_DM = old_DM - production.DM ();
      production_delay = cut_delay (removed_DM);
      last_cut = time;
    }
  else
    {
      DS = Crop::DSremove;

      // Create root AM if missing.
      static const symbol root_symbol ("root");
      if (!production.AM_root)
	production.AM_root = &AM::create (geo.cell_size (), time, Root,
					  crop_name, root_symbol, 
					  AM::Unlocked /* inorganic */);


      // Add crop to residuals.
      double extra_C = production.CH2OPool * (12./30.);
      double extra_N = 0.0;
      if (production.WStem < 0.1)
	{
	  extra_C += C_C_Stem * production.WStem;
	  extra_N += production.NStem;
	}
      else
	AM_stem.add (C_C_Stem * production.WStem * m2_per_cm2, 
		     production.NStem * m2_per_cm2);
      Stem_W_Loss += production.WStem;
      Stem_C_Loss += production.WStem * C_C_Stem;
      Stem_N_Loss += production.NStem;

      if (production.WDead < 0.1)
	{
	  extra_C += C_C_Dead * production.WDead;
	  extra_N += production.NDead;
	}
      else
	production.AM_leaf->add (C_C_Dead * production.WDead * m2_per_cm2, 
				 production.NDead * m2_per_cm2);
      Dead_W_Loss += production.WDead;
      Dead_C_Loss += production.WDead * C_C_Dead;
      Dead_N_Loss += production.NDead;
      if (production.WLeaf < 0.1)
	{
	  extra_C += C_C_Leaf * production.WLeaf;
	  extra_N += production.NLeaf;
	}
      else
	AM_leaf.add (C_C_Leaf * production.WLeaf * m2_per_cm2, 
		     production.NLeaf * m2_per_cm2);
      Leaf_W_Loss += production.WLeaf;
      Leaf_C_Loss += production.WLeaf * C_C_Leaf;
      Leaf_N_Loss += production.NLeaf;
      if (production.WSOrg < 0.1)
	{
	  extra_C += C_C_SOrg * production.WSOrg;
	  extra_N += production.NSOrg;
	}
      else
	AM_sorg.add (C_C_SOrg * production.WSOrg * m2_per_cm2, 
		     production.NSOrg * m2_per_cm2);
      SOrg_W_Loss += production.WSOrg;
      SOrg_C_Loss += production.WSOrg * C_C_SOrg;
      SOrg_N_Loss += production.NSOrg;

      daisy_assert (iszero (production.WRoot)
                    || production.NRoot > 0.0);
      const double Root_C
        = (production.WRoot * C_C_Root + extra_C) * m2_per_cm2;
      const double Root_N = (production.NRoot + extra_N) * m2_per_cm2;
      if (accumulate (density.begin (), density.end (), 0.0) > 0.0)
	{
	  production.AM_root->add_surface (geo, Root_C, Root_N, density);
	  geo.add_surface (residuals_N_soil, density, Root_N);
	  geo.add_surface (residuals_C_soil, density, Root_C);
	}
      else
	{
	  production.AM_root->add (Root_C, Root_N);
	  residuals_N_top += production.NRoot + extra_N;
	  residuals_C_top += production.WRoot * C_C_Root + extra_C;
	}
      Root_W_Loss = production.WRoot;
      Root_C_Loss = production.WRoot * C_C_Root;
      Root_N_Loss = production.NRoot;
      Crop_W_Loss = Stem_W_Loss + Dead_W_Loss + Leaf_W_Loss + SOrg_W_Loss
	+ Root_W_Loss;
      Crop_C_Loss = Stem_C_Loss + Dead_C_Loss + Leaf_C_Loss + SOrg_C_Loss
	+ Root_C_Loss;
      Crop_N_Loss = Stem_N_Loss + Dead_N_Loss + Leaf_N_Loss + SOrg_N_Loss
	+ Root_N_Loss;
      production.WStem = production.WDead =  production.WLeaf 
	= production.WSOrg = production.WRoot = 0.0;
      production.CStem = production.CDead =  production.CLeaf 
	= production.CSOrg = production.CRoot = 0.0;
      production.NStem = production.NDead =  production.NLeaf 
	= production.NSOrg = production.NRoot = 0.0;
      production.NCrop = 0.0;
      total_new_W = 0.0;
      total_new_C = 0.0;
      total_new_N = 0.0;

      // Check mass balance.
      daisy_assert (approximate (total_old_W, 
				 total_new_W + Crop_W_Yield + Crop_W_Loss));
      daisy_assert (approximate (total_old_C, 
				 total_new_C + Crop_C_Yield + Crop_C_Loss));

      daisy_assert (approximate (total_old_N,
				 total_new_N + Crop_N_Yield + Crop_N_Loss));


      // Unlock and remove locked AM's.
      if (production.AM_root->locked ())
	production.AM_root->unlock (); // Stored in organic matter.
      else
	residuals.push_back (production.AM_root);	// No organic matter.
      production.AM_root = NULL;

      daisy_assert (production.AM_leaf);
      if (production.AM_leaf->locked ())
	production.AM_leaf->unlock (); // Stored in organic matter.
      else
	residuals.push_back (production.AM_leaf);// No organic matter.
      production.AM_leaf = NULL;
    }

  // Note: We can't check losses, as some go directy to organic matter.
  residuals_DM += Crop_W_Loss;
  residuals_N_top += Crop_N_Loss - Root_N_Loss;	// Root already added.
  residuals_C_top += Crop_C_Loss - Root_C_Loss;

  // We need to update the crop carbon values for production mass balance.
  production.update_carbon ();

  // Reset stress.
  const double wsd = water_stress_days;
  const double nsd = nitrogen_stress_days;
  water_stress_days = nitrogen_stress_days = 0.0;

  // Return harvest.
  if (combine)
    return *new Harvest (column_name, time, crop_name,
                         Stem_W_Yield + Dead_W_Yield + Leaf_W_Yield + WEYRm,
                         Stem_N_Yield + Dead_N_Yield + Leaf_N_Yield + NEYRm, 
                         Stem_C_Yield + Dead_C_Yield + Leaf_C_Yield + CEYRm,
                         0.0, 0.0, 0.0,
                         0.0, 0.0, 0.0,
                         0.0, 0.0, 0.0,
                         wsd, nsd);
  else
    return *new Harvest (column_name, time, crop_name,
                         Stem_W_Yield, Stem_N_Yield, Stem_C_Yield,
                         Dead_W_Yield, Dead_N_Yield, Dead_C_Yield,
                         Leaf_W_Yield, Leaf_N_Yield, Leaf_C_Yield,
                         WEYRm, NEYRm, CEYRm, wsd, nsd);
  
}

void
Harvesting::tick (const Time& time)
{ 
  if (production_delay < 0.01)
    cut_stress = 0.0;
  else
    {
      if (last_cut == Time::null ())
        last_cut = time;
      const Timestep step = time - last_cut;
      const double days_between = step.total_hours () / 24.0;
      daisy_assert (days_between >= 0.0);
      
      if (days_between >= production_delay)
	{
	  cut_stress = 0.0;
	  production_delay = 0.0;
	}
      else
	{
	  cut_stress = 1.0 - (exp (M_LN2 / production_delay * days_between)
			      - 1.0);
	  daisy_assert (cut_stress >= 0.0 && cut_stress <= 1.0);
	}
    }
}

void 
Harvesting::output (Log& log) const
{ 
  if (last_cut != Time::null ())
    output_submodule (last_cut, "last_cut", log);
  output_variable (production_delay, log);
  output_variable (cut_stress, log);
}

void 
Harvesting::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Submodel.
  alist.add ("submodel", "Harvesting");
  alist.add ("description", 
	     "Information about what happens to the crop at harvest and cut.");

  syntax.add_submodule_sequence ("Stem", Syntax::Const, 
				 "Stem AM parameters.", AOM::load_syntax);
  syntax.add_check ("Stem", AM::check_om_pools ());
  alist.add ("Stem", AM::default_AM ());
  syntax.add_submodule_sequence ("Leaf", Syntax::Const,
				 "Leaf AM parameters.", AOM::load_syntax);
  syntax.add_check ("Leaf", AM::check_om_pools ());
  alist.add ("Leaf", AM::default_AM ());
  syntax.add_submodule_sequence ("Dead", Syntax::Const,
				 "Dead leaves AM parameters.",
				 AOM::load_syntax);
  syntax.add_check ("Dead", AM::check_om_pools ());
  alist.add ("Dead", AM::default_AM ());
  syntax.add_submodule_sequence ("SOrg", Syntax::Const,
				 "Storage organ AM parameters.", 
				 AOM::load_syntax);
  syntax.add_check ("SOrg", AM::check_om_pools ());
  alist.add ("SOrg", AM::default_AM ());
  syntax.add_submodule_sequence ("Root", Syntax::Const,
				 "Root AM parameters.", AOM::load_syntax);
  syntax.add_check ("Root", AM::check_om_pools ());
  alist.add ("Root", AM::default_AM ());
  syntax.add ("EconomicYield_W", Syntax::None (), Syntax::Const, "\
Valuable fraction of storage organ (DM), e.g. grain or tuber.");
  alist.add ("EconomicYield_W", 1.00);
  syntax.add ("EconomicYield_N", Syntax::None (), Syntax::OptionalConst,
               "Valuable fraction of storage organ (N).\n\
By default the value for DM is used.");
  syntax.add ("DSmax", Syntax::None (), Check::non_negative (), 
	      Syntax::Const, "\
Maximal development stage for which the crop survives harvest.");
  alist.add ("DSmax", 0.80);
  syntax.add ("DSnew", Syntax::None (), Check::non_negative (),
              Syntax::OptionalConst,
	      "New development stage after harvest.\n\
If not specified, use the DS where an uncut crop would first reach the\n\
height it now has after the cut.  I.e. it uses the inverse function of\n\
the HvsDS Canopy parameter to find the new DS.");
  syntax.add_submodule ("last_cut", alist, Syntax::OptionalState,
			"Date of last cut.  Used for calculating cut delay.",
			Time::load_syntax);
  syntax.add ("production_delay", "d", Syntax::State,
	      "production delay caused by last cut");
  alist.add ("production_delay", 0.0);
  syntax.add ("cut_delay", "kg DM/ha", "d", Syntax::Const,
	      "\
Production and development delay in days as a function of the shoot DM\n\
removed by harvest.  By default, there is no delay.");
  PLF no_delay;
  no_delay.add (0.0, 0.0);
  no_delay.add (1.0, 0.0);
  alist.add ("cut_delay", no_delay);
  syntax.add_fraction ("cut_stress", Syntax::LogOnly, 
		       "Stress induced due to last cut.");
  syntax.add ("sorg_height", "cm", Syntax::OptionalConst, 
              "Vertical location of storage organ.\n\
Set this to a negative number for root fruits, this will cause harvesting\n\
to imply a suitable tillage operation, and guarentee that harvest will kill\n\
the plant.  By default, the storage organ is assumed to be located far\n\
above ground.");
}

Harvesting::Harvesting (Block& al)
  : Stem (al.alist_sequence ("Stem")),
    Leaf (al.alist_sequence ("Leaf")),
    Dead (al.alist_sequence ("Dead")),
    SOrg (al.alist_sequence ("SOrg")),
    Root (al.alist_sequence ("Root")),
    EconomicYield_W (al.number ("EconomicYield_W")),
    EconomicYield_N (al.check ("EconomicYield_N")
                     ? al.number ("EconomicYield_N")
                     : al.number ("EconomicYield_W")),
    DSmax (al.number ("DSmax")),
    DSnew (al.number ("DSnew", -44.0)),
    last_cut (al.check ("last_cut")
              ? submodel_value<Time> (al, "last_cut") 
              : Time::null ()),
    production_delay (al.number ("production_delay")),
    cut_delay (al.plf ("cut_delay")),
    cut_stress (0.0),
    sorg_height (al.number ("sorg_height", 42.42e42))
{ }

Harvesting::~Harvesting ()
{  }

static Submodel::Register 
soil_submodel ("Harvesting", Harvesting::load_syntax);
