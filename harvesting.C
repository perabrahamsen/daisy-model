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

#include "harvesting.h"
#include "production.h"
#include "am.h"
#include "om.h"
#include "crop.h"		// for Crop::DSremove.
#include "harvest.h"
#include "log.h"
#include "mathlib.h"
#include "submodel.h"

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
Harvesting::operator() (const string& column_name,
			const string& crop_name,
			const vector<double>& density,
			const Time& time,
			const Geometry& geometry,
			Production& production,
			double& DS,
			const double stem_harvest,
			const double leaf_harvest,
			const Chemicals& chemicals,
			const double stem_harvest_frac,
			const double leaf_harvest_frac,
			const double sorg_harvest_frac,
			const bool kill_off,
			vector<AM*>& residuals)
{

  // Import DM from production.
  const double WStem = production.WStem;
  const double WLeaf = production.WLeaf;
  const double WSOrg = production.WSOrg;
  const double WRoot = production.WRoot;
  const double WDead = production.WDead;
  const double NStem = production.NStem;
  const double NLeaf = production.NLeaf;
  const double NSOrg = production.NSOrg;
  const double NRoot = production.NRoot;
  const double NDead = production.NDead;
  const double old_DM = production.DM ();
  
  // Find C concentrations.
  const double C_C_Stem = DM_to_C_factor (production.E_Stem);
  const double C_C_Leaf = DM_to_C_factor (production.E_Leaf);
  const double C_C_Dead = C_C_Leaf;
  const double C_C_SOrg = DM_to_C_factor (production.E_SOrg);
  const double C_C_Root = DM_to_C_factor (production.E_Root);

  // Part of crop we attempt to harvest.
  const double dead_harvest = 1.0;
  const double sorg_harvest = 1.0;

  // Harvested yield and losses left in the field at harvest
  const double Stem_W_Yield = stem_harvest_frac * stem_harvest * WStem;
  const double Dead_W_Yield = stem_harvest_frac * dead_harvest * WDead;
  const double Leaf_W_Yield = leaf_harvest_frac * leaf_harvest * WLeaf;
  const double SOrg_W_Yield = sorg_harvest_frac * sorg_harvest * WSOrg;
  const double Stem_C_Yield = C_C_Stem * Stem_W_Yield;
  const double Dead_C_Yield = C_C_Dead * Dead_W_Yield;
  const double Leaf_C_Yield = C_C_Leaf * Leaf_W_Yield;
  const double SOrg_C_Yield = C_C_SOrg * SOrg_W_Yield;
  const double Stem_N_Yield = stem_harvest_frac * stem_harvest * NStem;
  const double Dead_N_Yield = stem_harvest_frac * dead_harvest * NDead;
  const double Leaf_N_Yield = leaf_harvest_frac * leaf_harvest * NLeaf;
  const double SOrg_N_Yield = sorg_harvest_frac * sorg_harvest * NSOrg;

  // Part of economic yield removed at harvest
  const double WEYRm
    = EconomicYield_W * SOrg_W_Yield; 
  const double NEYRm
    = EconomicYield_N * SOrg_N_Yield; 
  const double CEYRm		// EY_W is used for both DM and C.
    = EconomicYield_W * SOrg_C_Yield;

  const double Crop_N_Yield
    = Stem_N_Yield + Dead_N_Yield + Leaf_N_Yield + NEYRm;

  // Find losses.
  double Stem_W_Loss = (1.0 - stem_harvest_frac) * stem_harvest * WStem;
  double Dead_W_Loss = (1.0 - stem_harvest_frac) * dead_harvest * WDead;
  double Leaf_W_Loss = (1.0 - leaf_harvest_frac) * leaf_harvest * WLeaf;
  double SOrg_W_Loss = (1.0 - sorg_harvest_frac) * sorg_harvest * WSOrg
    + (1.0 - EconomicYield_W) * SOrg_W_Yield;
  double Stem_N_Loss = (1.0 - stem_harvest_frac) * stem_harvest * NStem;
  double Dead_N_Loss = (1.0 - stem_harvest_frac) * dead_harvest * NDead;
  double Leaf_N_Loss = (1.0 - leaf_harvest_frac) * leaf_harvest * NLeaf;
  double SOrg_N_Loss = (1.0 - sorg_harvest_frac) * sorg_harvest * NSOrg 
    + (1.0 - EconomicYield_N) * SOrg_N_Yield;
  const double Crop_N_Loss 
    = Stem_N_Loss + Dead_N_Loss + Leaf_N_Loss + SOrg_N_Loss;

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

  production.WStem = max(0.0, production.WStem);
  production.WDead = max(0.0, production.WDead);
  production.WLeaf = max(0.0, production.WLeaf);
  production.WSOrg = max(0.0, production.WSOrg);
  production.NStem = max(0.0, production.NStem);
  production.NDead = max(0.0, production.NDead);
  production.NLeaf = max(0.0, production.NLeaf);
  production.NSOrg = max(0.0, production.NSOrg);
  production.NCrop = max(0.0, production.NCrop);

  // Put tiny losses away in the "Stem" pool.
  if (Dead_W_Loss < 0.1)
    {
      Stem_W_Loss += Dead_W_Loss;
      Stem_N_Loss += Dead_N_Loss;
      Dead_W_Loss = 0.0;
      Dead_N_Loss = 0.0;
    }
  if (Leaf_W_Loss < 0.1)
    {
      Stem_W_Loss += Leaf_W_Loss;
      Stem_N_Loss += Leaf_N_Loss;
      Leaf_W_Loss = 0.0;
      Leaf_N_Loss = 0.0;
    }
  if (SOrg_W_Loss < 0.1)
    {
      Stem_W_Loss += SOrg_W_Loss;
      Stem_N_Loss += SOrg_N_Loss;
      SOrg_W_Loss = 0.0;
      SOrg_N_Loss = 0.0;
    }

  // Add crop remains to the soil.
  if (Stem_W_Loss > 0.0)
    {
      const double C = C_C_Stem * Stem_W_Loss;
      const double N = Stem_N_Loss;
      AM& am = AM::create (geometry, time, Stem, crop_name, "stem");
      am.add (C * m2_per_cm2, N * m2_per_cm2);
      assert (C == 0.0 || N > 0.0);
      residuals.push_back (&am);
      production.C_AM += C;
      production.N_AM += N;
    }
 if (Dead_W_Loss > 0.0)
   {
     const double C = C_C_Dead * Dead_W_Loss;
     const double N = Dead_N_Loss;
     if (!production.AM_leaf)
        production.AM_leaf
	  = &AM::create (geometry, time, Dead, crop_name, "dead", 
			 AM::Unlocked);
     production.AM_leaf->add (C * m2_per_cm2, N * m2_per_cm2);
     assert (C == 0.0 || N > 0.0);
     production.C_AM += C;
     production.N_AM += N;
   }
 if (Leaf_W_Loss > 0.0)
   {
     const double C = C_C_Leaf * Leaf_W_Loss;
     const double N = Leaf_N_Loss;
     AM& am = AM::create (geometry, time, Leaf, crop_name, "leaf");
     assert (C == 0.0 || N > 0.0);
     am.add ( C * m2_per_cm2, N * m2_per_cm2);
     residuals.push_back (&am);
     production.C_AM += C;
     production.N_AM += N;
   }
 if (SOrg_W_Loss > 0.0)
   {
     const double C = C_C_SOrg * SOrg_W_Loss;
     const double N = SOrg_N_Loss;
     AM& am = AM::create (geometry, time, SOrg, crop_name, "sorg");
     assert (C == 0.0 || N > 0.0);
     am.add ( C * m2_per_cm2, N * m2_per_cm2);
     residuals.push_back (&am);
     production.C_AM += C;
     production.N_AM += N;
   }

  // Dead or alive?
  if (!kill_off && DS < DSmax && leaf_harvest < 1.0)
    {
      if (DS > DSnew)
	DS = DSnew;

      // Cut delay.
      const double new_DM = production.DM ();
      const double removed_DM = old_DM - new_DM;
      production_delay = cut_delay (removed_DM);
      if (!last_cut)
	last_cut = new Time (time);
      else
	*last_cut = time;
    }
  else
    {
      DS = Crop::DSremove;

     // Update and unlock locked AMs.
      if (!production.AM_root)
	production.AM_root = &AM::create (geometry, time, Root,
					crop_name, "root", AM::Unlocked);
      if (accumulate (density.begin (), density.end (), 0.0) > 0.0)
	production.AM_root->add (geometry,
			       WRoot * C_C_Root * m2_per_cm2,
			       NRoot * m2_per_cm2,
			       density);
      else
	production.AM_root->add (WRoot * C_C_Root * m2_per_cm2,
			       NRoot * m2_per_cm2);
      assert (WRoot == 0.0 || NRoot > 0.0);
      if (production.AM_root->locked ())
	production.AM_root->unlock (); // Stored in organic matter.
      else
	residuals.push_back (production.AM_root);	// No organic matter.
      production.AM_root = NULL;

      if (production.AM_leaf)
	{
	  if (production.AM_leaf->locked ())
	    production.AM_leaf->unlock (); // Stored in organic matter.
	  else
	    residuals.push_back (production.AM_leaf);// No organic matter.
	  production.AM_leaf = NULL;
	}
    }

  return *new Harvest (column_name, time, crop_name,
		       Stem_W_Yield, Stem_N_Yield, Stem_C_Yield,
		       Dead_W_Yield, Dead_N_Yield, Dead_C_Yield,
		       Leaf_W_Yield, Leaf_N_Yield, Leaf_C_Yield,
		       WEYRm, NEYRm, CEYRm, chemicals);
}

void
Harvesting::tick (const Time& time)
{ 
  if (production_delay < 0.01)
    cut_stress = 0.0;
  else
    {
      assert (last_cut);
      const double days_between 
	= (0.0 + Time::hours_between (*last_cut, time)) / 24.0;
      assert (days_between >= 0.0);
      
      if (days_between >= production_delay)
	{
	  cut_stress = 0.0;
	  production_delay = 0.0;
	}
      else
	{
	  cut_stress = 1.0 - (exp (M_LN2 / production_delay * days_between)
			      - 1.0);
	  assert (cut_stress >= 0.0 && cut_stress <= 1.0);
	}
    }
}

void 
Harvesting::output (Log& log) const
{ 
  if (last_cut)
    log.output ("last_cut", *last_cut);
  log.output ("production_delay", production_delay);
  log.output ("cut_stress", cut_stress);
}

void 
Harvesting::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_submodule_sequence ("Stem", Syntax::Const, 
				 "Stem AOM parameters.", OM::load_syntax);
  alist.add ("Stem", AM::default_AOM ());
  syntax.add_submodule_sequence ("Leaf", Syntax::Const,
				 "Leaf AM parameters.", OM::load_syntax);
  alist.add ("Leaf", AM::default_AOM ());
  syntax.add_submodule_sequence ("Dead", Syntax::Const,
				 "Dead leaves AM parameters.",
				 OM::load_syntax);
  alist.add ("Dead", AM::default_AOM ());
  syntax.add_submodule_sequence ("SOrg", Syntax::Const,
				 "Storage organ AM parameters.", 
				 OM::load_syntax);
  alist.add ("SOrg", AM::default_AOM ());
  syntax.add_submodule_sequence ("Root", Syntax::Const,
				 "Root AM parameters.", OM::load_syntax);
  alist.add ("Root", AM::default_AOM ());
  syntax.add ("EconomicYield_W", Syntax::None (), Syntax::Const, "\
Valuable fraction of storage organ (DM), e.g. grain or tuber.");
  alist.add ("EconomicYield_W", 1.00);
  syntax.add ("EconomicYield_N", Syntax::None (), Syntax::OptionalConst,
               "Valuable fraction of storage organ (N).\n\
By default the value for DM is used.");
  syntax.add ("DSmax", Syntax::None (), Syntax::Const, "\
Maximal development stage for which the crop survives harvest.");
  alist.add ("DSmax", 0.80);
  syntax.add ("DSnew", Syntax::None (), Syntax::Const,
	       "New development stage after harvest.");
  alist.add ("DSnew", 0.20);
  syntax.add ("last_cut", Syntax::Date, Syntax::OptionalState,
	      "Date of last cut.  Used for calculating cut delay.");
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
}

Harvesting::Harvesting (const AttributeList& al)
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
    DSnew (al.number ("DSnew")),
    last_cut (al.check ("last_cut") ? new Time (al.time ("last_cut")) : NULL),
    production_delay (al.number ("production_delay")),
    cut_delay (al.plf ("cut_delay")),
    cut_stress (0.0)
{ }

Harvesting::~Harvesting ()
{ 
  if (last_cut)
    delete last_cut;
}

static Submodel::Register 
soil_submodel ("Harvesting", Harvesting::load_syntax);
