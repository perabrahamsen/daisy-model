// production.C -- Default crop production submodel.
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

#include "production.h"
#include "crpn.h"
#include "partition.h"
#include "organic_matter.h"
#include "am.h"
#include "log.h"
#include "time.h"
#include "plf.h"
#include "message.h"
#include "submodel.h"

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;
// Based on Penning de Vries et al. 1989, page 63
// E is the assimilate conversion effiency
static double DM_to_C_factor (double E)
{
   return 12.0/30.0 * (1.0 - (0.5673 - 0.5327 * E)) / E;
}
// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

double
Production::remobilization (const double DS)
{
  if (DS < ReMobilDS)
    {
      StemRes = 0.0;
      return 0.0;
    }
  else if (StemRes < 1.0e-9)
    {
      StemRes = ShldResC * WStem;
      return 0.0;
    }
  else
    {
      const double ReMobilization = ReMobilRt / 24. * StemRes;
      StemRes -= ReMobilization;
      return ReMobilization;
    }
}

double
Production::RSR () const
{
  const double shoot = WStem + WSOrg + WLeaf;
  const double root = WRoot;
  if (shoot < 20.0 || root < 20.0)
    return 0.33333;
  return root/shoot;
}

double
Production::DM () const
{
  return (WSOrg + WStem + WLeaf + WDead) * 10; // [g/m^2 -> kg/ha]
}

double
Production::total_N () const
{
  // kg/ha -> g/m^2
  const double conv = 1000.0 / (100.0 * 100.0);
  return NCrop / conv;
}

double
Production::maintenance_respiration (double r, double w, double T)
{
  if (w <= 0.0)
    return 0.0;

  return (molWeightCH2O / molWeightCO2)
    * r / 24. 
    * max (0.0, 
	   0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
			       - exp (0.57 - 0.042 * T - 0.0051 * T * T))) * w;
}

// Based on Penning de Vries et al. 1989, page 63
// Simple biochemical analysis
const double
Production::GrowthRespCoef (double E)
{ return  0.5673 - 0.5327 * E; }

void
Production::tick (const double AirT, const double SoilT,
		  const vector<double>& Density,
		  const Geometry& geometry,
		  const double DS, const double CAImRat,
		  const CrpN& nitrogen,
		  const Partition& partition)
{
  const double LeafGrowthRespCoef = GrowthRespCoef (E_Leaf);
  const double StemGrowthRespCoef = GrowthRespCoef (E_Stem);
  const double SOrgGrowthRespCoef = GrowthRespCoef (E_SOrg);
  const double RootGrowthRespCoef = GrowthRespCoef (E_Root);
  const double DS1 = fmod (DS, 2.0);

  // Remobilization
  const double ReMobil = remobilization (DS);
  const double CH2OReMobil = ReMobil * DM_to_C_factor (E_Stem) * 30.0/12.0;
  const double ReMobilResp = CH2OReMobil - ReMobil;
  CH2OPool += CH2OReMobil;

  // Release of root reserves
  bool ReleaseOfRootReserves = false;
  if (DS1 > IntDSRelRtRes && DS1 < EndDSRelRtRes)
    {
       if (WLeaf < LfRtRelRtRes * WRoot)
         {
            double RootRelease = RelRateRtRes * WRoot / 24.;
            CH2OPool += RootRelease;
            WRoot -= RootRelease;
            ReleaseOfRootReserves = true;
            CERR << "Extra CH2O: " << RootRelease << "\n";
         }
    }
  double NetAss = CanopyAss;

  double RMLeaf
    = maintenance_respiration (r_Leaf, WLeaf, AirT);
  const double RMStem
    = maintenance_respiration (r_Stem, WStem, AirT);
  const double RMSOrg
    = maintenance_respiration (r_SOrg, WSOrg, AirT);
  const double RMRoot
    = maintenance_respiration (r_Root, WRoot, SoilT);

  RMLeaf = max (0.0, RMLeaf - PotCanopyAss + CanopyAss);
  const double RM = RMLeaf + RMStem + RMSOrg + RMRoot + ReMobilResp;
  Respiration += RM;
  MaintRespiration = RM;
  NetAss -= RM;
  double RootResp = RMRoot;

  if (CH2OPool >= RM)
    {
      CH2OPool -= RM;
      double f_Leaf, f_Stem, f_SOrg, f_Root;
      partition (DS, RSR (), f_Leaf, f_Stem, f_Root, f_SOrg);
      if (ReleaseOfRootReserves)
        {
          f_Leaf += f_Root;
          f_Root = 0.0;
        }
      const double AssG = CH2OReleaseRate * CH2OPool;
      IncWLeaf = E_Leaf * f_Leaf * AssG;
      IncWStem = E_Stem * f_Stem * AssG - ReMobil;
      IncWSOrg = E_SOrg * f_SOrg * AssG;
      IncWRoot = E_Root * f_Root * AssG;
      CH2OPool -= AssG;
      NetAss -= LeafGrowthRespCoef * f_Leaf * AssG
        + StemGrowthRespCoef * f_Stem * AssG
        + SOrgGrowthRespCoef * f_SOrg * AssG
        + RootGrowthRespCoef * f_Root * AssG;
      RootResp += RootGrowthRespCoef * f_Root * AssG;
      GrowthRespiration = LeafGrowthRespCoef * f_Leaf * AssG
        + StemGrowthRespCoef * f_Stem * AssG
        + SOrgGrowthRespCoef * f_SOrg * AssG
        + RootGrowthRespCoef * f_Root * AssG;
      Respiration += GrowthRespiration;
    }
  else
    {
      if (RMLeaf <= CH2OPool)
	{
	  IncWLeaf = 0.0;
	  CH2OPool -= RMLeaf;
	}
      else
	{
	  IncWLeaf = CH2OPool - RMLeaf;
	  CH2OPool = 0.0;
	}
      if (RMSOrg <= CH2OPool)
	{
	  IncWSOrg = 0.0;
	  CH2OPool -= RMSOrg;
	}
      else
	{
	  IncWSOrg = (CH2OPool - RMSOrg) * 12.0/30.0
             / DM_to_C_factor (E_SOrg) - ReMobil;
	  CH2OPool = 0.0;
	}
      if (RMStem <= CH2OPool)
	{
	  IncWStem = -ReMobil;
	  CH2OPool -= RMStem;
	}
      else
	{
	  IncWStem = (CH2OPool - RMStem) * 12.0/30.0
             / DM_to_C_factor (E_Stem) - ReMobil;
          if (WStem + IncWStem + IncWSOrg >= 0.0
	      && WStem > WSOrg)
            {
	      IncWStem += IncWSOrg;
	      IncWSOrg  = 0.0;
            }
	  CH2OPool = 0.0;
	}
      if (RMRoot <= CH2OPool)
	{
	  IncWRoot = 0.0;
	  CH2OPool -= RMRoot;
	}
      else
	{
	  IncWRoot = CH2OPool - RMRoot;
	  CH2OPool = 0.0;
	}
      if (ReMobilResp <= CH2OPool)
	{
	  CH2OPool -= ReMobilResp;
	}
      else
	{
	  CH2OPool = 0.0;
	}
      if (CH2OPool > 0.0)
	CERR << "BUG: Extra CH2O: " << CH2OPool << "\n";
    }
  NetPhotosynthesis = molWeightCO2 / molWeightCH2O * NetAss;
  AccNetPhotosynthesis += NetPhotosynthesis;

  RootRespiration = molWeightCO2 / molWeightCH2O * RootResp;

  // Update dead leafs
  DeadWLeaf = LfDR (DS) / 24.0 * WLeaf;
  DeadWLeaf += WLeaf * 0.333 * CAImRat / 24.0;
  assert (DeadWLeaf >= 0.0);
  double DdLeafCnc;
  assert (WLeaf >= 0.0);
  if (NCrop > 1.05 * nitrogen.PtNCnt)
    {
      if (WLeaf > 0.0)
        DdLeafCnc = NLeaf/WLeaf;
      else
        DdLeafCnc = NStem/WStem;
    }
  else
    {
      if (WLeaf > 0.0)
        DdLeafCnc = (NLeaf / WLeaf 
		     - nitrogen.NfLeafCnc (DS))
         * ( 1.0 - nitrogen.TLLeafEff (DS)) +  nitrogen.NfLeafCnc (DS);
      else
        DdLeafCnc = NStem/WStem;
    }

  assert (DdLeafCnc >= 0.0);
  assert (DeadWLeaf >= 0.0);
  DeadNLeaf = DdLeafCnc * DeadWLeaf;
  assert (DeadNLeaf >= 0.0);
  IncWLeaf -= DeadWLeaf;
  assert (DeadWLeaf >= 0.0);
  WDead += (1.0 - ExfoliationFac) * DeadWLeaf;
  NDead += (1.0 - ExfoliationFac) * DeadNLeaf;
  assert (NDead >= 0.0);

  const double C_foli = DM_to_C_factor (E_Leaf) *
                        ExfoliationFac * DeadWLeaf;
  C_Loss = C_foli;
  const double N_foli = ExfoliationFac * DeadNLeaf;
  assert (N_foli >= 0.0);
  if (C_foli < 1e-50)
    assert (N_foli < 1e-40);
  else
    {
      assert (N_foli > 0.0);
      AM_leaf->add ( C_foli * m2_per_cm2, N_foli * m2_per_cm2);
      C_AM += C_foli;
      N_AM += N_foli;
    }

  // Update dead roots.
  double root_death_rate = RtDR (DS) * RtDR_T_factor (SoilT);
  if (RSR () > 1.1 * partition.RSR (DS))
    root_death_rate += Large_RtDR;

  DeadWRoot = root_death_rate / 24.0 * WRoot;
  double DdRootCnc;
  if (NCrop > 1.05 * nitrogen.PtNCnt)
    DdRootCnc = NRoot/WRoot;
  else
    DdRootCnc = (NRoot/WRoot - nitrogen.NfRootCnc (DS))
      * ( 1.0 - nitrogen.TLRootEff (DS)) +  nitrogen.NfRootCnc (DS);
  DeadNRoot = DdRootCnc * DeadWRoot;
  IncWRoot -= DeadWRoot;
  const double C_Root = DM_to_C_factor (E_Root) * DeadWRoot;
  C_Loss += C_Root;
  AM_root->add (geometry, C_Root * m2_per_cm2,
		DeadNRoot * m2_per_cm2,
		Density);
  assert (C_Root == 0.0 || DeadNRoot > 0.0);
  C_AM += C_Root;
  N_AM += DeadNRoot;

  // Update production.
  NCrop -= (DeadNLeaf + DeadNRoot);
  assert (NCrop > 0.0);
  WLeaf += IncWLeaf;
  WStem += IncWStem;
  WSOrg += IncWSOrg;
  WRoot += IncWRoot;
  CLeaf = WLeaf * DM_to_C_factor (E_Leaf);
  CStem = WStem * DM_to_C_factor (E_Stem);
  CSOrg = WSOrg * DM_to_C_factor (E_SOrg);
  CRoot = WRoot * DM_to_C_factor (E_Root);
  CDead = WDead * DM_to_C_factor (E_Leaf);
  CCrop = CLeaf + CStem + CSOrg + CRoot + CDead + CH2OPool * 12./30.;
}

void
Production::none ()
{
  IncWLeaf = 0.0;
  IncWStem = 0.0;
  IncWSOrg = 0.0;
  IncWRoot = 0.0;
  NetPhotosynthesis = 0.0;
  AccNetPhotosynthesis = 0.0;
  Respiration = 0.0;
  MaintRespiration = 0.0;
  GrowthRespiration = 0.0;
  RootRespiration = 0.0;
  DeadWLeaf = 0.0;
  DeadNLeaf = 0.0;
  DeadWRoot = 0.0;
  DeadNRoot = 0.0;
  C_Loss = 0.0;
}

void 
Production::output (Log& log) const
{
  log.output ("StemRes", StemRes);
  log.output ("CH2OPool", CH2OPool);
  log.output ("WLeaf", WLeaf);
  log.output ("WStem", WStem);
  log.output ("WRoot", WRoot);
  log.output ("WSOrg", WSOrg);
  log.output ("WDead", WDead);
  log.output ("CCrop", CCrop);
  log.output ("CLeaf", CLeaf);
  log.output ("CStem", CStem);
  log.output ("CRoot", CRoot);
  log.output ("CSOrg", CSOrg);
  log.output ("CDead", CDead);
  log.output ("NLeaf", NLeaf);
  log.output ("NStem", NStem);
  log.output ("NRoot", NRoot);
  log.output ("NSOrg", NSOrg);
  log.output ("NDead", NDead);
  log.output ("NCrop", NCrop);
  log.output ("C_AM", C_AM);
  log.output ("N_AM", N_AM);
  log.output ("PotCanopyAss", PotCanopyAss);
  log.output ("CanopyAss", CanopyAss);
  log.output ("NetPhotosynthesis", NetPhotosynthesis);
  log.output ("AccNetPhotosynthesis", AccNetPhotosynthesis);
  log.output ("Respiration", Respiration);
  log.output ("MaintRespiration", MaintRespiration);
  log.output ("GrowthRespiration", GrowthRespiration);
  log.output ("RootRespiration", RootRespiration);
  log.output ("IncWLeaf", IncWLeaf);
  log.output ("IncWStem", IncWStem);
  log.output ("IncWSOrg", IncWSOrg);
  log.output ("IncWRoot", IncWRoot);
  log.output ("DeadWLeaf", DeadWLeaf);
  log.output ("DeadNLeaf", DeadNLeaf);
  log.output ("DeadWRoot", DeadWRoot);
  log.output ("DeadNRoot", DeadNRoot);
  log.output ("C_Loss", C_Loss);
}

void 
Production::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Production");
  alist.add ("description", "\
Crop production in the default crop model.");

  // Remobilization.
  syntax.add ("ShldResC", Syntax::None (), Syntax::Const,
	      "Capacity of shielded reserves (fraction of stem DM).");
  alist.add ("ShldResC", 0.0);
  syntax.add ("ReMobilDS", Syntax::None (), Syntax::Const,
	      "Remobilization, Initial DS.");
  alist.add ("ReMobilDS", 1.20);
  syntax.add ("ReMobilRt", "d^-1", Syntax::Const,
	      "Remobilization, release rate.");
  alist.add ("ReMobilRt", 0.1);
  syntax.add ("StemRes", "g DM/m^2", Syntax::State,
	      "Shielded reserves in stems.");
  alist.add ("StemRes", 0.0);

  // Parameters.
  syntax.add ("CH2OReleaseRate", Syntax::None (), Syntax::Const,
	      "CH2O Release Rate constant.");
  alist.add ("CH2OReleaseRate", 0.04);
  syntax.add ("E_Root", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, root.");
  alist.add ("E_Root", 0.69);
  syntax.add ("E_Leaf", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, leaf.");
  alist.add ("E_Leaf", 0.68);
  syntax.add ("E_Stem", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, stem.");
  alist.add ("E_Stem", 0.66);
  syntax.add ("E_SOrg", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, storage organ.");
  syntax.add ("r_Root", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, root.");
  alist.add ("r_Root", 0.015);
  syntax.add ("r_Leaf", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, leaf.");
  syntax.add ("r_Stem", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, stem.");
  syntax.add ("r_SOrg", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, storage organ.");
  syntax.add ("ExfoliationFac", Syntax::None (), Syntax::Const,
	      "Exfoliation factor, 0-1.");
  alist.add ("ExfoliationFac", 1.0);
  syntax.add ("GrowthRateRedFac", Syntax::None (), Syntax::Const,
	      "Growth rate reduction factor, 0-1.");
  alist.add ("GrowthRateRedFac", 0.0);
  syntax.add ("LfDR", "DS", " d^-1", Syntax::Const,
	      "Death rate of Leafs.");
  syntax.add ("RtDR", "DS", " d^-1", Syntax::Const,
	      "Death rate of Roots.");
  syntax.add ("Large_RtDR", "d^-1", Syntax::Const,
	      "Extra death rate for large root/shoot.");
  alist.add ("Large_RtDR", 0.05);
  syntax.add ("RtDR_T_factor", "dg C", Syntax::None (), Syntax::Const,
	      "Temperature dependent factor for root death rate.");
  PLF none;
  none.add (  0.0, 1.0);
  none.add (100.0, 1.0);
  alist.add ("RtDR_T_factor", none);
  syntax.add ("IntDSRelRtRes", Syntax::None (), Syntax::Const,
	      "Initial DS for the release of root reserves.");
  alist.add ("IntDSRelRtRes", 0.80);
  syntax.add ("EndDSRelRtRes", Syntax::None (), Syntax::Const,
	      "End DS for the release of root reserves.");
  alist.add ("EndDSRelRtRes", 0.80);
  syntax.add ("RelRateRtRes", "d^-1", Syntax::Const,
	      "Release rate of root reserves.");
  alist.add ("RelRateRtRes", 0.05);
  syntax.add ("LfRtRelRtRes", Syntax::None (), Syntax::Const,
	      "Max Leaf:Root for the release of root res.");
  alist.add ("LfRtRelRtRes", 0.80);

  // Variables.
  syntax.add ("CH2OPool", "g DM/m^2", Syntax::State, "CH2O Pool.");
  alist.add ("CH2OPool", 0.001);
  syntax.add ("WLeaf", "g DM/m^2", Syntax::State, "Leaf dry matter weight.");
  alist.add ("WLeaf", 0.001);
  syntax.add ("WStem", "g DM/m^2", Syntax::State, "Stem dry matter weight.");
  alist.add ("WStem", 0.000);
  syntax.add ("WRoot", "g DM/m^2", Syntax::State, "Root dry matter weight.");
  alist.add ("WRoot", 0.001);
  syntax.add ("WSOrg", "g DM/m^2", Syntax::State,
	      "Storage organ dry matter weight.");
  alist.add ("WSOrg", 0.000);
  syntax.add ("WDead", "g DM/m^2", Syntax::State,
	      "Dead leaves dry matter weight.");
  alist.add ("WDead", 0.000);
  syntax.add ("CCrop", "g C/m^2", Syntax::LogOnly, "Crop C weight.");
  syntax.add ("CLeaf", "g C/m^2", Syntax::LogOnly, "Leaf C weight.");
  syntax.add ("CStem", "g C/m^2", Syntax::LogOnly, "Stem C weight.");
  syntax.add ("CRoot", "g C/m^2", Syntax::LogOnly, "Root C weight.");
  syntax.add ("CSOrg", "g C/m^2", Syntax::LogOnly, "Storage organ C weight.");
  syntax.add ("CDead", "g C/m^2", Syntax::LogOnly, "Dead leaves C weight.");
  syntax.add ("NLeaf", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the leaves.");
  alist.add ("NLeaf", 0.000);
  syntax.add ("NStem", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the stem.");
  alist.add ("NStem", 0.000);
  syntax.add ("NRoot", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the roots.");
  alist.add ("NRoot", 0.000);
  syntax.add ("NSOrg", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the storage organ.");
  alist.add ("NSOrg", 0.000);
  syntax.add ("NDead", "g N/m^2", Syntax::State,
	      "Nitrogen stored in dead leaves.");
  alist.add ("NDead", 0.000);
  syntax.add ("NCrop", "g N/m^2", Syntax::OptionalState,
	      "Total crop nitrogen content.\n\
For initialization, set this to the amount of N in the seed.");
  syntax.add ("C_AM", "g C/m^2", Syntax::State,
	      "Added C in plant material.");
  alist.add ("C_AM", 0.000);
  syntax.add ("N_AM", "g N/m^2", Syntax::State,
	      "Added N in plant material.");
  alist.add ("N_AM", 0.000);
  
  // Auxiliary.
  syntax.add ("PotCanopyAss", "g CH2O/m^2", Syntax::State,
	      "Potential canopy assimilation this day until now.");
  alist.add ("PotCanopyAss", 0.0);
  syntax.add ("CanopyAss", "g CH2O/m^2", Syntax::State,
	      "Canopy assimilation this day until now.");
  alist.add ("CanopyAss", 0.0);
  syntax.add ("NetPhotosynthesis", "g CO2/m^2/h", Syntax::LogOnly,
	      "Net Photosynthesis.");
  syntax.add ("AccNetPhotosynthesis", "g CO2/m^2", Syntax::LogOnly,
	      "Accumulated Net Photosynthesis.");
  syntax.add ("Respiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Crop Respiration.");
  syntax.add ("MaintRespiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Maintenance Respiration.");
  syntax.add ("GrowthRespiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Growth Respiration.");
  syntax.add ("RootRespiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Root Respiration.");
  syntax.add ("IncWLeaf", "g DM/m^2/d", Syntax::LogOnly,
	      "Leaf growth.");
  syntax.add ("IncWStem", "g DM/m^2/d", Syntax::LogOnly,
	      "Stem growth.");
  syntax.add ("IncWSOrg", "g DM/m^2/d", Syntax::LogOnly,
	      "Storage organ growth.");
  syntax.add ("IncWRoot", "g DM/m^2/d", Syntax::LogOnly,
	      "Root growth.");
  syntax.add ("DeadWLeaf", "g DM/m^2/d", Syntax::LogOnly,
	      "Leaf DM removed.");
  syntax.add ("DeadNLeaf", "g N/m2/d", Syntax::LogOnly,
	      "Leaf N removed.");
  syntax.add ("DeadWRoot", "g DM/m^2/d", Syntax::LogOnly,
	      "Root DM removed.");
  syntax.add ("DeadNRoot", "g N/m2/d", Syntax::LogOnly,
	      "Root N removed.");
  syntax.add ("C_Loss", "g C/m^2", Syntax::LogOnly,"C lost from the crop");
}

void
Production::initialize (const double SeedN)
{
  if (NCrop <= 0.0)
    NCrop = SeedN;
}

void
Production::initialize (const string& name, 
			const vector<AttributeList*>& root,
			const vector<AttributeList*>& dead,
			const Geometry& geometry,
			OrganicMatter& organic_matter)
{
  // Hotstart, find pool in organic matter.
  AM_root = organic_matter.find_am (name, "root");
  AM_leaf = organic_matter.find_am (name, "dead");

  // If not found, we is planting emerged crops.  Create pools.
  if (!AM_root)
    {
      AM_root = &AM::create (geometry, Time (1, 1, 1, 1), root,
			     name, "root", AM::Locked);
      organic_matter.add (*AM_root);
    }
	  
  if (!AM_leaf)
    {
      AM_leaf = &AM::create (geometry, Time (1, 1, 1, 1), dead,
			     name, "dead", AM::Locked);
      organic_matter.add (*AM_leaf);
    }
}

Production::Production (const AttributeList& al)
  : ShldResC (al.number ("ShldResC")),
    ReMobilDS (al.number ("ReMobilDS")),
    ReMobilRt (al.number ("ReMobilRt")),
    StemRes (al.number ("StemRes")),
    // Parameters.
    CH2OReleaseRate (al.number ("CH2OReleaseRate")),
    E_Root (al.number ("E_Root")),
    E_Leaf (al.number ("E_Leaf")),
    E_Stem (al.number ("E_Stem")),
    E_SOrg (al.number ("E_SOrg")),
    r_Root (al.number ("r_Root")),
    r_Leaf (al.number ("r_Leaf")),
    r_Stem (al.number ("r_Stem")),
    r_SOrg (al.number ("r_SOrg")),
    ExfoliationFac (al.number ("ExfoliationFac")),
    GrowthRateRedFac (al.number ("GrowthRateRedFac")),
    LfDR (al.plf ("LfDR")),
    RtDR (al.plf ("RtDR")),
    Large_RtDR (al.number ("Large_RtDR")),
    RtDR_T_factor (al.plf ("RtDR_T_factor")),
    IntDSRelRtRes (al.number ("IntDSRelRtRes")),
    EndDSRelRtRes (al.number ("EndDSRelRtRes")),
    RelRateRtRes (al.number ("RelRateRtRes")),
    LfRtRelRtRes (al.number ("LfRtRelRtRes")),
    // State.
    CH2OPool (al.number ("CH2OPool")),
    WLeaf (al.number ("WLeaf")),
    WStem (al.number ("WStem")),
    WRoot (al.number ("WRoot")),
    WSOrg (al.number ("WSOrg")),
    WDead (al.number ("WDead")),
    CCrop (0.0),
    CLeaf (0.0),
    CStem (0.0),
    CRoot (0.0),
    CSOrg (0.0),
    CDead (0.0),
    NCrop (al.check ("NCrop") ? al.number ("NCrop") : -42.42e42),
    NLeaf (al.number ("NLeaf")),
    NStem (al.number ("NStem")),
    NRoot (al.number ("NRoot")),
    NSOrg (al.number ("NSOrg")),
    NDead (al.number ("NDead")),
    C_AM  (al.number ("C_AM")),
    N_AM  (al.number ("N_AM")),
    AM_root (NULL),
    AM_leaf (NULL),
    // Auxiliary.
    PotCanopyAss (al.number ("PotCanopyAss")),
    CanopyAss (al.number ("CanopyAss")),
    NetPhotosynthesis (0.0),
    AccNetPhotosynthesis (0.0),
    Respiration (0.0),
    MaintRespiration (0.0),
    GrowthRespiration (0.0),
    RootRespiration (0.0),
    IncWLeaf (0.0),
    IncWStem (0.0),
    IncWSOrg (0.0),
    IncWRoot (0.0),
    DeadWLeaf (0.0),
    DeadNLeaf (0.0),
    DeadWRoot (0.0),
    DeadNRoot (0.0),
    C_Loss (0.0)
{ }

Production::~Production ()
{ }

static Submodel::Register 
production_submodel ("Production", Production::load_syntax);
