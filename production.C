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

#define BUILD_DLL

#include "production.h"
#include "crpn.h"
#include "partition.h"
#include "organic_matter.h"
#include "geometry.h"
#include "am.h"
#include "log.h"
#include "time.h"
#include "plf.h"
#include "submodel.h"
#include "treelog.h"
#include "mathlib.h"
#include "frame_submodel.h"
#include <sstream>

// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]
const double molWeightC = 12.0; // [gC/mol]

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;
// Based on Penning de Vries et al. 1989, page 63
// E is the assimilate conversion effiency
static double DM_to_C_factor (double E)
{
   return molWeightC/molWeightCH2O * (1.0 - (0.5673 - 0.5327 * E)) / E;
   // return 12.0/30.0 * (1.0 - (0.5673 - 0.5327 * E)) / E;
}

double
Production::remobilization (const double DS, const double dt)
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
      StemRes -= ReMobilization * dt;
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
Production::total_C () const
{
  // kg/ha -> g/m^2
  const double conv = 1000.0 / (100.0 * 100.0);
  return CCrop / conv;
}

bool 
Production::root_growth () const
{ return DailyNetRoot > 0.0; }

bool
Production::leaf_growth () const
{ return DailyNetLeaf > 0.0; }

double
Production::maintenance_respiration (double r, double w, double T)
{
  if (w <= 0.0)
    return 0.0;

  return (molWeightCH2O / molWeightCO2)
    * r / 24. 
    * std::max (0.0, 
                0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
                          - exp (0.57 - 0.042 * T - 0.0051 * T * T))) * w;
}

// Based on Penning de Vries et al. 1989, page 63
// Simple biochemical analysis
double
Production::GrowthRespCoef (double E)
{ return  0.5673 - 0.5327 * E; }

void
Production::tick (const double AirT, const double SoilT,
		  const std::vector<double>& Density,
		  const Geometry& geo,
		  const double DS, const double CAImRat,
		  const CrpN& nitrogen,
                  const double nitrogen_stress,
                  const double seed_C,
		  const Partition& partition,
		  double& residuals_DM,
		  double& residuals_N_top, double& residuals_C_top,
		  std::vector<double>& residuals_N_soil,
		  std::vector<double>& residuals_C_soil,
                  const double dt,
		  Treelog& msg)
{
  daisy_assert (DS >= 0.0);
  const double LeafGrowthRespCoef = GrowthRespCoef (E_Leaf);
  const double StemGrowthRespCoef = GrowthRespCoef (E_Stem);
  const double SOrgGrowthRespCoef = GrowthRespCoef (E_SOrg);
  const double RootGrowthRespCoef = GrowthRespCoef (E_Root);
  const double DS1 = fmod (DS, 2.0);

  // Seed.
  const double seed_CH2O = seed_C * molWeightCH2O / molWeightC;
  CH2OPool += seed_CH2O;

  // Remobilization
  const double ReMobil = remobilization (DS, dt);
  const double CH2OReMobil = ReMobil * DM_to_C_factor (E_Stem) * 30.0/12.0;
  const double ReMobilResp = CH2OReMobil - ReMobil;
  // Note:  We used to have "CH2OPool += Remobil", but that does not
  // give C balance.  ReMobilResp was invented, and gets it's strange
  // value, to get the same result but with a mass balance.  
  CH2OPool += CH2OReMobil * dt;

  // Release of root reserves
  bool ReleaseOfRootReserves = false;
  if (DS1 > IntDSRelRtRes && DS1 < EndDSRelRtRes)
    {
      if (WLeaf < LfRtRelRtRes * WRoot)
	{
	  const double RootRelease = RelRateRtRes * WRoot * dt / 24.;
	  CH2OPool += RootRelease * DM_to_C_factor (E_Root) * (30./12.);
	  WRoot -= RootRelease;
	  ReleaseOfRootReserves = true;
	}
    }

  // Photosyntheses.
  daisy_assert (std::isfinite (CanopyAss));
  CH2OPool += CanopyAss * dt;
  double NetAss = CanopyAss;    // [g CH2O/m^2/h]

  // Mantenance respiration.
  double RMLeaf                 // [g CH2O/m^2/h]
    = maintenance_respiration (r_Leaf, WLeaf, AirT);
  daisy_assert (std::isfinite (RMLeaf));
  const double RMStem           // [g CH2O/m^2/h]
    = maintenance_respiration (r_Stem, WStem, AirT);
  daisy_assert (std::isfinite (RMStem));
  const double RMSOrg           // [g CH2O/m^2/h]
    = maintenance_respiration (r_SOrg, WSOrg, AirT);
  daisy_assert (std::isfinite (RMSOrg));
  const double RMRoot           // [g CH2O/m^2/h]
    = maintenance_respiration (r_Root, WRoot, SoilT);
  daisy_assert (std::isfinite (RMRoot));

  // Water stress stops leaf respiration.
  daisy_assert (std::isfinite (PotCanopyAss));
  daisy_assert (std::isfinite (CanopyAss));
  RMLeaf = std::max (0.0, RMLeaf - PotCanopyAss + CanopyAss);
  daisy_assert (std::isfinite (RMLeaf));
  const double RM =             // [g CH2O/m^2/h]
    RMLeaf + RMStem + RMSOrg + RMRoot + ReMobilResp;
  Respiration += RM;
  MaintRespiration = RM;
  daisy_assert (std::isfinite (RM));
  NetAss -= RM;

  double LeafMResp = RMLeaf;     // [g CH2O/m^2/h]
  double StemMResp = RMStem;     // [g CH2O/m^2/h]
  double SOrgMResp = RMSOrg;     // [g CH2O/m^2/h]
  double RootMResp = RMRoot;     // [g CH2O/m^2/h]
  
  double LeafGResp = 0.0;
  double StemGResp = 0.0;
  double SOrgGResp = 0.0;
  double RootGResp = 0.0;
 
  daisy_assert (CH2OPool >= 0.0);
  if (CH2OPool >= RM * dt)
    {
      // We have enough assimilate to cover respiration.
      CH2OPool -= RM * dt;
      const double AssG         // [g CH2O/m^2/h]
        = std::min (1.0, dt * CH2OReleaseRate) * CH2OPool;
      CH2OPool -= AssG * dt;

      // Partition growth.
      double f_Leaf, f_Stem, f_SOrg, f_Root;

      partition (DS, RSR (), nitrogen_stress, 
                 f_Leaf, f_Stem, f_Root, f_SOrg);
      if (ReleaseOfRootReserves)
        {
          f_Leaf += f_Root;
          f_Root = 0.0;
        }

      IncWLeaf = E_Leaf * f_Leaf * AssG;
      IncWStem = E_Stem * f_Stem * AssG - ReMobil;
      IncWSOrg = E_SOrg * f_SOrg * AssG;
      IncWRoot = E_Root * f_Root * AssG;

      GrowthRespiration         // [g CH2O/m^2/h]
        = LeafGrowthRespCoef * f_Leaf * AssG
        + StemGrowthRespCoef * f_Stem * AssG
        + SOrgGrowthRespCoef * f_SOrg * AssG
        + RootGrowthRespCoef * f_Root * AssG;
      const double IncC         // // [g C/m^2/h]
        = IncWLeaf * DM_to_C_factor (E_Leaf)
        + IncWStem * DM_to_C_factor (E_Stem)
        + IncWSOrg * DM_to_C_factor (E_SOrg)
        + IncWRoot * DM_to_C_factor (E_Root);
      if (!approximate (AssG - CH2OReMobil,
                        IncC * 30.0/12.0 + GrowthRespiration))
        {
          std::ostringstream tmp;
          tmp << "C inblance in growth\n"
              << "Assimilate = " <<  AssG * 10 * 12./30. << " kg C/ha/h\n"
              << "Remobil = " << CH2OReMobil * 10 * 12./30. << " kg C/ha/h\n"
              << "Growth = " <<  IncC * 10 << " kg C/ha/h\n"
              << "Resp = " <<  GrowthRespiration * 10 * 12./30.
              << " kg C/ha/h\nError " 
              << ((AssG - CH2OReMobil) * 12./30
                  - IncC - GrowthRespiration * 12./30) * 10 
              <<  " kg C/ha/h";
          msg.error (tmp.str ());
        }
      daisy_assert (std::isfinite (GrowthRespiration));
      NetAss -= GrowthRespiration;
      Respiration += GrowthRespiration;
      LeafGResp = LeafGrowthRespCoef * f_Leaf * AssG;
      StemGResp = StemGrowthRespCoef * f_Stem * AssG;
      SOrgGResp = SOrgGrowthRespCoef * f_SOrg * AssG;
      RootGResp = RootGrowthRespCoef * f_Root * AssG;
    }
  else
    {
      // Too little assimilate to cover respiration.  
      // Give assimilate to most important parts first.

      // Remobilization should generate more assimilate than it
      // consumes.  We satisfy that process first. 
      daisy_assert (ReMobilResp * dt <= CH2OPool);
      CH2OPool -= ReMobilResp * dt;

      // We need leafs for photosyntheses, to generate more assimilate.
      if (RMLeaf * dt <= CH2OPool)
	{
	  IncWLeaf = 0.0;
	  CH2OPool -= RMLeaf * dt;
	}
      else
	{
	  IncWLeaf = (CH2OPool / dt - RMLeaf) * 12.0/30.0
            / DM_to_C_factor (E_Leaf);
	  CH2OPool = 0.0;
	}

      // Will somebody please think of the children!
      if (RMSOrg * dt <= CH2OPool)
	{
	  IncWSOrg = 0.0;
	  CH2OPool -= RMSOrg * dt;
	}
      else
	{
	  IncWSOrg = (CH2OPool - RMSOrg * dt) * 12.0/30.0
            / DM_to_C_factor (E_SOrg);
	  CH2OPool = 0.0;
	}

      // An upstanding crop.
      if (RMStem * dt <= CH2OPool)
	{
	  IncWStem = -ReMobil;
	  CH2OPool -= RMStem * dt;
	}
      else
	{
	  IncWStem = (CH2OPool / dt - RMStem) * 12.0/30.0
             / DM_to_C_factor (E_Stem) - ReMobil;
          // If we have more stem than sorg, and enough stem to cover
          // the loss of sorg, we do so.
          if (WStem + IncWStem * dt + IncWSOrg * dt >= 0.0
	      && WStem > WSOrg)
            {
              daisy_assert (IncWSOrg <= 0.0);
	      IncWStem += IncWSOrg * DM_to_C_factor (E_SOrg) 
                / DM_to_C_factor (E_Stem);
	      IncWSOrg  = 0.0;
            }
	  CH2OPool = 0.0;
	}

      // Roots are furthest away from the CH2O source, gets last.
      if (RMRoot * dt < CH2OPool)
	{
	  IncWRoot = 0.0;
	  CH2OPool -= RMRoot * dt;
	  std::ostringstream tmp;
	  tmp << "BUG: Extra CH2O: " << CH2OPool;
	  msg.error (tmp.str ());
	}
      else
	{
	  IncWRoot = (CH2OPool / dt - RMRoot) * 12.0/30.0
            / DM_to_C_factor (E_Root) ;
	  CH2OPool = 0.0;
	}
    }
  daisy_assert (std::isfinite (NetAss));
  NetPhotosynthesis = molWeightCO2 / molWeightCH2O * NetAss;
  AccNetPhotosynthesis += NetPhotosynthesis * dt;

  LeafGrowthRespiration = molWeightCO2 / molWeightCH2O * LeafGResp;  
  StemGrowthRespiration = molWeightCO2 / molWeightCH2O * StemGResp;  
  SOrgGrowthRespiration = molWeightCO2 / molWeightCH2O * SOrgGResp;
  RootGrowthRespiration = molWeightCO2 / molWeightCH2O * RootGResp;

  LeafMaintRespiration = molWeightCO2 / molWeightCH2O * LeafMResp;  
  StemMaintRespiration = molWeightCO2 / molWeightCH2O * StemMResp;  
  SOrgMaintRespiration = molWeightCO2 / molWeightCH2O * SOrgMResp;
  RootMaintRespiration = molWeightCO2 / molWeightCH2O * RootMResp;

  LeafRespiration = LeafGrowthRespiration + LeafMaintRespiration;
  StemRespiration = StemGrowthRespiration + StemMaintRespiration;
  SOrgRespiration = SOrgGrowthRespiration + SOrgMaintRespiration;
  RootRespiration = RootGrowthRespiration + RootMaintRespiration;

  // Update dead leafs
  DeadWLeaf = LfDR (DS) / 24.0 * WLeaf;
  DeadWLeaf += WLeaf * 0.333 * CAImRat / 24.0;
  daisy_assert (DeadWLeaf >= 0.0);
  double DdLeafCnc;
  daisy_assert (WLeaf >= 0.0);
  if (NCrop > 1.05 * nitrogen.PtNCnt)
    {
      if (WLeaf > 0.0)
        DdLeafCnc = NLeaf/WLeaf;
      else if (WStem > 0.0)
        DdLeafCnc = NStem/WStem;
      else 
	{
	  daisy_assert (DeadWLeaf < 1e-10);
	  DdLeafCnc = 1.0;
	}
    }
  else
    {
      if (WLeaf > 0.0)
        DdLeafCnc = (NLeaf / WLeaf - nitrogen.NfLeafCnc (DS))
          * ( 1.0 - nitrogen.TLLeafEff (DS)) +  nitrogen.NfLeafCnc (DS);
      else if (WStem > 0.0)
        DdLeafCnc = NStem/WStem;
      else
	{
	  daisy_assert (DeadWLeaf < 1e-10);
	  DdLeafCnc = 1.0;
	}
    }

  daisy_assert (DdLeafCnc >= 0.0);
  daisy_assert (DeadWLeaf >= 0.0);
  DeadNLeaf = DdLeafCnc * DeadWLeaf;
  daisy_assert (DeadNLeaf >= 0.0);
  IncWLeaf -= DeadWLeaf;
  daisy_assert (DeadWLeaf >= 0.0);
  WDead += (1.0 - ExfoliationFac) * DeadWLeaf * dt;
  NDead += (1.0 - ExfoliationFac) * DeadNLeaf * dt;
  daisy_assert (NDead >= 0.0);

  const double W_foli = ExfoliationFac * DeadWLeaf;
  const double C_foli = DM_to_C_factor (E_Leaf) * W_foli;
  C_Loss = C_foli;
  const double N_foli = ExfoliationFac * DeadNLeaf;
  daisy_assert (N_foli >= 0.0);
  if (C_foli < 1e-50)
    daisy_assert (N_foli < 1e-40);
  else
    {
      daisy_assert (N_foli > 0.0);
      AM_leaf->add (dt * C_foli * m2_per_cm2, dt * N_foli * m2_per_cm2);
      residuals_DM += W_foli;
      residuals_N_top += N_foli;
      residuals_C_top += C_foli;
      C_AM += C_foli * dt;
      N_AM += N_foli * dt;
    }

  // Update dead roots.
  double root_death_rate = RtDR (DS) * RtDR_T_factor (SoilT);
  if (RSR () > 1.1 * partition.RSR (DS))
    root_death_rate += Large_RtDR;

  if (root_death_rate > 0.0)
    {
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
      daisy_assert (AM_root);
      AM_root->add_surface (geo, dt * C_Root * m2_per_cm2,
                            dt * DeadNRoot * m2_per_cm2,
                            Density);
      daisy_assert (iszero (C_Root) || DeadNRoot > 0.0);
      residuals_DM += DeadWRoot;
      geo.add_surface (residuals_C_soil, Density, C_Root * m2_per_cm2);
      geo.add_surface (residuals_N_soil, Density, DeadNRoot * m2_per_cm2);
      C_AM += C_Root * dt;
      N_AM += DeadNRoot * dt;
    }
  else
    DeadWRoot = DeadNRoot = 0.0;

  // Update production.
  NCrop -= (DeadNLeaf + DeadNRoot) * dt;
  daisy_assert (NCrop > 0.0);
  WLeaf += IncWLeaf * dt;
  if (WLeaf < 0.0)
    WLeaf = 0.0;
  WStem += IncWStem * dt;
  if (WStem < 0.0)
    WStem = 0.0;
  WSOrg += IncWSOrg * dt;
  if (WSOrg < 0.0)
    WSOrg = 0.0;
  WRoot += IncWRoot * dt;
  if (WRoot < 0.0)
    WRoot = 0.0;
  DailyNetRoot += (IncWRoot + DeadWRoot) * dt;
  DailyNetLeaf += (IncWRoot + DeadWRoot) * dt;

  const double old_CCrop = CCrop;
  update_carbon ();
  CCrop = CLeaf + CStem + CSOrg + CRoot + CDead + CH2OPool * 12./30.;
  const double error 
    = (old_CCrop 
       + (dt * (NetPhotosynthesis *  12./44. - C_Loss)
          + seed_C)
       - CCrop) * 10;
  static double accum = 0.0;
  accum += error;
  daisy_assert (std::isfinite (NetPhotosynthesis));
  if (!approximate (old_CCrop 
                    + dt * (NetPhotosynthesis *  12./44 - C_Loss)
                    + seed_C,
                    CCrop)
      || error > 0.00001 /* 0.001 g/ha */)
    {
      std::ostringstream tmp;
      tmp << "C balance error\n"
          << "Old C = " << old_CCrop * 10 << " kg/ha\n"
          << "NetPhotosynthesis = "
          << NetPhotosynthesis *  12./30. * 10 * dt
          << " kg/ha\n"
          << "Loss " << C_Loss * dt * 10 << " kg/ha\n"
          << "Seed " << seed_C * 10 << " kg/ha\n"
          << "New C = " << CCrop * 10 << " kg/ha\n"
          << "Error = " << error * 1000 << " g/ha\n"
          << "Accumulated = " << accum << " kg/ha"; 
      msg.error (tmp.str ());
    }
}

void
Production::tick_daily ()
{ DailyNetRoot = DailyNetLeaf = 0.0; }

void 
Production::update_carbon ()
{
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
  LeafRespiration = 0.0;
  StemRespiration = 0.0;
  SOrgRespiration = 0.0;
  RootRespiration = 0.0;
  LeafMaintRespiration = 0.0;
  StemMaintRespiration = 0.0;
  SOrgMaintRespiration = 0.0;
  RootMaintRespiration = 0.0;
  LeafGrowthRespiration = 0.0;
  StemGrowthRespiration = 0.0;
  SOrgGrowthRespiration = 0.0;
  RootGrowthRespiration = 0.0;
  DeadWLeaf = 0.0;
  DeadNLeaf = 0.0;
  DeadWRoot = 0.0;
  DeadNRoot = 0.0;
  C_Loss = 0.0;
  DailyNetRoot = 0.0;
  DailyNetLeaf = 0.0;
}

void 
Production::output (Log& log) const
{
  output_variable (StemRes, log);
  output_variable (CH2OPool, log);
  output_variable (WLeaf, log);
  output_variable (WStem, log);
  output_variable (WRoot, log);
  output_variable (WSOrg, log);
  output_variable (WDead, log);
  output_variable (CLeaf, log);
  output_variable (CStem, log);
  output_variable (CRoot, log);
  output_variable (CSOrg, log);
  output_variable (CDead, log);
  output_variable (CCrop, log);
  output_variable (NLeaf, log);
  output_variable (NStem, log);
  output_variable (NRoot, log);
  output_variable (NSOrg, log);
  output_variable (NDead, log);
  output_variable (NCrop, log);
  output_variable (C_AM, log);
  output_variable (N_AM, log);
  output_variable (PotCanopyAss, log);
  output_variable (CanopyAss, log);
  output_variable (NetPhotosynthesis, log);
  output_variable (AccNetPhotosynthesis, log);
  output_variable (Respiration, log);
  output_variable (MaintRespiration, log);
  output_variable (GrowthRespiration, log);
  output_variable (LeafRespiration, log);
  output_variable (StemRespiration, log);
  output_variable (SOrgRespiration, log);
  output_variable (RootRespiration, log);
  output_variable (LeafMaintRespiration, log);
  output_variable (StemMaintRespiration, log);
  output_variable (SOrgMaintRespiration, log);
  output_variable (RootMaintRespiration, log);
  output_variable (LeafGrowthRespiration, log);
  output_variable (StemGrowthRespiration, log);
  output_variable (SOrgGrowthRespiration, log);
  output_variable (RootGrowthRespiration, log);
  output_variable (IncWLeaf, log);
  output_variable (IncWStem, log);
  output_variable (IncWSOrg, log);
  output_variable (IncWRoot, log);
  output_variable (DeadWLeaf, log);
  output_variable (DeadNLeaf, log);
  output_variable (DeadWRoot, log);
  output_variable (DeadNRoot, log);
  output_variable (C_Loss, log);
  output_variable (DailyNetRoot, log);
  output_variable (DailyNetLeaf, log);
}

void 
Production::load_syntax (FrameSubmodel& frame)
{
  frame.alist ().add ("submodel", "Production");
  frame.alist ().add ("description", "\
Crop production in the default crop model.");

  // Remobilization.
  frame.add ("ShldResC", Value::Fraction (), Value::Const,
	      "Capacity of shielded reserves (fraction of stem DM).");
  frame.add ("ShldResC", 0.0);
  frame.add ("ReMobilDS", Value::None (), Value::Const,
	      "Remobilization, Initial DS.");
  frame.add ("ReMobilDS", 1.20);
  frame.add ("ReMobilRt", "d^-1", Value::Const,
	      "Remobilization, release rate.");
  frame.add ("ReMobilRt", 0.1);
  frame.add ("StemRes", "g DM/m^2", Value::State,
	      "Shielded reserves in stems.");
  frame.add ("StemRes", 0.0);

  // Parameters.
  frame.add ("CH2OReleaseRate", "h^-1", Value::Const,
	      "CH2O Release Rate constant.");
  frame.add ("CH2OReleaseRate", 0.04);
  frame.add ("E_Root", Value::None (), Value::Const,
	      "Conversion efficiency, root.");
  frame.add ("E_Root", 0.69);
  frame.add ("E_Leaf", Value::None (), Value::Const,
	      "Conversion efficiency, leaf.");
  frame.add ("E_Leaf", 0.68);
  frame.add ("E_Stem", Value::None (), Value::Const,
	      "Conversion efficiency, stem.");
  frame.add ("E_Stem", 0.66);
  frame.add ("E_SOrg", Value::None (), Value::Const,
	      "Conversion efficiency, storage organ.");
  frame.add ("r_Root", Value::None (), Value::Const,
	      "Maintenance respiration coefficient, root.");
  frame.add ("r_Root", 0.015);
  frame.add ("r_Leaf", "d^-1", Value::Const,
	      "Maintenance respiration coefficient, leaf.");
  frame.add ("r_Stem", "d^-1", Value::Const,
	      "Maintenance respiration coefficient, stem.");
  frame.add ("r_SOrg", "d^-1", Value::Const,
	      "Maintenance respiration coefficient, storage organ.");
  frame.add ("ExfoliationFac", Value::None (), Value::Const,
	      "Exfoliation factor, 0-1.");
  frame.add ("ExfoliationFac", 1.0);
  frame.add ("LfDR", "DS", " d^-1", Value::Const,
	      "Death rate of Leafs.");
  frame.add ("RtDR", "DS", " d^-1", Value::Const,
	      "Death rate of Roots.");
  frame.add ("Large_RtDR", "d^-1", Value::Const,
	      "Extra death rate for large root/shoot.");
  frame.add ("Large_RtDR", 0.05);
  frame.add ("RtDR_T_factor", "dg C", Value::None (), Value::Const,
	      "Temperature dependent factor for root death rate.");
  PLF none;
  none.add (  0.0, 1.0);
  none.add (100.0, 1.0);
  frame.add ("RtDR_T_factor", none);
  frame.add ("IntDSRelRtRes", Value::None (), Value::Const,
	      "Initial DS for the release of root reserves.");
  frame.add ("IntDSRelRtRes", 0.80);
  frame.add ("EndDSRelRtRes", Value::None (), Value::Const,
	      "End DS for the release of root reserves.");
  frame.add ("EndDSRelRtRes", 0.80);
  frame.add ("RelRateRtRes", "d^-1", Value::Const,
	      "Release rate of root reserves.");
  frame.add ("RelRateRtRes", 0.05);
  frame.add ("LfRtRelRtRes", Value::None (), Value::Const,
	      "Max Leaf:Root for the release of root res.");
  frame.add ("LfRtRelRtRes", 0.80);

  // Variables.
  frame.add ("CH2OPool", "g CH2O/m^2", Value::State, "CH2O Pool.");
  frame.add ("CH2OPool", 0.001);
  frame.add ("WLeaf", "g DM/m^2", Value::State, "Leaf dry matter weight.");
  frame.add ("WLeaf", 0.001);
  frame.add ("WStem", "g DM/m^2", Value::State, "Stem dry matter weight.");
  frame.add ("WStem", 0.000);
  frame.add ("WRoot", "g DM/m^2", Value::State, "Root dry matter weight.");
  frame.add ("WRoot", 0.001);
  frame.add ("WSOrg", "g DM/m^2", Value::State,
	      "Storage organ dry matter weight.");
  frame.add ("WSOrg", 0.000);
  frame.add ("WDead", "g DM/m^2", Value::State,
	      "Dead leaves dry matter weight.");
  frame.add ("WDead", 0.000);
  frame.add ("CLeaf", "g C/m^2", Value::LogOnly, "Leaf C weight.");
  frame.add ("CStem", "g C/m^2", Value::LogOnly, "Stem C weight.");
  frame.add ("CRoot", "g C/m^2", Value::LogOnly, "Root C weight.");
  frame.add ("CSOrg", "g C/m^2", Value::LogOnly, "Storage organ C weight.");
  frame.add ("CDead", "g C/m^2", Value::LogOnly, "Dead leaves C weight.");
  frame.add ("CCrop", "g C/m^2", Value::LogOnly, "Crop C weight.");
  frame.add ("NLeaf", "g N/m^2", Value::State,
	      "Nitrogen stored in the leaves.");
  frame.add ("NLeaf", 0.000);
  frame.add ("NStem", "g N/m^2", Value::State,
	      "Nitrogen stored in the stem.");
  frame.add ("NStem", 0.000);
  frame.add ("NRoot", "g N/m^2", Value::State,
	      "Nitrogen stored in the roots.");
  frame.add ("NRoot", 0.000);
  frame.add ("NSOrg", "g N/m^2", Value::State,
	      "Nitrogen stored in the storage organ.");
  frame.add ("NSOrg", 0.000);
  frame.add ("NDead", "g N/m^2", Value::State,
	      "Nitrogen stored in dead leaves.");
  frame.add ("NDead", 0.000);
  frame.add ("NCrop", "g N/m^2", Value::OptionalState,
	      "Total crop nitrogen content.\n\
By default, this will start as the amount of N in the seed.");
  frame.add ("C_AM", "g C/m^2", Value::State,
	      "Added C in plant material.");
  frame.add ("C_AM", 0.000);
  frame.add ("N_AM", "g N/m^2", Value::State,
	      "Added N in plant material.");
  frame.add ("N_AM", 0.000);
  
  // Auxiliary.
  frame.add ("PotCanopyAss", "g CH2O/m^2/h", Value::LogOnly,
	      "Potential canopy assimilation, i.e. stressfree production.");
  frame.add ("CanopyAss", "g CH2O/m^2/h", Value::LogOnly,
	      "Canopy assimilation.");
  frame.add ("NetPhotosynthesis", "g CO2/m^2/h", Value::LogOnly,
	      "Net Photosynthesis.");
  frame.add ("AccNetPhotosynthesis", "g CO2/m^2", Value::LogOnly,
	      "Accumulated Net Photosynthesis.");
  frame.add ("Respiration", "g CH2O/m^2/h", Value::LogOnly,
	      "Crop Respiration.");
  frame.add ("MaintRespiration", "g CH2O/m^2/h", Value::LogOnly,
	      "Maintenance Respiration.");
  frame.add ("GrowthRespiration", "g CH2O/m^2/h", Value::LogOnly,
	      "Growth Respiration.");
  frame.add ("LeafRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Total Leaf Respiration.");
  frame.add ("StemRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Total Stem Respiration.");
  frame.add ("SOrgRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Total SOrg Respiration.");
  frame.add ("RootRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Total Root Respiration.");
  frame.add ("LeafMaintRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Leaf Maintenance Respiration.");
  frame.add ("StemMaintRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Stem Maintenance Respiration.");
  frame.add ("SOrgMaintRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "SOrg Maintenance Respiration.");
  frame.add ("RootMaintRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Root Maintenance Respiration.");
  frame.add ("LeafGrowthRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Leaf Growth Respiration.");
  frame.add ("StemGrowthRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Stem Growth Respiration.");
  frame.add ("SOrgGrowthRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "SOrg Growth Respiration.");
  frame.add ("RootGrowthRespiration", "g CO2/m^2/h", Value::LogOnly,
	      "Root Growth Respiration.");
  frame.add ("IncWLeaf", "g DM/m^2/h", Value::LogOnly,
	      "Leaf growth.");
  frame.add ("IncWStem", "g DM/m^2/h", Value::LogOnly,
	      "Stem growth.");
  frame.add ("IncWSOrg", "g DM/m^2/h", Value::LogOnly,
	      "Storage organ growth.");
  frame.add ("IncWRoot", "g DM/m^2/h", Value::LogOnly,
	      "Root growth.");
  frame.add ("DeadWLeaf", "g DM/m^2/h", Value::LogOnly,
	      "Leaf DM removed.");
  frame.add ("DeadNLeaf", "g N/m2/h", Value::LogOnly,
	      "Leaf N removed.");
  frame.add ("DeadWRoot", "g DM/m^2/h", Value::LogOnly,
	      "Root DM removed.");
  frame.add ("DeadNRoot", "g N/m2/h", Value::LogOnly,
	      "Root N removed.");
  frame.add ("C_Loss", "g C/m^2/h", Value::LogOnly,"C lost from the crop");
  frame.add ("DailyNetRoot", "g DM/m^2", Value::State,
	      "Root growth minus root respiration so far this day.");
  frame.add ("DailyNetRoot", 0.0);
  frame.add ("DailyNetLeaf", "g DM/m^2", Value::State,
	      "Leaf growth minus leaf respiration so far this day.");
  frame.add ("DailyNetLeaf", 0.0);
}

void
Production::initialize (const double SeedN)
{
  if (NCrop <= 0.0)
    NCrop = SeedN;
}

void
Production::initialize (const symbol name, 
			const std::vector<const AttributeList*>& root,
			const std::vector<const AttributeList*>& dead,
			const Geometry& geo,
			OrganicMatter& organic_matter)
{
  // Hotstart, find pool in organic matter.
  daisy_assert (AM_root == NULL);
  static const symbol root_symbol ("root");
  AM_root = organic_matter.find_am (name, root_symbol);
  daisy_assert (AM_leaf == NULL);
  static const symbol dead_symbol ("dead");
  AM_leaf = organic_matter.find_am (name, dead_symbol);

  // If not found, we is planting emerged crops.  Create pools.
  if (!AM_root)
    {
      AM_root = &AM::create (geo.cell_size (), Time (1, 1, 1, 1), root,
			     name, root_symbol, AM::Locked);
      organic_matter.add (*AM_root);
    }
	  
  if (!AM_leaf)
    {
      AM_leaf = &AM::create (geo.cell_size (), Time (1, 1, 1, 1), dead,
			     name, dead_symbol, AM::Locked);
      organic_matter.add (*AM_leaf);
    }
  daisy_assert (AM_root);
  daisy_assert (AM_leaf);
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
    CLeaf (WLeaf * DM_to_C_factor (E_Leaf)),
    CStem (WStem * DM_to_C_factor (E_Stem)),
    CRoot (WRoot * DM_to_C_factor (E_Root)),
    CSOrg (WSOrg * DM_to_C_factor (E_SOrg)),
    CDead (WDead * DM_to_C_factor (E_Leaf)),
    CCrop (CLeaf + CStem + CSOrg + CRoot + CDead + CH2OPool * 12./30.),
    NCrop (al.number ("NCrop", -42.42e42)),
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
    PotCanopyAss (0.0),
    CanopyAss (0.0),
    NetPhotosynthesis (0.0),
    AccNetPhotosynthesis (0.0),
    Respiration (0.0),
    MaintRespiration (0.0),
    GrowthRespiration (0.0),
    LeafRespiration (0.0),
    StemRespiration (0.0),
    SOrgRespiration (0.0),
    RootRespiration (0.0),
    LeafMaintRespiration (0.0),
    StemMaintRespiration (0.0),
    SOrgMaintRespiration (0.0),
    RootMaintRespiration (0.0),
    LeafGrowthRespiration (0.0),
    StemGrowthRespiration (0.0),
    SOrgGrowthRespiration (0.0),
    RootGrowthRespiration (0.0),
    IncWLeaf (0.0),
    IncWStem (0.0),
    IncWSOrg (0.0),
    IncWRoot (0.0),
    DeadWLeaf (0.0),
    DeadNLeaf (0.0),
    DeadWRoot (0.0),
    DeadNRoot (0.0),
    C_Loss (0.0),
    DailyNetRoot  (al.number ("DailyNetRoot")),
    DailyNetLeaf  (al.number ("DailyNetLeaf"))
{ }

Production::~Production ()
{ }

static Submodel::Register 
production_submodel ("Production", Production::load_syntax);
