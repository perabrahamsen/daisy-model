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
#include "root_system.h"
#include "canopy_std.h"
#include "harvesting.h"
#include "production.h"
#include "development.h"
#include "crpaux.h"
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
#include "om.h"
#include "organic_matter.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "am.h"
#include "harvest.h"
#include "mathlib.h"
#include "message.h"

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;
// Based on Penning de Vries et al. 1989, page 63
// E is the assimilate conversion effiency
static double DM_to_C_factor (double E)
{
   return 12.0/30.0 * (1.0 - (0.5673 - 0.5327 * E)) / E;
}
// Based on Penning de Vries et al. 1989, page 63
// Simple biochemical analysis
static const double GrowthRespCoef (double E)
{
   return  0.5673 - 0.5327 * E;
}

// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

class CropStandard : public Crop
{
  // Content.
public:
  RootSystem& root_system;
  CanopyStandard& canopy;
  Harvesting& harvesting;
  Production& production;
  Development& development;
  CrpAux& auxiliary;
  Partition& partition;
  Vernalization& vernalization;
  Photosynthesis& photosynthesis;
  CrpN& nitrogen;
  const bool enable_water_stress;
  const bool enable_N_stress;

  // Communication with Bioclimate.
public:
  double water_stress () const // [0-1] (0 = full production)
  { return root_system.water_stress; }
  double nitrogen_stress () const // [0-1] (1 = no production)
  { return root_system.nitrogen_stress; }
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }
  double height () const	// Crop height [cm]
  { return canopy.Height; }
  double LAI () const
  { return canopy.CAI; }
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
			    double EvapInterception)
  { return root_system.water_uptake (Ept, soil, soil_water, EvapInterception);}
  void force_production_stress  (double pstress)
  { root_system.production_stress = pstress; }

  // Internal functions.
protected:
  // Sugar production [gCH2O/m2/h] by canopy photosynthesis.
  double CanopyPhotosynthesis (const Bioclimate&);
  double ReMobilization ();
  double MaintenanceRespiration (double r, double w, double T);
  void NetProduction (const Bioclimate&, const Geometry&, const SoilHeat&);

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Soil&,
	     OrganicMatter*,
	     const SoilHeat&,
	     const SoilWater&,
	     SoilNH4*,
	     SoilNO3*);
  const Harvest& harvest (const string& column_name,
			  const Time&, const Geometry&,
			  Bioclimate& bioclimate,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off,
			  vector<AM*>& residuals);
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
  void initialize (const Geometry& geometry, OrganicMatter&);
  void initialize (const Geometry& geometry);
  CropStandard (const AttributeList& vl);
  ~CropStandard ();
};

void
CropStandard::initialize (const Geometry& geometry,
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
		   production.WStem, development.DS);
      root_system.set_density (geometry, production.WRoot);
      nitrogen.content (development.DS, production);
    }
}

void
CropStandard::initialize (const Geometry& geometry)
{
  root_system.initialize (geometry.size ());
  production.initialize (nitrogen.SeedN);

  if (development.DS >= 0)
    nitrogen.content (development.DS, production);
}

double
CropStandard::CanopyPhotosynthesis (const Bioclimate& bioclimate)
{
  // sugar production [gCH2O/m2/h] by canopy photosynthesis.
  const PLF& LAIvsH = canopy.LAIvsH;
  const double Ta = bioclimate.daily_air_temperature ();
  const double DS = development.DS;
  // Temperature effect and development stage effect
  const double Teff = photosynthesis.TempEff (Ta) * photosynthesis.DSEff (DS);

  // One crop: assert (approximate (canopy.CAI, bioclimate.CAI ()));
  if (!approximate (LAIvsH (canopy.Height), canopy.CAI))
    {
      CERR << "Bug: CAI below top: " << LAIvsH (canopy.Height)
	   << " Total CAI: " << canopy.CAI << "\n";
      CanopyStructure ();
      CERR << "Adjusted: CAI below top: " << LAIvsH (canopy.Height)
	   << " Total CAI: " << canopy.CAI << "\n";
    }

 // CAI below the current leaf layer.
  double prevLA = LAIvsH (bioclimate.height (0));
  // Assimilate produced by canopy photosynthesis
  double Ass = 0.0;
  // Accumulated CAI, for testing purposes.
  double accCAI =0.0;
  // Number of computational intervals in the canopy.
  const int No = bioclimate.NumberOfIntervals ();
  // CAI in each interval.
  const double dCAI = bioclimate.LAI () / No;

  // True, if we haven't reached the top of the crop yet.
  bool top_crop = true;

  for (int i = 0; i < No; i++)
    {
      const double height = bioclimate.height (i+1);
      assert (height < bioclimate.height (i));

      if (top_crop && height <= canopy.Height)
	{
	  // We count day hours at the top of the crop.
	  top_crop = false;
	  if (bioclimate.PAR (i) > 0.5 * 25.0)
	    development.partial_day_length += 1.0;
	}
      // Leaf Area index for a given leaf layer
      const double LA = prevLA - LAIvsH (height);
      assert (LA >= 0.0);
      if (LA > 0)
	{
	  prevLA = LAIvsH (height);
	  accCAI += LA;

	  const double dPAR
	    = (bioclimate.PAR (i) - bioclimate.PAR (i + 1)) / dCAI;

	  // Leaf Photosynthesis [gCO2/m2/h]
	  const double F = photosynthesis.Fm *
	    (1.0 - exp (- (photosynthesis.Qeff * dPAR / photosynthesis.Fm)));

	  Ass += LA * F;
	}
    }
  assert (approximate (accCAI, canopy.CAI));

  return (molWeightCH2O / molWeightCO2) * Teff * Ass;
}

double
CropStandard::ReMobilization ()
{
  const double DS = development.DS;
  const double WStem = production.WStem;
  double& StemRes = auxiliary.StemRes;

  if (DS < production.ReMobilDS)
    {
      StemRes = 0.0;
      return 0.0;
    }
  else if (StemRes < 1.0e-9)
    {
      StemRes = production.ShldResC * WStem;
      return 0.0;
    }
  else
    {
      const double ReMobilization = production.ReMobilRt / 24. * StemRes;
      StemRes -= ReMobilization;
      return ReMobilization;
    }
}

double
CropStandard::MaintenanceRespiration (double r, double w, double T)
{
  if (w <= 0.0)
    return 0.0;

  return (molWeightCH2O / molWeightCO2)
    * r / 24. * max (0.0, 0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
			      - exp (0.57 - 0.042 * T - 0.0051 * T * T))) * w;
}

void
CropStandard::NetProduction (const Bioclimate& bioclimate,
			     const Geometry& geometry,
			     const SoilHeat& soil_heat)
{
  const double LeafGrowthRespCoef = GrowthRespCoef (production.E_Leaf);
  const double StemGrowthRespCoef = GrowthRespCoef (production.E_Stem);
  const double SOrgGrowthRespCoef = GrowthRespCoef (production.E_SOrg);
  const double RootGrowthRespCoef = GrowthRespCoef (production.E_Root);
  const double DS = development.DS;
  const double DS1 = fmod (development.DS, 2.0);
  const double Depth = root_system.Depth;
  bool ReleaseOfRootReserves = false;

  // Remobilization
  const double ReMobil = ReMobilization ();
  const double CH2OReMobil = ReMobil * DM_to_C_factor (production.E_Stem) * 30.0/12.0;
  const double ReMobilResp = CH2OReMobil - ReMobil;
  production.CH2OPool += CH2OReMobil;

  // Release of root reserves
  if (DS1 > production.IntDSRelRtRes && DS1 < production.EndDSRelRtRes)
    {
       if (production.WLeaf < production.LfRtRelRtRes * production.WRoot)
         {
            double RootRelease = production.RelRateRtRes * production.WRoot / 24.;
            production.CH2OPool += RootRelease;
            production.WRoot -= RootRelease;
            ReleaseOfRootReserves = true;
            CERR << "Extra CH2O: " << RootRelease << "\n";
         }
    }
  double NetAss = auxiliary.CanopyAss;

  const double AirT = bioclimate.daily_air_temperature ();
  const double SoilT = soil_heat.T (geometry.interval_plus (-Depth / 3));
  double RMLeaf
    = MaintenanceRespiration (production.r_Leaf, production.WLeaf, AirT);
  const double RMStem
    = MaintenanceRespiration (production.r_Stem, production.WStem, AirT);
  const double RMSOrg
    = MaintenanceRespiration (production.r_SOrg, production.WSOrg, AirT);
  const double RMRoot
    = MaintenanceRespiration (production.r_Root, production.WRoot, SoilT);

  RMLeaf = max (0.0, RMLeaf - auxiliary.PotCanopyAss + auxiliary.CanopyAss);
  const double RM = RMLeaf + RMStem + RMSOrg + RMRoot + ReMobilResp;
  auxiliary.Respiration += RM;
  auxiliary.MaintRespiration = RM;
  NetAss -= RM;
  double RootResp = RMRoot;

  if (production.CH2OPool >= RM)
    {
      production.CH2OPool -= RM;
      double f_Leaf, f_Stem, f_SOrg, f_Root;
      partition (DS, production.RSR (), f_Leaf, f_Stem, f_Root, f_SOrg);
      if (ReleaseOfRootReserves)
        {
          f_Leaf += f_Root;
          f_Root = 0.0;
        }
      const double AssG = production.CH2OReleaseRate * production.CH2OPool;
      auxiliary.IncWLeaf = production.E_Leaf * f_Leaf * AssG;
      auxiliary.IncWStem = production.E_Stem * f_Stem * AssG - ReMobil;
      auxiliary.IncWSOrg = production.E_SOrg * f_SOrg * AssG;
      auxiliary.IncWRoot = production.E_Root * f_Root * AssG;
      production.CH2OPool -= AssG;
      NetAss -= LeafGrowthRespCoef * f_Leaf * AssG
        + StemGrowthRespCoef * f_Stem * AssG
        + SOrgGrowthRespCoef * f_SOrg * AssG
        + RootGrowthRespCoef * f_Root * AssG;
      RootResp += RootGrowthRespCoef * f_Root * AssG;
      auxiliary.GrowthRespiration = LeafGrowthRespCoef * f_Leaf * AssG
        + StemGrowthRespCoef * f_Stem * AssG
        + SOrgGrowthRespCoef * f_SOrg * AssG
        + RootGrowthRespCoef * f_Root * AssG;
      auxiliary.Respiration += auxiliary.GrowthRespiration;
    }
  else
    {
      if (RMLeaf <= production.CH2OPool)
	{
	  auxiliary.IncWLeaf = 0.0;
	  production.CH2OPool -= RMLeaf;
	}
      else
	{
	  auxiliary.IncWLeaf = production.CH2OPool - RMLeaf;
	  production.CH2OPool = 0.0;
	}
      if (RMSOrg <= production.CH2OPool)
	{
	  auxiliary.IncWSOrg = 0.0;
	  production.CH2OPool -= RMSOrg;
	}
      else
	{
	  auxiliary.IncWSOrg = (production.CH2OPool - RMSOrg) * 12.0/30.0
             / DM_to_C_factor (production.E_SOrg) - ReMobil;
	  production.CH2OPool = 0.0;
	}
      if (RMStem <= production.CH2OPool)
	{
	  auxiliary.IncWStem = -ReMobil;
	  production.CH2OPool -= RMStem;
	}
      else
	{
	  auxiliary.IncWStem = (production.CH2OPool - RMStem) * 12.0/30.0
             / DM_to_C_factor (production.E_Stem) - ReMobil;
          if (production.WStem + auxiliary.IncWStem + auxiliary.IncWSOrg >= 0.0
	      && production.WStem > production.WSOrg)
            {
	      auxiliary.IncWStem += auxiliary.IncWSOrg;
	      auxiliary.IncWSOrg  = 0.0;
            }
	  production.CH2OPool = 0.0;
	}
      if (RMRoot <= production.CH2OPool)
	{
	  auxiliary.IncWRoot = 0.0;
	  production.CH2OPool -= RMRoot;
	}
      else
	{
	  auxiliary.IncWRoot = production.CH2OPool - RMRoot;
	  production.CH2OPool = 0.0;
	}
      if (production.CH2OPool > 0.0)
	CERR << "BUG: Extra CH2O: " << production.CH2OPool << "\n";
    }
  auxiliary.NetPhotosynthesis = molWeightCO2 / molWeightCH2O * NetAss;
  auxiliary.AccNetPhotosynthesis += auxiliary.NetPhotosynthesis;

  auxiliary.RootRespiration = molWeightCO2 / molWeightCH2O * RootResp;

  // Update dead leafs
  auxiliary.DeadWLeaf = production.LfDR (DS) / 24.0 * production.WLeaf;
  auxiliary.DeadWLeaf += production.WLeaf * 0.333 * canopy.CAImRat / 24.0;
  assert (auxiliary.DeadWLeaf >= 0.0);
  double DdLeafCnc;
  assert (production.WLeaf >= 0.0);
  if (production.NCrop > 1.05 * nitrogen.PtNCnt)
    {
      if (production.WLeaf > 0.0)
        DdLeafCnc = production.NLeaf/production.WLeaf;
      else
        DdLeafCnc = production.NStem/production.WStem;
    }
  else
    {
      if (production.WLeaf > 0.0)
        DdLeafCnc = (production.NLeaf / production.WLeaf 
		     - nitrogen.NfLeafCnc (DS))
         * ( 1.0 - nitrogen.TLLeafEff (DS)) +  nitrogen.NfLeafCnc (DS);
      else
        DdLeafCnc = production.NStem/production.WStem;
    }

  assert (DdLeafCnc >= 0.0);
  assert (auxiliary.DeadWLeaf >= 0.0);
  auxiliary.DeadNLeaf = DdLeafCnc * auxiliary.DeadWLeaf;
  assert (auxiliary.DeadNLeaf >= 0.0);
  auxiliary.IncWLeaf -= auxiliary.DeadWLeaf;
  assert (auxiliary.DeadWLeaf >= 0.0);
  production.WDead += (1.0 - production.ExfoliationFac) * auxiliary.DeadWLeaf;
  production.NDead += (1.0 - production.ExfoliationFac) * auxiliary.DeadNLeaf;
  assert (production.NDead >= 0.0);

  const double C_foli = DM_to_C_factor (production.E_Leaf) *
                        production.ExfoliationFac * auxiliary.DeadWLeaf;
  auxiliary.C_Loss = C_foli;
  const double N_foli = production.ExfoliationFac * auxiliary.DeadNLeaf;
  assert (N_foli >= 0.0);
  if (C_foli < 1e-50)
    assert (N_foli < 1e-40);
  else
    {
      assert (N_foli > 0.0);
      production.AM_leaf->add ( C_foli * m2_per_cm2, N_foli * m2_per_cm2);
      production.C_AM += C_foli;
      production.N_AM += N_foli;
    }

  // Update dead roots.
  double RtDR = production.RtDR (DS);
  if (production.RSR () > 1.1 * partition.RSR (DS))
    RtDR += production.Large_RtDR;

  auxiliary.DeadWRoot = RtDR / 24.0 * production.WRoot;
  double DdRootCnc;
  if (production.NCrop > 1.05 * nitrogen.PtNCnt)
    DdRootCnc = production.NRoot/production.WRoot;
  else
    DdRootCnc = (production.NRoot/production.WRoot - nitrogen.NfRootCnc (DS))
      * ( 1.0 - nitrogen.TLRootEff (DS)) +  nitrogen.NfRootCnc (DS);
  auxiliary.DeadNRoot = DdRootCnc * auxiliary.DeadWRoot;
  auxiliary.IncWRoot -= auxiliary.DeadWRoot;
  const double C_Root = DM_to_C_factor (production.E_Root) * auxiliary.DeadWRoot;
  auxiliary.C_Loss += C_Root;
  production.AM_root->add (geometry, C_Root * m2_per_cm2,
		      auxiliary.DeadNRoot * m2_per_cm2,
		      root_system.Density);
  assert (C_Root == 0.0 || auxiliary.DeadNRoot > 0.0);
  production.C_AM += C_Root;
  production.N_AM += auxiliary.DeadNRoot;

  // Update production.
  production.NCrop -= (auxiliary.DeadNLeaf + auxiliary.DeadNRoot);
  assert (production.NCrop > 0.0);
  production.WLeaf += auxiliary.IncWLeaf;
  production.WStem += auxiliary.IncWStem;
  production.WSOrg += auxiliary.IncWSOrg;
  production.WRoot += auxiliary.IncWRoot;
  production.CLeaf = production.WLeaf * DM_to_C_factor (production.E_Leaf);
  production.CStem = production.WStem * DM_to_C_factor (production.E_Stem);
  production.CSOrg = production.WSOrg * DM_to_C_factor (production.E_SOrg);
  production.CRoot = production.WRoot * DM_to_C_factor (production.E_Root);
  production.CDead = production.WDead * DM_to_C_factor (production.E_Leaf);
  production.CCrop = production.CLeaf + production.CStem + production.CSOrg +
                production.CRoot + production.CDead + production.CH2OPool * 12./30.;
}

void
CropStandard::tick (const Time& time,
		    const Bioclimate& bioclimate,
		    const Soil& soil,
		    OrganicMatter* organic_matter,
		    const SoilHeat& soil_heat,
		    const SoilWater& soil_water,
		    SoilNH4* soil_NH4,
		    SoilNO3* soil_NO3)
{
  // Update partial_soil_temperature and pressure potential.
  development.partial_soil_temperature +=
    soil_heat.T (soil.interval_plus (-root_system.DptEmr));
  development.soil_h =
    soil_water.h (soil.interval_plus (-root_system.DptEmr/2.));

  if (time.hour () == 0 && development.DS <= 0)
    {
      // Calculate average soil temperature.
      development.soil_temperature =
	development.partial_soil_temperature / 24.0;
      development.partial_soil_temperature = 0.0;

      development.emergence ();
      if (development.DS >= 0)
	{
	  COUT << " [" << name << " is emerging]\n";

	  canopy.tick (production.WLeaf, production.WSOrg,
		       production.WStem, development.DS);
	  nitrogen.content (development.DS, production);
	  root_system.tick (soil, soil_heat, production.WRoot, 0.0);

	  if (organic_matter)
	    {
	      if (!production.AM_root)
		production.AM_root
		  = &AM::create (soil, time, harvesting.Root,
				 name, "root", AM::Locked);
	      if (!production.AM_leaf)
		production.AM_leaf
		  = &AM::create (soil, time, harvesting.Dead,
				 name, "dead", AM::Locked);

	      organic_matter->add (*production.AM_root);
	      organic_matter->add (*production.AM_leaf);
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
      assert (soil_NH4);
      nitrogen.update (time.hour (), production.NCrop, development.DS,
		       enable_N_stress,
		       soil, soil_water, *soil_NH4, *soil_NO3,
		       root_system);
    }
  else
    {
      assert (!soil_NH4);
      production.NCrop = nitrogen.PtNCnt;
    }  
  const double nitrogen_stress = root_system.nitrogen_stress;
  const double water_stress = root_system.water_stress;

  if (bioclimate.PAR (bioclimate.NumberOfIntervals () - 1) > 0)
    {
      double Ass = CanopyPhotosynthesis (bioclimate);
      auxiliary.PotCanopyAss = Ass;
      if (root_system.production_stress >= 0.0)
	Ass *= (1.0 - root_system.production_stress);
      else if (enable_water_stress)
	Ass *= (1.0 - water_stress);
      if (enable_N_stress)
	Ass *= (1.0 - nitrogen_stress);
      auxiliary.CanopyAss = Ass;
      const double ProdLim = (1.0 - production.GrowthRateRedFac);
      production.CH2OPool += ProdLim * Ass;
    }
  else
      auxiliary.CanopyAss = 0.0;

  NetProduction (bioclimate, soil, soil_heat);
  nitrogen.content (development.DS, production);
  if (time.hour () != 0)
    return;

  canopy.tick (production.WLeaf, production.WSOrg,
	       production.WStem, development.DS);

  development.tick_daily (name, bioclimate.daily_air_temperature (), 
			  production.WLeaf, auxiliary, vernalization);
  root_system.tick (soil, soil_heat, production.WRoot, auxiliary.IncWRoot);
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
		       vector<AM*>& residuals)
{
  nitrogen.content (development.DS, production);
  const double DS = development.DS;
  const double DSmax = harvesting.DSmax;

  const double WStem = production.WStem;
  const double WLeaf = production.WLeaf;
  const double WSOrg = production.WSOrg;
  const double WRoot = production.WRoot;
  const double WDead = production.WDead;
  const double WCrop = WStem + WLeaf + WSOrg + WDead + WRoot;
  const double NStem = production.NStem;
  const double NLeaf = production.NLeaf;
  const double NSOrg = production.NSOrg;
  const double NRoot = production.NRoot;
  const double NDead = production.NDead;
  const double NCrop = production.NCrop;
  double Stem_Conc;
  double Leaf_Conc;
  double SOrg_Conc;
  double Dead_Conc;
  double Root_Conc;
  double Crop_Conc;
  if (WStem > 0.0) Stem_Conc = NStem / WStem; else Stem_Conc = 0.0;
  if (WLeaf > 0.0) Leaf_Conc = NLeaf / WLeaf; else Leaf_Conc = 0.0;
  if (WSOrg > 0.0) SOrg_Conc = NSOrg / WSOrg; else SOrg_Conc = 0.0;
  if (WDead > 0.0) Dead_Conc = NDead / WDead; else Dead_Conc = 0.0;
  if (WRoot > 0.0) Root_Conc = NRoot / WRoot; else Root_Conc = 0.0;
  if (WCrop > 0.0) Crop_Conc = NCrop / WCrop; else Crop_Conc = 0.0;
  const double C_C_Stem = DM_to_C_factor (production.E_Stem);
  const double C_C_Leaf = DM_to_C_factor (production.E_Leaf);
  const double C_C_Dead = C_C_Leaf;
  const double C_C_SOrg = DM_to_C_factor (production.E_SOrg);
  const double C_C_Root = DM_to_C_factor (production.E_Root);

  const vector<AttributeList*>& Stem = harvesting.Stem;
  const vector<AttributeList*>& Dead = harvesting.Dead;
  const vector<AttributeList*>& Leaf = harvesting.Leaf;
  const vector<AttributeList*>& SOrg = harvesting.SOrg;

  const vector<double>& density = root_system.Density;
  const double length = height ();
  double stem_harvest = 1.0;
  double dead_harvest = 1.0;
  double leaf_harvest = 1.0;
  double sorg_harvest = 1.0;

  Chemicals chemicals;

  // Leave stem and leaf below stub alone.

  if (stub_length < length)
    {
      stem_harvest = (1.0 - stub_length / length);

      const double total_CAI = LAI ();
      if (total_CAI > 0.0)
	{
	  const double stub_CAI = LAIvsH ()(stub_length);
	  leaf_harvest = (1.0 - stub_CAI / total_CAI);
	  bioclimate.harvest_chemicals (chemicals, total_CAI - stub_CAI);
	}
    }
  else
    {
      stem_harvest = 0.0;
      leaf_harvest = 0.0;
    }
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
  const double WEYRm = harvesting.EconomicYield_W * SOrg_W_Yield; // W is used for both
  const double NEYRm = harvesting.EconomicYield_N * SOrg_N_Yield; // DM and C.
  const double CEYRm = harvesting.EconomicYield_W * SOrg_C_Yield;

  const double Crop_N_Yield = Stem_N_Yield + Dead_N_Yield + Leaf_N_Yield + NEYRm;

  double Stem_W_Loss = (1.0 - stem_harvest_frac) * stem_harvest * WStem;
  double Dead_W_Loss = (1.0 - stem_harvest_frac) * dead_harvest * WDead;
  double Leaf_W_Loss = (1.0 - leaf_harvest_frac) * leaf_harvest * WLeaf;
  double SOrg_W_Loss = (1.0 - sorg_harvest_frac) * sorg_harvest * WSOrg +
                       (1.0 - harvesting.EconomicYield_W) * SOrg_W_Yield;
  double Stem_N_Loss = (1.0 - stem_harvest_frac) * stem_harvest * NStem;
  double Dead_N_Loss = (1.0 - stem_harvest_frac) * dead_harvest * NDead;
  double Leaf_N_Loss = (1.0 - leaf_harvest_frac) * leaf_harvest * NLeaf;
  double SOrg_N_Loss = (1.0 - sorg_harvest_frac) * sorg_harvest * NSOrg +
                       (1.0 - harvesting.EconomicYield_N) * SOrg_N_Yield;
  const double Crop_N_Loss = Stem_N_Loss + Dead_N_Loss + Leaf_N_Loss + SOrg_N_Loss;

  production.WStem -= (Stem_W_Yield + Stem_W_Loss);
  production.WDead -= (Dead_W_Yield + Dead_W_Loss);
  production.WLeaf -= (Leaf_W_Yield + Leaf_W_Loss);
  production.WSOrg -= (WEYRm + SOrg_W_Loss);
  production.NStem -= (Stem_N_Yield + Stem_N_Loss);
  production.NDead -= (Dead_N_Yield + Dead_N_Loss);
  production.NLeaf -= (Leaf_N_Yield + Leaf_N_Loss);
  production.NSOrg -= (NEYRm + SOrg_N_Loss);
  production.NCrop -= (Crop_N_Yield + Crop_N_Loss-(Dead_N_Yield + Dead_N_Loss));

  production.WStem = max(0.0, production.WStem);
  production.WDead = max(0.0, production.WDead);
  production.WLeaf = max(0.0, production.WLeaf);
  production.WSOrg = max(0.0, production.WSOrg);
  production.NStem = max(0.0, production.NStem);
  production.NDead = max(0.0, production.NDead);
  production.NLeaf = max(0.0, production.NLeaf);
  production.NSOrg = max(0.0, production.NSOrg);
  production.NCrop = max(0.0, production.NCrop);

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
 
#if 0
  const double WBal = WCrop - (production.WStem+production.WLeaf+production.WDead+production.WSOrg+production.WRoot)
                     - (Stem_W_Yield+Leaf_W_Yield+WEYRm+Dead_W_Yield)
                     - (Stem_W_Loss+Leaf_W_Loss+SOrg_W_Loss+Dead_W_Loss );
  const double NBal = NCrop - (production.NStem+production.NLeaf+production.NDead+production.NSOrg+production.NRoot)
                     - (Stem_N_Yield+Leaf_N_Yield+NEYRm+Dead_N_Yield)
                     - (Stem_N_Loss +Leaf_N_Loss +SOrg_N_Loss +Dead_N_Loss );
  double New_Crop_Conc;
#endif
  double New_Stem_Conc;
  double New_Leaf_Conc;
  double New_SOrg_Conc;
  double New_Dead_Conc;
  double New_Root_Conc;
  if (production.WStem > 0.0) New_Stem_Conc = production.NStem / production.WStem;
    else New_Stem_Conc = 0.0;
  if (production.WLeaf > 0.0) New_Leaf_Conc = production.NLeaf / production.WLeaf;
    else New_Leaf_Conc = 0.0;
  if (production.WSOrg > 0.0) New_SOrg_Conc = production.NSOrg / production.WSOrg;
    else New_SOrg_Conc = 0.0;
  if (production.WDead > 0.0) New_Dead_Conc = production.NDead / production.WDead;
    else New_Dead_Conc = 0.0;
  if (production.WRoot > 0.0) New_Root_Conc = production.NRoot / production.WRoot;
    else New_Root_Conc = 0.0;


  // Part of economic yield left in the field at harvest
  //const double WEYLf = harvesting.EconomicYield_W * (1 - sorg_harvest_frac) * WSOrg;
  // Part of non economic yield removed from the field at harvest
  //const double WRsRm = (1 - harvesting.EconomicYield_W) * sorg_harvest_frac * WSOrg;
  // Part of non economic yield left in the field at harvest
  //const double WRsLf = (1 - harvesting.EconomicYield_W) * (1 - sorg_harvest_frac) * WSOrg;
  // Part of economic yield removed at harvest
  // Part of economic yield left in the field at harvest
  //const double NEYLf = harvesting.EconomicYield_N * (1 - sorg_harvest_frac) * NSOrg;
  // Part of non economic yield removed from the field at harvest
  //const double NRsRm = (1 - harvesting.EconomicYield_N) * sorg_harvest_frac * NSOrg;
  // Part of non economic yield left in the field at harvest
  //const double NRsLf = (1 - harvesting.EconomicYield_N) * (1 - sorg_harvest_frac) * NSOrg;

  if (!kill_off && DS < DSmax && stub_length > 0.0)
    {
      // Cut back development stage and production.
      const double DSnew = harvesting.DSnew;

      if (DS > DSnew)
	development.DS = DSnew;

      // Stop fixation after cut.
      if (DS > nitrogen.DS_start_fixate)
	nitrogen.DS_start_fixate = nitrogen.DS_cut_fixate;

      if (DS > 0.0)
	{
	  // Adjust canopy for the sake of bioclimate.
	  canopy.Height = min (stub_length, canopy.Height);
	  canopy.Offset
	    = canopy.Height
	    - canopy.HvsDS (development.DS) ;
	  assert (approximate (canopy.CropHeight (production.WStem,
                               development.DS), canopy.Height));
	  canopy.CropCAI (production.WLeaf, production.WSOrg,
			  production.WStem, development.DS);
	  CanopyStructure ();

	  // Residuals left in the field
	  const double C = C_C_SOrg * SOrg_W_Loss;
	  const double N = SOrg_N_Loss;
	  AM& am = AM::create (geometry, time, SOrg, name, "sorg");
	  assert (C == 0.0 || N > 0.0);
	  am.add ( C * m2_per_cm2, N * m2_per_cm2);
	  residuals.push_back (&am);
	  production.C_AM += C;
	  production.N_AM += N;
	}
    }
  else
    {
      development.DS = DSremove;

     // Update and unlock locked AMs.
      if (!production.AM_root)
	production.AM_root = &AM::create (geometry, time, harvesting.Root,
					name, "root", AM::Unlocked);
      if (geometry.total (density) > 0.0)
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
  // Add crop remains to the soil.
  if (Stem_W_Loss > 0.0)
    {
      const double C = C_C_Stem * Stem_W_Loss;
      const double N = Stem_N_Loss;
      AM& am = AM::create (geometry, time, Stem, name, "stem");
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
           = &AM::create (geometry, time, Dead, name, "dead", AM::Unlocked);
        production.AM_leaf->add (C * m2_per_cm2, N * m2_per_cm2);
        assert (C == 0.0 || N > 0.0);
        production.C_AM += C;
        production.N_AM += N;
   }
 if (Leaf_W_Loss > 0.0)
   {
     const double C = C_C_Leaf * Leaf_W_Loss;
     const double N = Leaf_N_Loss;
     AM& am = AM::create (geometry, time, Leaf, name, "leaf");
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
     AM& am = AM::create (geometry, time, SOrg, name, "sorg");
     assert (C == 0.0 || N > 0.0);
     am.add ( C * m2_per_cm2, N * m2_per_cm2);
     residuals.push_back (&am);
     production.C_AM += C;
     production.N_AM += N;
   }
  return *new Harvest (column_name, time, name,
		       Stem_W_Yield, Stem_N_Yield, Stem_C_Yield,
		       Dead_W_Yield, Dead_N_Yield, Dead_C_Yield,
		       Leaf_W_Yield, Leaf_N_Yield, Leaf_C_Yield,
		       WEYRm, NEYRm, CEYRm, chemicals);
}

void
CropStandard::output (Log& log) const
{
  output_submodule (root_system, "Root", log);
  output_submodule (canopy, "Canopy", log);
  output_submodule (harvesting, "Harvest", log);
  output_submodule (production, "Prod", log);
  output_submodule (development, "Devel", log);
  output_submodule (auxiliary, "CrpAux", log);
  output_submodule (vernalization, "Vernal", log);
  output_submodule (nitrogen, "CrpN", log);
}

CropStandard::CropStandard (const AttributeList& al)
  : Crop (al),
    root_system (*new RootSystem (al.alist ("Root"))),
    canopy (*new CanopyStandard (al.alist ("Canopy"))),
    harvesting (*new Harvesting (al.alist ("Harvest"))),
    production (*new Production (al.alist ("Prod"))),
    development (*new Development (al.alist ("Devel"))),
    auxiliary (*new CrpAux (al.alist ("CrpAux"))),
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
  delete &auxiliary;
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

  syntax.add_submodule ("Root", alist, Syntax::State, Syntax::Singleton,
			"Root system.", RootSystem::load_syntax);
  syntax.add_submodule ("Canopy", alist, Syntax::State, Syntax::Singleton,
			"Canopy.", CanopyStandard::load_syntax);
  syntax.add_submodule ("Harvest", alist, Syntax::State, Syntax::Singleton,
			"Harvest parameters.", Harvesting::load_syntax);
  syntax.add_submodule ("Prod", alist, Syntax::State, Syntax::Singleton,
			"Production.", Production::load_syntax);
  syntax.add_submodule ("Devel", alist, Syntax::State, Syntax::Singleton,
			"Development and phenology.", 
			Development::load_syntax);
  syntax.add_submodule ("CrpAux", alist, Syntax::State, Syntax::Singleton,
			"Auxiliary state.", CrpAux::load_syntax);
  syntax.add_submodule ("Partit", alist, Syntax::Const, Syntax::Singleton,
			"Assimilate partitioning.", Partition::load_syntax);
  syntax.add_submodule ("Vernal", alist,
			Syntax::OptionalState, Syntax::Singleton,
			"Vernalization.", Vernalization::load_syntax);
  syntax.add_submodule ("LeafPhot", alist, Syntax::Const, Syntax::Singleton,
			"Leaf photosynthesis.", Photosynthesis::load_syntax);
  syntax.add_submodule ("CrpN", alist, Syntax::State, Syntax::Singleton,
			"Nitrogen parameters.", CrpN::load_syntax);

  syntax.add ("enable_water_stress", Syntax::Boolean, Syntax::Const,
	      "Set this to true to let water stress limit production.");
  alist.add ("enable_water_stress", true);
  syntax.add ("enable_N_stress", Syntax::Boolean, Syntax::Const,
	      "Set this true to let nitrogen stress limit production.");
  alist.add ("enable_N_stress", true);

  Librarian<Crop>::add_type ("default", alist, syntax, &make);
}
