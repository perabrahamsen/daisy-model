// production.h -- Default crop production submodel.
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

#ifndef PRODUCTION_H
#define PRODUCTION_H

#include "symbol.h"
#include "soil_water.h"
#include <string>
#include <vector>
#include <boost/shared_ptr.hpp>

class OrganicMatter;
class CrpN;
class Partition;
class Geometry;
class Frame;
class FrameModel;
class FrameSubmodel;
class Log;
class PLF;
class AM;
class Treelog;
class Metalib;

class Production 
{
  // Remobilization.
private:
  const double ShldResC;	// Capacity of Shielded Reserves
  const double ReMobilDS;	// Remobilization, Initial DS
  const double ReMobilRt;	// Remobilization, release rate
  double StemRes;		// Shielded Reserves in Stems
  double remobilization (const double DS, const double dt);

  // Parameters.
private:
  const double CH2OReleaseRate;	// CH2O Release Rate [h-1]
public:
  const double E_Root;		// Conversion efficiency, root
  const double E_Leaf;		// Conversion efficiency, leaf
  const double E_Stem;		// Conversion efficiency, stem
  const double E_SOrg;		// Conversion efficiency, stor. org.
private:
  const double r_Root;		// Maint. resp. coeff., root
  const double r_Leaf;		// Maint. resp. coeff., leaf
  const double r_Stem;		// Maint. resp. coeff., stem
  const double r_SOrg;		// Maint. resp. coeff., stor. org.
  const double ExfoliationFac;	// Exfoliation factor, 0-1
  const PLF& LfDR;		// Death rate of Leafs
  const PLF& RtDR;		// Death rate of Roots
  const double Large_RtDR;	// Extra death rate for large root/shoot
  const PLF& RtDR_T_factor;	// Temperature factor for root death rate
  const double water_log_h_limit; // Pressure for water logging [cm]
  const double water_log_root_limit = 0.1; // Min root density for WL [cm/cm^3]
  const PLF& RtDR_water_log_factor; // Multiply DR with this.
  const PLF& RtDR_water_log_addend; // Add this to DR.
  const double IntDSRelRtRes;	// Initial DS for the release of root reserves
  const double EndDSRelRtRes;	// End DS for the release of root reserves
  const double RelRateRtRes;	// Release rate of root reserves
  const double LfRtRelRtRes;	// Max Leaf:Root for the release of root res.

  // State.
public:
  double CH2OPool;		// Carbonhydrate pool [g/m2]
  double WLeaf;			// Leaf dry matter weight [g/m2]
  double WStem;			// Stem dry matter weight [g/m2]
  double WRoot;			// Root dry matter weight [g/m2]
  double WSOrg;			// Storage organ dry matter weight [g/m2]
  double WDead;			// Dead plant material [g/m2]
  double CLeaf;			// Leaf C weight [g/m2]
  double CStem;			// Stem C weight [g/m2]
  double CRoot;			// Root C weight [g/m2]
  double CSOrg;			// Storage organ C weight [g/m2]
  double CDead;			// Dead plant material C [g/m2]
  double CCrop;			// C stored in dry matter [g/m2]
  double NCrop;			// Nitrogen stored in dry matter [g/m2]
  double NLeaf;			// Leaf nitrogen [g/m2]
  double NStem;			// Stem nitrogen [g/m2]
  double NRoot;			// Root nitrogen [g/m2]
  double NSOrg;			// Storage organ nitrogen [g/m2]
  double NDead;			// N in dead plant material [g/m2]
  double C_AM;			// Added C in plant material [g/m2]
  double N_AM;			// Added N in plant material [g/m2]
  AM* AM_root;			// Dead organic root matter.
  AM* AM_leaf;			// Dead organic leaf matter.

  // Auxiliary.
public:
  double PotCanopyAss;		// Potential Canopy Assimilation [g CH2O/m2/h]
  double CanopyAss;	        // Canopy Assimilation [g CH2O/m2/h]
  double NetPhotosynthesis;	// Net Photosynthesis [g CO2/m2/h]
  double AccNetPhotosynthesis;	// Accunulated Net Photosynthesis [g CO2/m2]
private:
  double Respiration;		// Crop Respiration [g CO2/m2/h]
  double MaintRespiration;	// Maintenance Respiration [g CO2/m2/h]
  double GrowthRespiration;	// Growth Respiration [g CO2/m2/h]
  double LeafRespiration;	// Total Leaf Respiration [g CO2/m2/h]
  double StemRespiration;	// Total Stem Respiration [g CO2/m2/h]
  double SOrgRespiration;	// Total SOrg Respiration [g CO2/m2/h]
  double RootRespiration;	// Total Root Respiration [g CO2/m2/h]
  double LeafMaintRespiration;	// Leaf Maintenance Respiration [g CO2/m2/h]
  double StemMaintRespiration;	// Stem Maintenance Respiration [g CO2/m2/h]
  double SOrgMaintRespiration;	// SOrg Maintenance Respiration [g CO2/m2/h]
  double RootMaintRespiration;	// Root Maintenance Respiration [g CO2/m2/h]
  double LeafGrowthRespiration;	// Leaf Growth Respiration [g CO2/m2/h]
  double StemGrowthRespiration;	// Stem Growth Respiration [g CO2/m2/h]
  double SOrgGrowthRespiration;	// SOrg Growth Respiration [g CO2/m2/h]
  double RootGrowthRespiration;	// Root Growth Respiration [g CO2/m2/h]
  double IncWLeaf;		// Leaf growth [g DM/m2/d]
  double IncWStem;		// Stem growth [g DM/m2/d]
  double IncWSOrg;		// Storage organ growth [g DM/m2/d]
  double IncWRoot;		// Root growth [g DM/m2/d]
  double DeadWLeaf;		// Leaf DM removed [g DM/m2/d]
  double DeadNLeaf;		// Leaf N removed [g N/m2/d]
  double DeadWRoot;		// Root DM removed [g DM/m2/d]
  double DeadNRoot;		// Root N removed [g N/m2/d]
  double C_Loss;		// C lost from the plant. [g/m2]
  double DailyNetRoot;          // Net root growth this day.
  double DailyNetShoot;          // Net shoot growth this day.
  double water_logged;		 // Fraction of root volume water logged [0-1]
  
  // Queries.
private:
  double RSR () const;		// Root / Shoot ratio.
public:
  double DM () const;		// Shoot dry matter, [kg DM/ha].
  double total_N () const;	// N content [kg N/ha]
  double total_C () const;	// C content [kg N/ha]
  bool root_growth () const;    // True iff daily ass. covers root resp.
  bool shoot_growth () const;    // True iff daily ass. covers leaf resp.

  // Simulation.
private:
  static double maintenance_respiration (double r, double w, double T);
  static double GrowthRespCoef (double E);
public:
  void tick (double AirT, double SoilT,
	     const std::vector<double>& Density,
	     const std::vector<double>& D, // [cm/cm^3/h]
	     const double D_tot_DM, // [g DM/h]
	     const Geometry& geometry, const SoilWater& soil_water,
	     double DS, double CAImRat,
	     const CrpN& nitrogen,
             double nitrogen_stress,
	     double NNI,
             double seed_C,
	     Partition& partition,
	     double& residuals_DM, 
	     double& residuals_N_top, double& residuals_C_top,
	     std::vector<double>& residuals_N_soil,
	     std::vector<double>& residuals_C_soil,
             double dt,
	     Treelog&);
  void tick_daily ();
  void update_carbon ();
  void none ();
  void output (Log& log) const;

  // Create and Destroy.
public:
  void initialize (const double SeedN);
  void initialize (const Metalib&, symbol name,
		   const std::vector<boost::shared_ptr<const FrameModel>/**/>& root,
		   const std::vector<boost::shared_ptr<const FrameModel>/**/>& dead,
		   const Geometry&, OrganicMatter&, Treelog&);
  static void load_syntax (Frame&);
  Production (const FrameSubmodel&);
  ~Production ();
};

#endif // PRODUCTION_H
