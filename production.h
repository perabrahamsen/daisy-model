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
#include <string>
#include <vector>
using namespace std;

class OrganicMatter;
class CrpN;
class Partition;
class Geometry;
class AttributeList;
class Syntax;
class Log;
class PLF;
class AM;
class Treelog;

class Production 
{
  // Remobilization.
private:
  const double ShldResC;	// Capacity of Shielded Reserves
  const double ReMobilDS;	// Remobilization, Initial DS
  const double ReMobilRt;	// Remobilization, release rate
  double StemRes;		// Shielded Reserves in Stems
  double remobilization (const double DS);

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
  const double Large_RtDR;	// Extra death rate for large root/shoot.
  const PLF& RtDR_T_factor;	// Temperature factor for root death rate.
  const double IntDSRelRtRes;	// Initial DS for the release of root reserves
  const double EndDSRelRtRes;	// End DS for the release of root reserves
  const double RelRateRtRes;	// Release rate of root reserves
  const double LfRtRelRtRes;	// Max Leaf:Root for the release of root res.
  const double nitrogen_stress_limit; // Allocate all ass. to SOrg above this.

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
  double RootRespiration;	// Root Respiration [g CO2/m2/h]
public:
  double IncWLeaf;		// Leaf growth [g DM/m2/d]
private:
  double IncWStem;		// Stem growth [g DM/m2/d]
  double IncWSOrg;		// Storage organ growth [g DM/m2/d]
public:
  double IncWRoot;		// Root growth [g DM/m2/d]
  double DeadWLeaf;		// Leaf DM removed [g DM/m2/d]
private:
  double DeadNLeaf;		// Leaf N removed [g N/m2/d]
  double DeadWRoot;		// Root DM removed [g DM/m2/d]
  double DeadNRoot;		// Root N removed [g N/m2/d]
  double C_Loss;		// C lost from the plant. [g/m2]

  // Queries.
private:
  double RSR () const;		// Root / Shoot ratio.
public:
  double DM () const;		// Shoot dry matter, [kg DM/ha].
  double total_N () const;	// N content [kg N/ha]
  double total_C () const;	// C content [kg N/ha]

  // Simulation.
private:
  static double maintenance_respiration (double r, double w, double T);
  static double GrowthRespCoef (double E);
public:
  void tick (double AirT, double SoilT,
	     const vector<double>& Density,
	     const Geometry& geometry,
	     double DS, double CAImRat,
	     const CrpN& nitrogen,
             double nitrogen_stress,
	     const Partition& partition,
	     double& residuals_DM, 
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil,
	     vector<double>& residuals_C_soil,
	     Treelog&);
  void none ();
  void output (Log& log) const;

  // Create and Destroy.
public:
  void initialize (const double SeedN);
  void initialize (symbol name,
		   const vector<AttributeList*>& root,
		   const vector<AttributeList*>& dead,
		   const Geometry&, OrganicMatter&);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  Production (const AttributeList&);
  ~Production ();
};

#endif // PRODUCTION_H
