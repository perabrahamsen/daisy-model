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

#include <string>
#include <vector>
using namespace std;

class OrganicMatter;
class Geometry;
class AttributeList;
class Syntax;
class Log;
class PLF;
class AM;

class Production 
{
  // Parameters.
public:
    double CH2OReleaseRate;     // CH2O Release Rate [h-1]
    double E_Root;		// Conversion efficiency, root
    double E_Leaf;		// Conversion efficiency, leaf
    double E_Stem;		// Conversion efficiency, stem
    double E_SOrg;		// Conversion efficiency, stor. org.
    double r_Root;		// Maint. resp. coeff., root
    double r_Leaf;		// Maint. resp. coeff., leaf
    double r_Stem;		// Maint. resp. coeff., stem
    double r_SOrg;		// Maint. resp. coeff., stor. org.
    double ShldResC;		// Capacity of Shielded Reserves
    double ReMobilDS;		// Remobilization, Initial DS
    double ReMobilRt;		// Remobilization, release rate
    double ExfoliationFac;      // Exfoliation factor, 0-1
    double GrowthRateRedFac;    // Growth rate reduction factor, 0-1
    const PLF& LfDR;		// Death rate of Leafs
    const PLF& RtDR;		// Death rate of Roots
    const double Large_RtDR;	// Extra death rate for large root/shoot.
    const double IntDSRelRtRes; // Initial DS for the release of root reserves
    const double EndDSRelRtRes; // End DS for the release of root reserves
    const double RelRateRtRes;  // Release rate of root reserves
    const double LfRtRelRtRes;  // Max Leaf:Root for the release of root res.

  // State.
public:
    double CH2OPool;            // Carbonhydrate pool [g/m2]
    double WLeaf;		// Leaf dry matter weight [g/m2]
    double WStem;		// Stem dry matter weight [g/m2]
    double WRoot;		// Root dry matter weight [g/m2]
    double WSOrg;		// Storage organ dry matter weight [g/m2]
    double WDead;               // Dead plant material [g/m2]
    double CCrop;		// C stored in dry matter [g/m2]
    double CLeaf;		// Leaf C weight [g/m2]
    double CStem;		// Stem C weight [g/m2]
    double CRoot;		// Root C weight [g/m2]
    double CSOrg;		// Storage organ C weight [g/m2]
    double CDead;               // Dead plant material C [g/m2]
    double NCrop;		// Nitrogen stored in dry matter [g/m2]
    double NLeaf;		// Leaf nitrogen [g/m2]
    double NStem;		// Stem nitrogen [g/m2]
    double NRoot;		// Root nitrogen [g/m2]
    double NSOrg;		// Storage organ nitrogen [g/m2]
    double NDead;               // N in dead plant material [g/m2]
    double C_AM;                // Added C in plant material [g/m2]
    double N_AM;                // Added N in plant material [g/m2]
    AM* AM_root;		// Dead organic root matter.
    AM* AM_leaf;		// Dead organic leaf matter.

  // Queries.
public:
  double RSR () const;		// Root / Shoot ratio.
  double DM () const;		// Shoot dry matter, [kg DM/ha].
  double total_N () const;	// N content [kg N/ha]

  // Simulation.
public:
  void output (Log& log) const;

  // Create and Destroy.
public:
  void initialize (const double SeedN);
  void initialize (const string& name,
		   const vector<AttributeList*>& root,
		   const vector<AttributeList*>& dead,
		   const Geometry&, OrganicMatter&);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  Production (const AttributeList&);
  ~Production ();
};

#endif // PRODUCTION_H
