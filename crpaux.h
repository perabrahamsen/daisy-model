// crpaux.h -- Auxiliary crop state.
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

#ifndef CRPAUX_H
#define CRPAUX_H

class AttributeList;
class Syntax;
class Log;
class AM;

struct CrpAux
{
  // Content.
  double StemRes;		// Shielded Reserves in Stems
  double PotTransp;	        // Potential Transpiration [mm/h]
  double PotCanopyAss;		// Potential Canopy Assimilation [g CH2O/m2/h]
  double CanopyAss;	        // Canopy Assimilation [g CH2O/m2/h]
  double NetPhotosynthesis;	// Net Photosynthesis [g CO2/m2/h]
  double AccNetPhotosynthesis;	// Accunulated Net Photosynthesis [g CO2/m2]
  double Respiration;		// Crop Respiration [g CO2/m2/h]
  double MaintRespiration;	// Maintenance Respiration [g CO2/m2/h]
  double GrowthRespiration;	// Growth Respiration [g CO2/m2/h]
  double RootRespiration;	// Root Respiration [g CO2/m2/h]
  double IncWLeaf;		// Leaf growth [g DM/m2/d]
  double IncWStem;		// Stem growth [g DM/m2/d]
  double IncWSOrg;		// Storage organ growth [g DM/m2/d]
  double IncWRoot;		// Root growth [g DM/m2/d]
  double DeadWLeaf;		// Leaf DM removed [g DM/m2/d]
  double DeadNLeaf;		// Leaf N removed [g N/m2/d]
  double DeadWRoot;		// Root DM removed [g DM/m2/d]
  double DeadNRoot;		// Root N removed [g N/m2/d]
  double C_Loss;		// C lost from the plant. [g/m2]

  // Simulation.
  void no_production ();
  void output (Log&) const;
  
  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  CrpAux (const AttributeList&);
  ~CrpAux ();
};

#endif // CRPAUX_H
