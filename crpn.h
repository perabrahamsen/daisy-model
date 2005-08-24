// crpn.h -- Default crop nitrogen parameters.
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

#ifndef CRPN_H
#define CRPN_H

class Production;
class Soil;
class SoilWater;
class SoilNH4;
class SoilNO3;
class RootSystem;
class AttributeList;
class Syntax;
class Log;
class PLF;
class Treelog;

class CrpN 
{
  // Obsolete.
public:
  const double SeedN;		// N-content in seed [ g N/m^2 ]

  // Content.
private:
  const PLF& PtLeafCnc;		// Upper limit for N-conc in leaves
  const PLF& CrLeafCnc;		// Critical lim f. N-conc in leaves
public:
  const PLF& NfLeafCnc;		// Non-func lim f. N-conc in leaves
private:
  const PLF& PtStemCnc;		// Upper limit for N-conc in stems
  const PLF& CrStemCnc;		// Critical lim f. N-conc in stems
  const PLF& NfStemCnc;		// Non-func lim f. N-conc in stems
  const PLF& PtRootCnc;		// Upper limit for N-conc in roots
  const PLF& CrRootCnc;		// Critical lim f. N-conc in roots
public:
  const PLF& NfRootCnc;		// Non-func lim f. N-conc in roots
private:
  const PLF& PtSOrgCnc;		// Upper limit for N-conc in stor org
  const PLF& CrSOrgCnc;		// Critical lim f. N-conc in stor org
  const PLF& NfSOrgCnc;		// Non-func lim f. N-conc in stor org
public:
  const PLF& TLLeafEff;		// Translocation effiency, Leaf.
  const PLF& TLRootEff;		// Translocation effiency, Root.
public:
  double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
private:
  double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
  double NfNCnt;		// Non-func Nitrogen Content in Crop [g/m2]

  // Root uptake.
private:
  double NO3_root_min;		// Minimum NO3 conc near roots [g N/cm^3]
  double NH4_root_min;		// Minimum NH4 conc near roots [g N/cm^3]

  // Stress
public:
  double nitrogen_stress;	// Fraction of requested nitrogen didn't got.
  double nitrogen_stress_days;	// Accumulated nitrogen stress.

  // Fixation.
private:
  const double DS_fixate;	// Fixation of atmospheric N. after this DS
  const double DS_cut_fixate;	// Restore fixation this DS after cut.
  const double fixate_factor;	// Fraction of N need covered by fixation.
  double Fixated;		// N fixation from air. [g/m2/h]
  double AccFixated;		// Accumulated N fixation from air. [g/m2]
  double DS_start_fixate;	// Start fixation at this DS.


  // Simulation.
public:
  void cut (double DS);
  void content (const double DS, Production& production);
  void clear ();
  void update (int Hour, double& NCrop, double DS,
	       bool enable_N_stress,
	       const Soil& soil, const SoilWater& soil_water,
	       SoilNH4& soil_NH4, SoilNO3& soil_NO3,
               double day_fraction,
	       RootSystem& root_system);
  void output (Log& log) const;

  // Create and Destroy.
private:
  bool check_alist (const AttributeList& al, Treelog& err);
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  CrpN (const AttributeList&);
  ~CrpN ();
};

#endif // CRPN_H
