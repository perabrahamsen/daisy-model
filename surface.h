// surface.h
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


#include "uzmodel.h"

class IM;
class AttributeList;
class Log;
class SoilWater;
class Soil;
class Chemicals;
class SoilChemicals;

class Surface : public UZtop
{
  struct Implementation;
  Implementation& impl;

public:

  // Communication with soil.
  bool flux_top () const;	// From UZtop.
  double q () const;
  void flux_top_on () const;
  void flux_top_off () const;
  bool accept_top (Treelog&, double);
  bool soil_top () const;
  double ponding () const;
  double temperature () const;
  int last_node () const;

  const IM& matter_flux ();
  const Chemicals&  chemicals_down () const;

  void mixture (const IM& soil_im /* g/cm^2/mm */);
  void mixture (const SoilChemicals& soil_chemicals);
  void update_water (const Soil&, const vector<double>& S_,
		     vector<double>& h_, vector<double>& Theta_,
		     vector<double>& q, const vector<double>& q_p);

  // Manager.
  void fertilize (const IM&);
  void spray (const Chemicals& chemicals_in);
  void set_detention_capacity (double);
  void ridge (const Soil& soil, const SoilWater& soil_water,
	      const AttributeList&);
  void unridge ();

  // Simulation.
  void output (Log&) const;
  void tick (Treelog&, double PotSoilEvaporation, double Water, double temp,
	     const Soil&, const SoilWater&, double soil_T);

  // Communication with bioclimate.
  double EpFactor () const;
  double albedo (const Soil& soil, const SoilWater& soil_water) const;
  double exfiltration () const; // [mm/h]
  double evap_soil_surface () const; // [mm/h]
  double evap_pond (Treelog&) const; // [mm/h]

  // Communication with external model.
  void put_ponding (double pond);	// [mm]
  void put_no3 (double no3); // [g/cm^2]
  double get_no3 () const; // [g/cm^2]
  void put_chemical (symbol, double); // [g/cm^2]
  double get_chemical (symbol) const; // [g/cm^2]

  // Create.
  static void load_syntax (Syntax&, AttributeList&);
  Surface (const AttributeList& par);
  ~Surface ();
};

