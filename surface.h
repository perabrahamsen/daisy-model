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
class Geometry;
class Geometry1D;
class Chemicals;
class SoilChemicals;
class Treelog;

class Surface
{
  struct Implementation;
  Implementation& impl;

public:
  // Communication with soil water.
  enum top_t { forced_pressure, forced_flux, limited_water, soil };
  top_t top_type (const Geometry&, size_t edge) const;
  double q_top (const Geometry&, size_t edge, double dt) const; // [cm]
  double h_top (const Geometry&, size_t edge, double dt) const; // [cm]
  void accept_top (double amount, const Geometry&, size_t edge, 
                   double dt, Treelog&);
  size_t last_cell (const Geometry&, size_t edge) const;

  // Column.
  const IM& matter_flux ();
  const Chemicals&  chemicals_down () const;

  // Ridge.
  void update_water (const Geometry1D& geo,
                     const Soil&, const std::vector<double>& S_,
		     std::vector<double>& h_, std::vector<double>& Theta_,
		     std::vector<double>& q, const std::vector<double>& q_p,
                     double dt);

  // Manager.
  void fertilize (const IM& /* [g/cm^2] */);
  void spray (const Chemicals& chemicals_in);
  void set_detention_capacity (double);
  void ridge (const Geometry1D& geo,
              const Soil& soil, const SoilWater& soil_water,
	      const AttributeList&);
  void unridge ();

  // Simulation.
  void mixture (const IM& soil_im /* [g/cm^2/mm] */, double dt);
  void mixture (const Geometry& geo,
                const SoilChemicals& soil_chemicals, double dt);
  void output (Log&) const;
  void tick (Treelog&, double PotSoilEvaporation, double flux_in /* [mm/h] */,
             double temp /* [dg C] */, const Geometry& geo,
             const Soil&, const SoilWater&,
             double soil_T /* [dg C] */, double dt /* [h] */);

  // Communication with bioclimate.
  double ponding () const;
  double temperature () const;
  double EpFactor () const;
  double albedo (const Geometry&, const Soil&, const SoilWater&) const;
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

