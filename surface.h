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

#ifndef SURFACE_H
#define SURFACE_H

#include "model_derived.h"
#include <vector>

class Geometry;
class Geometry1D;
class Treelog;
class Soil;
class SoilWater;
class BlockModel;
class Time;

class Surface : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;
  
public:
  // Communication with soil water.
  enum top_t { forced_pressure, forced_flux, limited_water };
  virtual top_t top_type (const Geometry&, size_t edge) const = 0;
  virtual double q_top (const Geometry&, size_t edge, const double dt) const = 0; // [cm/h]
  virtual double h_top (const Geometry&, size_t edge) const = 0; // [cm]
  virtual void accept_top (double amount, const Geometry&, size_t edge, 
			   double dt, Treelog&) = 0;

  // Column.
  virtual double runoff_rate () const = 0; // [h^-1]
  virtual double mixing_resistance () const = 0; // [h/mm]
  virtual double mixing_depth () const = 0; // [cm]
  
  // Manager.
  virtual void set_detention_capacity (double) = 0;

  // Simulation.
  virtual void update_pond_average (const Geometry& geo) = 0;
  virtual void tick (const Time&, double dt /* [h] */, 
		     double PotSoilEvaporationWet, 
		     double PotSoilEvaporationDry, 
		     double flux_in /* [mm/h] */,
		     double temp /* [dg C] */, const Geometry& geo,
		     const Soil&, const SoilWater&,
		     double soil_T /* [dg C] */, Treelog&) = 0;

  // Communication with bioclimate.
  virtual double ponding_average () const = 0; // [mm]
  virtual double ponding_max () const = 0;     // [mm]
  virtual double temperature () const = 0;     // [dg C]
  virtual double EpFactor () const = 0;        // []
  virtual double albedo (const Geometry&, const Soil&, const SoilWater&) const = 0;
  virtual double exfiltration (double dt) const = 0; // [mm/h]
  virtual double evap_soil_surface () const = 0; // [mm/h]
  virtual double evap_pond (double dt, Treelog&) const = 0; // [mm/h]
  virtual void put_ponding (double pond) = 0;	// [mm]
  virtual void set_svat_temperature (double T /* dg C */) = 0;
  
  // Create.
  virtual void initialize (const Geometry&) = 0;
private:
  Surface ();
public:
  Surface (const BlockModel&);
  ~Surface ();
};

#endif // SURFACE_H
