// chemical.h --- Track a chemical.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef CHEMICAL_H
#define CHEMICAL_H

#include "model_framed.h"
#include <vector>
#include <iosfwd>

class Litter;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class OrganicMatter;
class Adsorption;
class Chemistry;
class Volume;
class VCheck;
class Treelog;
class Log;
class BlockModel;
class Scope;
class Bioclimate;
class Vegetation;

class Chemical : public ModelFramed
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Some standard chemicals.
public:
  static const symbol NO3 ();
  static const symbol NH4 ();
  static const symbol DON ();
  static const symbol DOC ();

  // And standard units.
public:
  static const symbol surface_storage_unit (); // [g/m^2]
  static const symbol surface_flux_unit (); // [g/m^2/h]

  // Table.
public:
  virtual void sorption_table (const Soil& soil, const size_t cell, 
                               const double Theta, const double start,
                               const double factor, const int intervals,
                               Treelog& msg) const = 0;

  // Soil.
public:
  virtual const Adsorption& adsorption () const = 0;
  virtual double diffusion_coefficient () const = 0; // [cm^2/h] in free solute

  virtual double molar_mass () const = 0; // [g/mol], negative = unknown

  // Surface content.
  virtual double surface_release_fraction () const = 0; // []
  virtual double surface_storage_amount () const = 0;  // [g/cm^2]
  virtual double surface_immobile_amount () const = 0;  // [g/cm^2]
  virtual double litter_storage_amount () const = 0;	// [g/cm^2]
  virtual double canopy_storage_amount () const = 0;	// [g/cm^2]
  
  // Soil content.
public:
  virtual double C_below () const = 0; // Concentration in groundwater [g/cm^3]
  virtual double C_secondary (size_t i) const = 0;
  virtual double C_primary (size_t i) const = 0;
  virtual double C_average (size_t i) const = 0;
  virtual double C_to_drain (size_t i) const = 0;
  virtual double M_primary (size_t i) const = 0;
  virtual double M_secondary (size_t i) const = 0;
  virtual double M_total (size_t i) const = 0;
  virtual double total_surface (const Geometry&, 
  				double from, double to) const = 0; // [g/cm^2]
  virtual double S_secondary (size_t i) const = 0;
  virtual double S_primary (size_t i) const = 0;
  
  // Transport.
public:
  virtual void set_primary (const Soil& soil, const SoilWater& soil_water,
                            const std::vector<double>& M,
                            const std::vector<double>& J) = 0;
  virtual void set_secondary (const Soil& soil, const SoilWater& soil_water,
                              const std::vector<double>& M,
                              const std::vector<double>& J) = 0;
  virtual void set_tertiary (const std::vector<double>& S_p, 
			     const std::vector<double>& J_p) = 0;
  virtual void  add_tertiary (const std::vector<double>& M,
			      const std::vector<double>& J,
			      const std::vector<double>& S_M2B,
			      const std::vector<double>& S_indirect_drain,
			      const std::vector<double>& S_p_drain) = 0;

  // Sink.
public:
  virtual void clear () = 0;
  virtual void add_to_root_sink (const std::vector<double>&) = 0;
  virtual void add_to_transform_source (const std::vector<double>&) = 0;
  virtual void add_to_transform_sink (const std::vector<double>&) = 0;
  virtual void add_to_transform_source_secondary (const std::vector<double>&) = 0;
  virtual void add_to_transform_sink_secondary (const std::vector<double>&) = 0;
  virtual void add_to_surface_transform_source (double amount  /* [g/cm^2/h] */) = 0;
  virtual void add_to_litter_transform_source (double amount  /* [g/cm^2/h] */) = 0;
  virtual void add_to_canopy_transform_source (double amount  /* [g/cm^2/h] */) = 0;
  virtual void release_surface_colloids (double surface_release) = 0;

  // Management.
public:
  virtual void remove_all () = 0;
  virtual double total_content (const Geometry&) const = 0; // [g/m^2]
  virtual void update_C (const Soil&, const SoilWater&) = 0;
  virtual void deposit (double flux /* [g/m^2/h] */) = 0;
  virtual void spray_overhead (double amount /* [g/m^2] */) = 0;
  virtual void spray_surface (double amount /* [g/m^2] */) = 0;
  virtual void dissipate_surface (double amount /* [g/m^2] */) = 0;
  virtual void harvest (double removed, double surface) = 0;
  virtual void incorporate (const Geometry&, double amount /* [g/m^2] */, 
			    double from, double to) = 0;
  virtual void incorporate (const Geometry&, double amount /* [g/m^2] */, 
			    const Volume& volume) = 0;
  virtual void mix (const Geometry& geo, const Soil&, const SoilWater&,
		    double from, double to, double penetration) = 0;
  virtual void swap (const Geometry& geo, const Soil&, const SoilWater&,
		     double from, double middle, double to) = 0;

  // Simulation.
public:
  virtual void tick_source (const Scope&, 
                            const Geometry&, const Soil&, const SoilWater&, 
                            const SoilHeat&, const OrganicMatter&, 
                            const Chemistry&, Treelog&) = 0;
  virtual double suggest_dt () const = 0;
  virtual void tick_top (const Vegetation&, const Bioclimate&, const Litter&,
			 Chemistry&,
                         double surface_runoff_rate /* [h^-1] */,
                         double dt /* [h] */,
                         Treelog&) = 0;
  virtual void tick_surface (const double pond /* [cm] */,
                             const Geometry& geo, 
                             const Soil& soil, const SoilWater& soil_water, 
                             const double z_mixing /* [cm] */,
                             Treelog& msg) = 0;
  virtual void tick_soil (const Geometry&, const Soil&, const SoilWater&,
                          double dt, const Scope&, Treelog&) = 0;
  virtual void tick_after (const Geometry&, Treelog&) = 0;
  virtual void mixture (const Geometry& geo,
                        const double pond /* [mm] */, 
                        const double rate /* [h/mm] */,
                        const double dt /* [h]*/) = 0;
  virtual void infiltrate (const double rate /* [h^-1] */,
                           const double water /* [mm] */,
                           const double dt /* [h] */) = 0;
  virtual double down () = 0;     // [g/m^2/h]
  virtual void uptake (const Soil&, const SoilWater&, double dt) = 0;
  virtual void decompose (const Geometry& geo,
                          const Soil&, const SoilWater&, const SoilHeat&, 
                          const OrganicMatter&, Chemistry&, double dt,
			  Treelog& msg) = 0;
  virtual double decompose_soil_factor (size_t c,
					const Geometry&, const Soil&, 
					const SoilWater&, const SoilHeat&, 
					const OrganicMatter&) const = 0;
  virtual void output (Log&) const = 0;
  virtual void debug_cell (std::ostream&, const size_t c) const = 0;

  // Create and Destroy.
public:
  static const VCheck& check_buildable ();
  virtual bool check (const Scope&, 
                      const Geometry&, const Soil&, const SoilWater&,
		      const OrganicMatter&, const Chemistry&,
		      Treelog&) const = 0;
  virtual void initialize (const Scope&, const Geometry&,
                           const Soil&, const SoilWater&, const SoilHeat&,
			   Treelog&) = 0;
private:
  Chemical ();
protected:
  explicit Chemical (const BlockModel&);
public:
  ~Chemical ();
};

#endif // CHEMICAL_H
