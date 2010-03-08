// chemical_std.C
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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

#define BUILD_DLL
#include "chemical.h"
#include "organic.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "soil.h"
#include "geometry.h"
#include "abiotic.h"
#include "adsorption.h"
#include "chemistry.h"
#include "log.h"
#include "block_model.h"
#include "frame_model.h"
#include "mathlib.h"
#include "plf.h"
#include "check.h"
#include "librarian.h"
#include "number.h"
#include "scope_soil.h"
#include "scope_multi.h"
#include "vcheck.h"
#include "memutils.h"
#include "submodeler.h"
#include "treelog.h"
#include <sstream>

struct ChemicalStandard : public Chemical
{
  // Units.
  static const symbol g_per_cm3;

  // Parameters.
  const double crop_uptake_reflection_factor;
  const double canopy_dissipation_rate;
  const double canopy_washoff_coefficient;
  const double surface_decompose_rate;
  const double diffusion_coefficient_; 
  const double decompose_rate;
  const PLF decompose_heat_factor_;
  const PLF decompose_water_factor_;
  const PLF decompose_CO2_factor;
  const PLF decompose_conc_factor;
  const PLF decompose_depth_factor;
  const PLF decompose_lag_increment;
  struct Product
  {
    const double fraction;
    const symbol chemical;
    static void load_syntax (Frame&);
    Product (const Block&);
  };
  const auto_vector<const Product*> product;
  
  const std::auto_ptr<Number> C_below_expr;
  double C_below_value;
  const std::auto_ptr<Number> initial_expr;
  const std::auto_ptr<Adsorption> adsorption_;

  // Management.
  double deposit_;
  double spray_;
  double dissipate_;
  double harvest_;
  double residuals;
  double surface_tillage;
  double litter_tillage;

  // Surface state and log.
  double snow_storage;
  double snow_in;
  double snow_out;
  
  double canopy_storage;
  double canopy_in;
  double canopy_dissipate;
  double canopy_harvest;
  double canopy_out;

  double litter_storage;
  double litter_in;
  double litter_out;

  double surface_storage;
  double surface_solute;
  double surface_immobile;
  double surface_in;
  double surface_out;
  double surface_mixture;
  double surface_runoff;
  double surface_decompose;
  double surface_transform;
  double surface_release;

  // Soil state and log.
  std::vector<double> C_avg_;   // Concentration in soil solution [g/cm^3]
  std::vector<double> C_secondary_;   // Conc. in secondary domain [g/cm^3]
  std::vector<double> C_primary_; // Conc. in primary domain [g/cm^3]
  std::vector<double> M_primary_; // Content in primary domain [g/cm^3]
  std::vector<double> M_total_; // Concentration in soil [g/cm^3]
  std::vector<double> M_error; // Accumulated error [g/cm^3]
  std::vector<double> S_secondary_;  // Secondary domain source term.
  std::vector<double> S_primary_;// Primary domain source term.
  std::vector<double> S_tertiary_;      // Source term for macropores only.
  std::vector<double> S_exchange;       // Exchange from primary to secondary.
  std::vector<double> S_drain;  // Source term for soil drainage only.
  std::vector<double> S_external; // External source term, e.g. incorp. fert.
  std::vector<double> S_permanent; // Permanent external source term.
  std::vector<double> S_root;   // Root uptake source term (negative).
  std::vector<double> S_decompose;      // Decompose source term.
  std::vector<double> S_transform;      // Transform source term.
  std::vector<double> J_primary; // Solute transport in primary matrix water.
  std::vector<double> J_secondary; // Solute transport in secondary matrix.
  std::vector<double> J_matrix;    // Solute transport log in matrix water.
  std::vector<double> J_tertiary; // Solute transport log in tertiary water.
  std::vector<double> tillage;         // Changes during tillage.
  std::vector<double> lag;

  // Utilities.
  double decompose_heat_factor (const double T) const;
  double decompose_water_factor (const double h) const;

  // Solute.
  const Adsorption& adsorption () const;
  double diffusion_coefficient () const;

  // Surface content.
  double surface_release_fraction () const; // []
  double surface_immobile_amount () const;  // [g/cm^2]
  double surface_storage_amount () const;  // [g/cm^2]

  // Soil content.
  double C_below () const; // Concentration in groundwater [g/cm^3]
  double C_secondary (size_t) const;
  double C_primary (size_t) const;
  double M_primary (size_t) const;
  double M_total (size_t) const;
  double total_surface (const Geometry&, 
                        double from, double to) const; // [g/cm^2]
  double S_secondary (size_t) const;
  double S_primary (size_t) const;
  double S_tertiary (size_t) const;
  
  // Transport.
  void set_macro_flux (size_t e, double value);
  void set_primary (const Soil& soil, const SoilWater& soil_water,
                    const std::vector<double>& M,
                    const std::vector<double>& J);
  void set_secondary (const Soil& soil, const SoilWater& soil_water,
                      const std::vector<double>& C,
                      const std::vector<double>& J);
  void set_tertiary (const std::vector<double>& S_p, 
                     const std::vector<double>& J_p);

  // Sink.
  void clear ();
  void add_to_source_secondary (const std::vector<double>&);
  void add_to_source_primary (const std::vector<double>&);
  void add_to_sink_secondary (const std::vector<double>&);
  void add_to_sink_primary (const std::vector<double>&);
  void add_to_root_sink (const std::vector<double>&);
  void add_to_decompose_sink (const std::vector<double>&);
  void add_to_transform_source (const std::vector<double>&);
  void add_to_transform_sink (const std::vector<double>&);
  void add_to_transform_source_secondary (const std::vector<double>&);
  void add_to_transform_sink_secondary (const std::vector<double>&);
  void add_to_surface_transform_source (double amount  /* [g/cm^2/h] */);
  void release_surface_colloids (double surface_release);

  // Management.
  void update_C (const Soil&, const SoilWater&);
  void deposit (const double amount, const double dt); // [g/m^2]
  void spray (const double amount, const double dt); // [g/m^2]
  void dissipate (const double amount, const double dt); // [g/m^2]
  void harvest (const double removed, const double surface, const double dt);
  void incorporate (const Geometry&, double amount, 
                    double from, double to, double dt);
  void incorporate (const Geometry&, double amount, 
                    const Volume&, double dt);
  void mix (const Geometry& geo, const Soil&, const SoilWater&,
            double from, double to, double penetration, double dt);
  void swap (const Geometry& geo, const Soil&, const SoilWater&,
             double from, double middle, double to, double dt);
  
  // Simulation.
  void tick_top (const double snow_leak_rate, // [h^-1]
                 const double canopy_cover, // [],
                 const double canopy_leak_rate, // [h^-1]
                 const double litter_cover, // [],
                 const double litter_leak_rate,
                 const double surface_runoff_rate, // [h^-1]
                 const double dt, // [h]
                 Treelog& msg);
  void tick_surface (const double pond,
                     const Geometry& geo, 
                     const Soil& soil, const SoilWater& soil_water, 
                     const double z_mixing, Treelog&);
  void tick_soil (const Units&,
                  const Geometry&, const Soil&, const SoilWater&, double dt,
                  const Scope&, Treelog&);
  void mixture (const Geometry& geo,
                const double pond /* [mm] */, 
                const double rate /* [h/mm] */,
                const double dt /* [h]*/);
  void infiltrate (const double rate, const double dt);
  double down ();                 // [g/m^2/h]
  void uptake (const Soil&, const SoilWater&, double dt);
  void decompose (const Geometry& geo,
                  const Soil&, const SoilWater&, const SoilHeat&, 
                  const OrganicMatter&, Chemistry&, double dt, Treelog&);
  void output (Log&) const;

  // Create.
  bool check (const Units&, const Scope&, 
              const Geometry&, const Soil&, const SoilWater&, 
              const Chemistry&, Treelog&) const;
  static void fillup (std::vector<double>& v, const size_t size);
  void initialize (const Units&, const Scope&, const Geometry&,
                   const Soil&, const SoilWater&, const SoilHeat&, Treelog&);
  static double find_surface_decompose_rate (const Block& al);
  ChemicalStandard (const BlockModel&);
};

const symbol 
ChemicalStandard::g_per_cm3 ("g/cm^3");

void
ChemicalStandard::Product::load_syntax (Frame& frame)
{
  frame.declare ("fraction", Attribute::Fraction (), Attribute::Const,
                 "Fraction of decomposed matter that become this chemcial.");
  frame.declare_string ("chemical", Attribute::Const, 
                 "Chemical product of decomposed matter.");
  frame.order ("fraction", "chemical");
}

ChemicalStandard::Product::Product (const Block& al)
  : fraction (al.number ("fraction")),
    chemical (al.name ("chemical"))
{ }

double
ChemicalStandard::decompose_heat_factor (const double T) const
{
  if (decompose_heat_factor_.size () < 1)
    return Abiotic::f_T0 (T);
  else
    return decompose_heat_factor_ (T);
}
double 

ChemicalStandard::decompose_water_factor (const double h) const
{
  if (decompose_water_factor_.size () < 1)
    return Abiotic::f_h (h);
  else
    return decompose_water_factor_ (h);
}

const Adsorption&
ChemicalStandard::adsorption () const
{ return *adsorption_; }

double
ChemicalStandard::diffusion_coefficient () const
{ return diffusion_coefficient_; }

double
ChemicalStandard::surface_release_fraction () const
{ return surface_release; }

double 
ChemicalStandard::surface_immobile_amount () const
{
  const double m2_per_cm2 = 0.01 * 0.01;
  return surface_immobile * m2_per_cm2; 
}

double 
ChemicalStandard::surface_storage_amount () const
{
  const double m2_per_cm2 = 0.01 * 0.01;
  return surface_storage * m2_per_cm2; 
}

double 
ChemicalStandard::C_below () const
{ return C_below_value; }

double 
ChemicalStandard::C_secondary (size_t i) const
{ return C_secondary_[i]; }

double 
ChemicalStandard::C_primary (size_t i) const
{ return C_primary_[i]; }

double 
ChemicalStandard::M_primary (size_t i) const
{ return M_primary_[i]; }

double 
ChemicalStandard::M_total (size_t i) const
{ return M_total_[i]; }

double
ChemicalStandard::total_surface (const Geometry& geo, 
                                 const double from, const double to) const
{ return geo.total_surface (M_total_, from, to); }

double 
ChemicalStandard::S_secondary (size_t i) const
{ return S_secondary_[i]; }

double 
ChemicalStandard::S_primary (size_t i) const
{ return S_primary_[i]; }

double 
ChemicalStandard::S_tertiary (size_t i) const
{ return S_tertiary_[i]; }

void 
ChemicalStandard::set_macro_flux (const size_t e, const double value)
{ J_tertiary[e] = value; }

void 
ChemicalStandard::set_primary (const Soil& soil, const SoilWater& soil_water,
                               const std::vector<double>& M,
                               const std::vector<double>& J)
{
  // Update cells.
  daisy_assert (M_primary_.size () == M.size ());
  const size_t cell_size = M.size ();
  M_primary_ = M;

  for (size_t c = 0; c < cell_size; c++)
    {
      // Try to compensate for earlier errors.
      M_primary_[c] += M_error[c];
      M_error[c] = 0.0;
      if (M_primary_[c] < 0.0)
        {
          // Did not work, save error for next timestep.
          M_error[c] += M_primary_[c];
          M_primary_[c] = 0.0;
        }
      const double Theta_primary = soil_water.Theta_primary (c);
      const double Theta_secondary = soil_water.Theta_secondary (c);
      const double Theta_matrix = Theta_primary + Theta_secondary;
      M_total_[c] = M_primary_[c] + C_secondary_[c] * Theta_secondary;
      C_primary_[c] 
        = adsorption_->M_to_C (soil, Theta_primary, c, M_primary_[c]);
      C_avg_[c] 
        = (Theta_primary * C_primary_[c] + Theta_secondary * C_secondary_[c]) 
        / Theta_matrix;
      if (iszero (Theta_secondary))
        C_secondary_[c] = C_primary_[c];
    }

  // Update fluxes.
  const size_t edge_size = J.size ();
  daisy_assert (J_primary.size () == edge_size);
  daisy_assert (J_secondary.size () == edge_size);
  J_primary = J;
  for (size_t e = 0; e < edge_size; e++)
    J_matrix[e] = J_primary[e] + J_secondary[e];

}

void 
ChemicalStandard::set_secondary (const Soil& soil, const SoilWater& soil_water,
                                 const std::vector<double>& C,
                                 const std::vector<double>& J)
{
  // Update cells.
  daisy_assert (C_secondary_.size () == C.size ());
  const size_t cell_size = C.size ();
  C_secondary_ = C;

  for (size_t c = 0; c < cell_size; c++)
    {
      const double Theta_primary = soil_water.Theta_primary (c);
      const double Theta_secondary = soil_water.Theta_secondary (c);
      const double Theta_matrix = Theta_primary + Theta_secondary;
      if (iszero (Theta_secondary))
        C_secondary_[c] = C_primary_[c];
      M_total_[c] = M_primary_[c] + C_secondary_[c] * Theta_secondary;
      C_avg_[c] 
        = (Theta_primary * C_primary_[c] + Theta_secondary * C_secondary_[c]) 
        / Theta_matrix;
    }

  // Update fluxes.
  const size_t edge_size = J.size ();
  daisy_assert (J_primary.size () == edge_size);
  daisy_assert (J_secondary.size () == edge_size);
  J_secondary = J;
  for (size_t e = 0; e < edge_size; e++)
    J_matrix[e] = J_primary[e] + J_secondary[e];

}

void 
ChemicalStandard::set_tertiary (const std::vector<double>& S_p, 
                                const std::vector<double>& J_p)
{
  daisy_assert (S_tertiary_.size () == S_p.size ());
  S_tertiary_ = S_p;
  add_to_source_secondary (S_p);
  daisy_assert (J_tertiary.size () == J_p.size ());
  J_tertiary = J_p;
}


void
ChemicalStandard::clear ()
{
  deposit_ = 0.0;
  spray_ = 0.0;
  dissipate_ = 0.0;
  harvest_ = 0.0;
  residuals = 0.0;
  surface_tillage = 0.0;
  litter_tillage = 0.0;
  surface_transform = 0.0;
  surface_release = 0.0;
  std::fill (S_secondary_.begin (), S_secondary_.end (), 0.0);
  std::fill (S_primary_.begin (), S_primary_.end (), 0.0);
  std::fill (S_external.begin (), S_external.end (), 0.0);
  std::fill (S_root.begin (), S_root.end (), 0.0);
  std::fill (S_decompose.begin (), S_decompose.end (), 0.0);
  std::fill (S_transform.begin (), S_transform.end (), 0.0);
  std::fill (J_primary.begin (), J_primary.end (), 0.0);
  std::fill (J_secondary.begin (), J_secondary.end (), 0.0);
  std::fill (J_matrix.begin (), J_matrix.end (), 0.0);
  std::fill (J_tertiary.begin (), J_tertiary.end (), 0.0);
  std::fill (tillage.begin (), tillage.end (), 0.0);
  
}

void
ChemicalStandard::add_to_source_secondary (const std::vector<double>& v)
{
  daisy_assert (S_secondary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_secondary_[i] += v[i];
      daisy_assert (std::isfinite (S_secondary_[i]));
    }
}


void
ChemicalStandard::add_to_source_primary (const std::vector<double>& v)
{
  daisy_assert (S_primary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_primary_[i] += v[i];
      daisy_assert (std::isfinite (S_primary_[i]));
    }
}

void
ChemicalStandard::add_to_sink_secondary (const std::vector<double>& v)
{
  daisy_assert (S_secondary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_secondary_[i] -= v[i];
      daisy_assert (std::isfinite (S_secondary_[i]));
    }
}

void
ChemicalStandard::add_to_sink_primary (const std::vector<double>& v)
{
  daisy_assert (S_primary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_primary_[i] -= v[i];
      daisy_assert (std::isfinite (S_primary_[i]));
    }
}

void
ChemicalStandard::add_to_root_sink (const std::vector<double>& v)
{
  daisy_assert (S_root.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_root[i] -= v[i];
  add_to_sink_secondary (v);
}

void
ChemicalStandard::add_to_decompose_sink (const std::vector<double>& v)
{
  daisy_assert (S_decompose.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_decompose[i] -= v[i];
  add_to_sink_primary (v);
}

void
ChemicalStandard::add_to_transform_sink (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] -= v[i];
  add_to_sink_primary (v);
}

void
ChemicalStandard::add_to_transform_source (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] += v[i];
  add_to_source_primary (v);
}

void
ChemicalStandard::add_to_transform_sink_secondary (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] -= v[i];
  add_to_sink_secondary (v);
}

void
ChemicalStandard::add_to_transform_source_secondary (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] += v[i];
  add_to_source_secondary (v);
}

void 
ChemicalStandard::add_to_surface_transform_source (const double amount /* [g/cm^2/h] */)
{
  const double m2_per_cm2 = 0.01 * 0.01;
  surface_transform += amount / m2_per_cm2;
}

void
ChemicalStandard::release_surface_colloids (const double surface_release_value)
{
  if (std::isnormal (surface_release))
    throw "Multiple reactions setting surface release";
  surface_release = surface_release_value;
}

void
ChemicalStandard::update_C (const Soil& soil, const SoilWater& soil_water)
{
  for (size_t i = 0; i < C_primary_.size (); i++)
    {
      C_avg_[i] = adsorption_->M_to_C (soil, soil_water.Theta (i), i,
                                       M_total_[i]);
      C_primary_[i] = C_avg_[i];
      C_secondary_[i] = C_avg_[i];
      M_primary_[i] = M_total_[i]
        - C_secondary_[i] * soil_water.Theta_secondary (i);
    }
}
  
void 
ChemicalStandard::deposit (const double amount, const double dt) // [g/m^2]
{ deposit_ += amount / dt; }

void 
ChemicalStandard::spray (const double amount, const double dt) // [g/m^2]
{ spray_ += amount / dt; }

void 
ChemicalStandard::dissipate (const double amount, const double dt) // [g/m^2]
{ dissipate_ += amount / dt; }

void 
ChemicalStandard::harvest (const double removed, const double surface,
                           const double dt)
{ 
  const double new_storage 
    = canopy_storage 
    + (spray_ + deposit_ - harvest_ - residuals) * dt;
  const double gone_rate = new_storage * removed / dt;
  harvest_ += gone_rate * (1.0 - surface); 
  residuals += gone_rate * surface;
}

void 
ChemicalStandard::incorporate (const Geometry& geo, const double amount,
                               const double from, const double to,
                               const double dt)
{ 
  daisy_assert (amount >= 0.0);
  daisy_assert (from <= 0.0);
  daisy_assert (to <= from);
  const double m2_per_cm2 = 0.01 * 0.01;
  geo.add_surface (S_external, from, to, m2_per_cm2 * amount / dt);
}

void 
ChemicalStandard::incorporate (const Geometry& geo, const double amount,
                               const Volume& volume, const double dt)
{ 
  daisy_assert (amount >= 0.0);
  const double m2_per_cm2 = 0.01 * 0.01;
  geo.add_surface (S_external, volume, m2_per_cm2 * amount / dt);
}

void 
ChemicalStandard::mix (const Geometry& geo,
                       const Soil& soil, const SoilWater& soil_water, 
                       const double from, const double to,
                       const double penetration, const double dt)
{ 
  // Removed from surface.
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
  daisy_assert (penetration <= 1.0);
  daisy_assert (penetration >= 0.0);
  const double surface_removed = surface_storage * penetration;
  surface_tillage += surface_removed / dt;
  surface_storage -= surface_removed;
  daisy_assert (surface_storage >= 0.0);
  surface_solute *= (1.0 - penetration);
  surface_immobile *= (1.0 - penetration);
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
  const double litter_removed = litter_storage * penetration;
  litter_tillage += litter_removed / dt;
  litter_storage -= litter_removed;
  const double removed = surface_removed + litter_removed;

  // Add to soil.
  const double m2_per_cm2 = 0.01 * 0.01;
  const double penetrated = removed * m2_per_cm2;
  geo.add_surface (M_total_, from, to, penetrated);
  geo.add_surface (tillage, from, to, penetrated / dt);

  // Mix.
  geo.mix (M_total_, from, to, tillage, dt);
  update_C (soil, soil_water);
}

void 
ChemicalStandard::swap (const Geometry& geo,
                        const Soil& soil, const SoilWater& soil_water,
                        const double from, const double middle, const double to,
                        const double dt)
{ 
  geo.swap (M_total_, from, middle, to, tillage, dt);
  update_C (soil, soil_water);
}

void 
ChemicalStandard::tick_top (const double snow_leak_rate, // [h^-1]
                            const double canopy_cover, // [],
                            const double canopy_leak_rate, // [h^-1]
                            const double litter_cover, // [],
                            const double litter_leak_rate, // [h^-1]
                            const double surface_runoff_rate, // [h^-1]
                            const double dt, // [h]
                            Treelog& msg)
{
  TREELOG_MODEL (msg);

  const double old_storage = snow_storage + canopy_storage + litter_storage;

  // Snow pack
  snow_in = spray_ + deposit_;
  snow_storage += snow_in * dt;

  snow_out = snow_storage * snow_leak_rate;
  snow_storage -= snow_out * dt;

  // Canopy.
  canopy_in = snow_out * canopy_cover;
  const double canopy_bypass = snow_out - canopy_in;
  canopy_harvest = harvest_;
  canopy_storage += (canopy_in - canopy_harvest - residuals) * dt;

  const double old_canopy = canopy_storage;
  const double washoff_rate 
    = canopy_washoff_coefficient * canopy_leak_rate;
  canopy_storage // Implicit solution: new = old - new * rate =>
    = old_canopy / (1.0 + dt * (canopy_dissipation_rate + washoff_rate));
  canopy_dissipate = canopy_dissipation_rate * canopy_storage;
  canopy_out = canopy_storage * washoff_rate + residuals;
  daisy_balance (old_canopy, canopy_storage, 
                 - dt * (canopy_out - residuals + canopy_dissipate));

  if (canopy_storage < 1e-18)
    {
      canopy_out += canopy_storage / dt;
      canopy_storage = 0.0;
    }

  // Volatilization bypasses the system.
  spray_ += dissipate_;
  snow_in += dissipate_;
  snow_out += dissipate_;
  canopy_in += dissipate_;
  canopy_dissipate += dissipate_;

  // Litter
  const double below_canopy = canopy_out + canopy_bypass;
  litter_in = (canopy_out - residuals + canopy_bypass) * litter_cover
    + residuals;
  const double litter_bypass = below_canopy - litter_in;
  litter_storage += litter_in * dt;
  litter_out = litter_storage * litter_leak_rate;
  litter_storage -= litter_out * dt;

  // Surface
  surface_in = litter_out + litter_bypass;
  surface_runoff = surface_storage * surface_runoff_rate; 
  surface_decompose = surface_storage * surface_decompose_rate; 
  surface_storage += (surface_in + surface_transform 
                      - surface_runoff - surface_decompose) * dt;
  if (surface_storage < 0.0)
    {
      std::ostringstream tmp;
      tmp << "Storage: " << surface_storage 
          << "; In: " << surface_in * dt 
          << "; Transform (source): " << surface_transform * dt
          << "; Runoff: " << surface_runoff * dt 
          << "; Decompose:" << surface_decompose * dt << "\n";
      surface_decompose += surface_storage / dt;
      surface_storage = 0.0;
      tmp << "Adjusted decompose = " << surface_decompose * dt
          << "; Storage = 0";
      msg.warning (tmp.str ());
    }

  // Mass balance.
  const double new_storage = snow_storage + canopy_storage + litter_storage;
  const double input = spray_ + deposit_;
  const double output = surface_in + canopy_harvest + canopy_dissipate;
  if (!approximate (new_storage, old_storage + (input - output) * dt)
      && !approximate (new_storage - old_storage, (input - output) * dt)
      && !approximate (new_storage + output * dt, old_storage + input * dt))
    {
      std::ostringstream tmp;
      tmp << "Mass balance error: "
          << "new_storage - old_storage != (input - output) * dt)\n"
          << new_storage << " - " << old_storage << " != (" << input 
          << " - " << output << ") * " << dt << "), error = "  
          << new_storage - old_storage - (input - output) * dt << " g/ha";
      msg.error (tmp.str ());
    }
}

void
ChemicalStandard::tick_surface (const double pond /* [cm] */,
                                const Geometry& geo, 
                                const Soil& soil, const SoilWater& soil_water, 
                                const double z_mixing /* [cm] */,
                                Treelog& msg)
// Divide surface storage in immobile and solute mixing layer.
{
  TREELOG_MODEL (msg);

  // Find total concentration in mixing layer.
  const double m2_per_cm2 = 0.01 * 0.01 ; // [m^2/cm^2]
  daisy_assert (surface_storage >= 0.0);
  const double M = surface_storage * m2_per_cm2 / z_mixing; // [g/cm^3]
  daisy_assert (M >= 0.0);
  const double Theta_pond = std::max (pond, 0.0) / z_mixing; // []
  daisy_assert (Theta_pond >= 0.0);

  // Now find the solute.
  surface_solute = 0.0;
  double total_area = 0.0;      // [cm^2]

  // We look at each top cell.
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  daisy_assert (edge_above_size > 0U);
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const int cell = geo.edge_other (edge, Geometry::cell_above);
      daisy_assert (geo.cell_is_internal (cell));

      // Concentration in soil water.
      const double Theta = soil_water.Theta (cell) + Theta_pond;
      daisy_assert (Theta > 0.0);
      const double C = adsorption_->M_to_C (soil, Theta, cell, M); // [g/cm^3]
      daisy_assert (C >= 0.0);
      // Accumulate based on cell surface area.
      const double area = geo.edge_area (edge); // [cm^2]
      total_area += area;
      surface_solute += C * area * Theta;       // [g/cm]
      daisy_assert (surface_solute >= 0.0);
    }
  
  // Convert solute back to surface dimensions.
  daisy_assert (total_area == geo.surface_area ());
  const double surface_area = geo.surface_area () * m2_per_cm2; // [m^2]
  daisy_assert (surface_solute >= 0.0);
  surface_solute *= z_mixing;   // [g]
  surface_solute /= surface_area; // [g/m^2]
  daisy_assert (surface_solute >= 0.0);

  // The immobile is the rest.
  surface_immobile = surface_storage - surface_solute;
  if (surface_immobile < 0.0)
    {
      daisy_approximate (surface_solute, surface_storage);
      surface_immobile = 0.0;
      surface_solute = surface_storage;
    }

  // Check that full and no adsorption is calculated right.
  if (adsorption_->full ())
    daisy_approximate (surface_storage, surface_immobile);
  else if (adsorption_->name == "none")
    daisy_approximate (surface_storage, surface_solute);
}

void                            // Called just before solute movement.
ChemicalStandard::tick_soil (const Units& units, const Geometry& geo,
                             const Soil& soil,
                             const SoilWater& soil_water,
                             const double dt,
                             const Scope& scope,
                             Treelog& msg)
{
  TREELOG_MODEL(msg);

  // Constants.
  const size_t cell_size = geo.cell_size ();

  // Find C below.
  if (!C_below_expr->tick_value (units, C_below_value, g_per_cm3, scope, msg))
    C_below_value = -1.0;

  // Initialize.
  std::fill (S_tertiary_.begin (), S_tertiary_.end (), 0.0);
  std::fill (J_tertiary.begin (), J_tertiary.end (), 0.0);

  // Permanent source.
  for (size_t c = 0; c < cell_size; c++)
    S_external[c] += S_permanent[c];
  add_to_source_secondary (S_external); 
 
  // Drainage.
  for (size_t c = 0; c < cell_size; c++)
    S_drain[c] = -soil_water.S_drain (c) * C_secondary_[c];
  add_to_source_secondary (S_drain); 

  // Exchange between primary and secondary domains.
  std::fill (S_exchange.begin (), S_exchange.end (), 0.0);
  
  for (size_t c = 0; c < cell_size; c++)
    {
      // Old water.
      const double Theta_sec_old = soil_water.Theta_secondary_old (c);
      if (Theta_sec_old < 1e-6)
        // No water to exchange.
        continue;

      // New water.
      const double Theta_sec_new = soil_water.Theta_secondary (c);
      const double M_tot = M_total (c);
      const double M_prim = M_primary (c);
      const double M_sec = M_tot - M_prim;
      if (Theta_sec_new < 1e-6)
        // Move all to primary domain.
        {
          S_exchange[c] = M_sec / dt;
          continue;
        }
      
      // Find alpha.
      const double alpha = soil.alpha (c);

      // The exchange rate based on concentration gradient.
      const double C_prim = C_primary (c);
      const double C_sec = C_secondary (c);
      if (!approximate (C_sec * Theta_sec_old, M_sec)
          && !approximate (M_prim, M_tot))
        {
          std::ostringstream tmp;
          tmp << "C_sec (" << C_sec << ") * Theta_sec_old (" << Theta_sec_old
              << ") != M_sec (" << M_sec << "), error = " 
              << M_sec - C_sec * Theta_sec_old;
          msg.warning (tmp.str ());
        }
      double S_x = alpha * (C_prim - C_sec);

      // But don't exchange more than what would result in equilibrium.
      const double Theta_prim_new = soil_water.Theta_primary (c);
      const double Theta_matrix_new = Theta_prim_new + Theta_sec_new;
      daisy_approximate (Theta_matrix_new, Theta_prim_new + Theta_sec_new);
      const double C_avg_new 
        = adsorption_->M_to_C (soil, Theta_matrix_new, c, M_tot);
      const double M_sec_new = C_avg_new * Theta_sec_new;
      const double M_prim_new = M_tot - M_sec_new;

      if (S_x > 0)
        // Flow from primary to secondary domain.
        {
          if (M_sec + S_x * dt > M_sec_new)
            // We overshoot.
            {
              if (M_sec_new > M_sec)
                // If content should increse, go for it.
                S_x = (M_sec_new - M_sec) / dt;
              else
                // If content should decrease, don't move any.
                S_x = 0.0;
            }
        }
      else
        // Flow from secondary to primary domain.
        {
          if (M_prim - S_x * dt > M_sec_new)
            // We overshoot.
            {
              if (M_prim_new > M_prim)
                // If content should increase, go for it.
                S_x = -(M_prim_new - M_prim) / dt;
              else
                // If content should decrease, don't move any.
                S_x = 0.0;
            }
        }
      // Make it official.
      S_exchange[c] = S_x;      
    }
  add_to_sink_primary (S_exchange); 
  add_to_source_secondary (S_exchange); 
}

void 
ChemicalStandard::mixture (const Geometry& geo,
                           const double pond /* [mm] */, 
                           const double R_mixing /* [h/mm] */,
                           const double dt /* [h]*/)
{
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
  // Make sure we have something to mix.
  if (pond < 1e-6 || R_mixing < 1e-99 || adsorption_->full ())
    {
      surface_mixture = 0.0;
      return;
    }

  // Mix them.
  const Chemical& chemical = *this;
  const double soil_conc
    = geo.content_hood (chemical, &Chemical::C_secondary, Geometry::cell_above)
    * (100.0 * 100.0) / 10.0; // [g/cm^3/] -> [g/m^2/mm]
  const double storage_conc = surface_solute / pond;// [g/m^2/mm]
  
  surface_mixture // [g/m^2/h]
    = bound (0.0, (storage_conc - soil_conc) / R_mixing, surface_solute / dt);
  surface_storage -= surface_mixture * dt;
  daisy_assert (surface_storage >= 0.0);
  surface_solute -= surface_mixture * dt;
  daisy_assert (surface_solute >= 0.0);
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
}

void 
ChemicalStandard::infiltrate (const double rate, const double dt)
{
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
  daisy_assert (surface_storage >= 0.0);
  daisy_assert (surface_solute >= 0.0);
  daisy_assert (surface_immobile >= 0.0);
  daisy_assert (surface_storage >= surface_solute);
  daisy_assert (rate * dt <= 1.0);

  surface_out = surface_solute * rate;
  const double loss = surface_out * dt;
  if (loss < surface_solute)
    surface_solute -= loss;
  else
    {
      surface_out = surface_solute / dt;
      surface_solute = 0.0;
    }
  daisy_assert (surface_immobile >= 0.0);
  daisy_assert (surface_solute >= 0.0);
  surface_storage = surface_immobile + surface_solute;
}

double
ChemicalStandard::down ()                 // [g/m^2/h]
{ return surface_out + surface_mixture; }

void 
ChemicalStandard::uptake (const Soil& soil, 
                          const SoilWater& soil_water,
                          const double dt)
{
  std::vector<double> uptaken (soil.size (), 0.0);

  const double rate = 1.0 - crop_uptake_reflection_factor;
  
  for (unsigned int i = 0; i < soil.size (); i++)
    uptaken[i] = C_secondary (i) * soil_water.S_root (i) * rate;
  
  add_to_root_sink (uptaken);
}

void 
ChemicalStandard::decompose (const Geometry& geo,
                             const Soil& soil, 
                             const SoilWater& soil_water,
                             const SoilHeat& soil_heat,
                             const OrganicMatter& organic_matter,
                             Chemistry& chemistry, const double dt, Treelog&)
{
  const size_t cell_size = geo.cell_size ();
  std::vector<double> decomposed (cell_size, 0.0);

  // Update lag time.
  bool found = false;
  for (size_t c = 0; c < cell_size; c++)
    {
      lag[c] += this->decompose_lag_increment (C_primary_[c]) * dt;
      
      if (lag[c] >= 1.0)
        {
          lag[c] = 1.0;
          found = true;
        }
      else if (lag[c] < 0.0)
        lag[c] = 0.0;
    }

  // No decomposition.
  if (!found)
    return;

  for (size_t c = 0; c < cell_size; c++)
    {
      const double heat_factor 
        = this->decompose_heat_factor (soil_heat.T (c));
      const double water_factor 
        = this->decompose_water_factor (soil_water.h (c));
      const double CO2_factor 
        = this->decompose_CO2_factor (organic_matter.CO2 (c));
      const double conc_factor
        = this->decompose_conc_factor (C_primary_[c]);
      const double depth_factor
        = this->decompose_depth_factor (geo.cell_z (c));
      const double rate
        = decompose_rate * heat_factor * water_factor * CO2_factor
        * conc_factor * depth_factor;
      decomposed[c] = M_primary (c) * rate;
    }

  this->add_to_decompose_sink (decomposed);

  for (size_t i = 0; i < product.size (); i++)
    {
      const symbol name = product[i]->chemical;
      if (chemistry.know (name))
        {
          Chemical& chemical = chemistry.find (name);
          const double fraction = product[i]->fraction;
          std::vector<double> created = decomposed;
          for (size_t c = 0; c < cell_size; c++)
            created[c] *= fraction;
          chemical.add_to_transform_source (created);
        }
    }
}

void
ChemicalStandard::output (Log& log) const
{
  // Parameters.
  output_derived (adsorption_, "adsorption", log);

  // Management and climate fluxes.
  output_value (deposit_, "deposit", log);
  output_value (spray_, "spray", log);
  output_variable (surface_tillage, log);
  output_variable (litter_tillage, log);

  // Surface.
  output_variable (snow_storage, log);
  output_variable (snow_in, log);
  output_variable (snow_out, log);
  output_variable (canopy_storage, log);
  output_variable (canopy_in, log);
  output_variable (canopy_dissipate, log);
  output_variable (canopy_harvest, log);
  output_variable (canopy_out, log);
  output_variable (litter_storage, log);
  output_variable (litter_in, log);
  output_variable (litter_out, log);
  output_variable (surface_storage, log);
  output_variable (surface_solute, log);
  output_variable (surface_immobile, log);
  output_variable (surface_in, log);
  output_variable (surface_runoff, log);
  output_variable (surface_decompose, log);
  output_variable (surface_transform, log);
  output_variable (surface_release, log);
  output_variable (surface_mixture, log);
  output_variable (surface_out, log);
  output_value (snow_storage + canopy_storage 
                + litter_storage + surface_storage,
                "top_storage", log);
  output_value (canopy_dissipate + canopy_harvest + surface_runoff 
                + surface_decompose - surface_transform,
                "top_loss", log);
  output_value (C_avg_, "C", log);
  output_value (C_secondary_, "C_secondary", log);
  output_value (C_primary_, "C_primary", log);
  output_value (M_total_, "M", log);
  output_value (M_primary_, "M_primary", log);
  static const symbol M_secondary_symbol ("M_secondary");
  if (log.check_leaf (M_secondary_symbol))
    {
      std::vector<double> M_secondary = M_total_;
      daisy_assert (M_primary_.size () == M_secondary.size ());
      for (size_t i = 0; i < M_primary_.size (); i++)
        M_secondary[i] -= M_primary_[i];
      log.output_entry (M_secondary_symbol, M_secondary);
    }
  output_value (M_error, "M_error", log);
  output_value (S_secondary_, "S_secondary", log);
  output_value (S_primary_, "S_primary", log);
  output_value (S_tertiary_, "S_tertiary", log);
  output_variable (S_exchange, log);
  output_variable (S_drain, log);
  output_variable (S_external, log);
  output_variable (S_permanent, log);
  output_variable (S_root, log);
  output_variable (S_decompose, log);
  output_variable (S_transform, log);
  output_variable (J_primary, log);
  output_variable (J_secondary, log);
  output_variable (J_matrix, log);
  output_variable (J_tertiary, log);
  output_variable (tillage, log);
  output_variable (lag, log);
}

bool 
ChemicalStandard::check (const Units& units, const Scope& scope, 
                         const Geometry& geo, 
                         const Soil& soil, const SoilWater& soil_water,
                         const Chemistry& chemistry,
                         Treelog& msg) const
{
  const size_t cell_size = geo.cell_size ();

  // Warn against untraced chemicals.
  if (!chemistry.know (name) && !chemistry.ignored (name))
    msg.warning ("This chemical will not be traced");
  else 
    for (size_t i = 0; i < product.size (); i++)
      {
        const symbol chemical = product[i]->chemical;
        if (!chemistry.know (chemical) && !chemistry.ignored (chemical))
          msg.warning ("Decompose product '" + chemical.name () 
                       + "' will not be traced");
      }

  bool ok = true;

  if (!C_below_expr->check_dim (units, scope, g_per_cm3, msg))
    ok = false;

  const bool solid = adsorption_->full ();

  for (size_t i = 0; i < cell_size; i++)
    {
      try 
        {   
          const double Theta_primary = soil_water.Theta_primary (i);
          const double Theta_secondary = soil_water.Theta_secondary (i);
          const double M = M_total_[i];
          const double M_primary = M_primary_[i];
          const double C = C_avg_[i];
          const double C_secondary = C_secondary_[i];
          const double C_primary = C_primary_[i];
          
          if (!approximate (adsorption_->M_to_C (soil, Theta_primary, i, 
                                                 M_primary),
                            C_primary))
            throw "C_primary does not match M_primary";
          if (!approximate (M_primary, M - C_secondary * Theta_secondary))
            throw "M_primary should be M - C_secondary * Theta"; 
          if (M_primary > M)
            throw "M_primary > M";

          if (iszero (soil_water.Theta_secondary (i)))
            {
              if (!approximate (C_secondary, C))
                throw "C_secondary should be C when there is no secondary water";
              if (!approximate (C_primary, C))
                throw "C_primary should be C for when there is no secondary water";
              if (!approximate (M_primary, M))
                throw "M_primary should be M for when there is no secondary water";
            }
          
          if (iszero (M))
            {
              if (std::isnormal (C))
                throw "C & M mismatch in solute";
            }
          else if (iszero (C))
            {
              if (std::isnormal (M) && !solid)
                throw "No solute part";
            }
          else if (solid)
            {
              if (std::isnormal (C_avg_[i]))
                throw "C should be zero for non-solutes";
            }
        }
      catch (const char *const error)
        {
          std::stringstream tmp;
          tmp << name << "[" << i << "]";
          Treelog::Open next (msg, tmp.str ());
          msg.error (error);
          ok = false;
        }
    }
  return ok;
}

void 
ChemicalStandard::fillup (std::vector<double>& v, const size_t size)
{
  if (v.size () > 0)
    {
      // Fill it up.
      while (v.size () < size)
        v.push_back (v[v.size () - 1]);
      if (v.size () > size)
        throw ("To many members of sequence");
    }
}

void
ChemicalStandard::initialize (const Units& units, const Scope& parent_scope,
                              const Geometry& geo,
                              const Soil& soil, const SoilWater& soil_water, 
                              const SoilHeat& soil_heat,
                              Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  C_below_expr->initialize (units, parent_scope, msg);

  std::vector<double> Ms;
  geo.initialize_layer (C_avg_, frame (), "C", msg);
  geo.initialize_layer (C_secondary_, frame (), "C_secondary", msg);
  geo.initialize_layer (M_total_, frame (), "M", msg);
  geo.initialize_layer (Ms, frame (), "Ms", msg);

  fillup (C_avg_, cell_size);
  fillup (C_secondary_, cell_size);
  fillup (M_total_, cell_size);
  fillup (Ms, cell_size);

  if (M_total_.size () == 0 && C_avg_.size () == 0)
    {
      if (Ms.size () != 0)
        {
          daisy_assert (Ms.size () == cell_size);

          for (size_t i = M_total_.size (); i < Ms.size (); i++)
            M_total_.push_back (Ms[i] * soil.dry_bulk_density (i));
        }
      if (M_total_.size () == 0 && C_avg_.size () == 0)
        {
          ScopeSoil scope_soil (geo, soil, soil_water, soil_heat);
          ScopeMulti multi (parent_scope, scope_soil);
          daisy_assert (cell_size > 0);
          scope_soil.set_cell (0);
          if (!initial_expr->initialize (units, multi, msg))
            msg.error ("Could not initialize 'inital_expr'");
          for (size_t c = 0; c < cell_size; c++)
            { 
              scope_soil.set_cell (c);
              double value = 0.0;
              if (!initial_expr->tick_value (units,
                                             value, g_per_cm3, multi, msg))
                msg.error ("Could not evaluate 'inital_expr'");
              M_total_.push_back (value);
            }
        }
    }

  for (size_t i = 0; i < cell_size; i++)
    {
      const double Theta = soil_water.Theta (i);
      const double Theta_primary = soil_water.Theta_primary (i);
      const double Theta_secondary = soil_water.Theta_secondary (i);
      daisy_assert (approximate (Theta, Theta_secondary + Theta_primary));
      const bool has_C_secondary = C_secondary_.size () > i;
      const bool has_C_avg  = C_avg_.size () > i;
      const bool has_M_total  = M_total_.size () > i;
      daisy_assert (has_C_avg || has_M_total);

      if (iszero (Theta_secondary))
        // No secondary water.
        {
          if (!has_C_avg)
            C_avg_.push_back (adsorption_->M_to_C (soil, Theta, i, 
                                                   M_total_[i]));
          if (!has_M_total) 
            M_total_.push_back (adsorption_->C_to_M (soil, Theta, i,
                                                     C_avg_[i])); 
          if (!has_C_secondary)
            C_secondary_.push_back (C_avg_[i]);
          C_primary_.push_back (C_avg_[i]);
          M_primary_.push_back (M_total_[i]);
        }
      else if (!has_C_secondary)
        // Secondary water in equilibrium.
        {
          if (!has_C_avg)
            C_avg_.push_back (adsorption_->M_to_C (soil, Theta, i, 
                                                   M_total_[i]));
          if (!has_M_total) 
            M_total_.push_back (adsorption_->C_to_M (soil, Theta, i, 
                                                     C_avg_[i])); 
          C_primary_.push_back (C_avg_[i]);
          C_secondary_.push_back (C_avg_[i]);
          M_primary_.push_back (M_total_[i] 
                                - C_secondary_[i] * Theta_secondary);
        }
      else if (has_C_avg)
        // Average and secondary concentrations known.
        {
          // Theta * C_a = Theta_i * C_i + Theta_m * C_m
          // => C_i = (Theta * C_a - Theta_m * C_m) / Theta_i
          C_primary_.push_back ((Theta * C_avg_[i]
                                 - Theta_secondary * C_secondary_[i])
                                / Theta_primary);
          M_primary_.push_back (adsorption_->C_to_M (soil, Theta_primary, i, 
                                                     C_primary_[i]));
          if (!has_M_total)
            M_total_.push_back (M_primary_[i] 
                                + Theta_secondary * C_secondary_[i]);
        }
      else
        // Averarage concentration and total matter known.
        {
          daisy_assert (has_M_total);
          M_primary_.push_back (M_total_[i]
                                - C_secondary_[i] * Theta_secondary);
          C_primary_.push_back (adsorption_->M_to_C (soil, 
                                                     Theta_primary,
                                                     i,
                                                     M_primary_[i]));
          C_avg_.push_back ((C_secondary_[i] * Theta_secondary
                             + C_primary_[i] * Theta_primary)
                            / Theta);
        }
    }

  daisy_assert (C_secondary_.size () == cell_size);
  daisy_assert (C_primary_.size () == cell_size);
  daisy_assert (M_primary_.size () == cell_size);
  daisy_assert (M_total_.size () == cell_size);
  M_error.insert (M_error.begin (), cell_size, 0.0);
  S_secondary_.insert (S_secondary_.begin (), cell_size, 0.0);
  S_primary_.insert (S_primary_.begin (), cell_size, 0.0);
  S_tertiary_.insert (S_tertiary_.begin (), cell_size, 0.0);
  S_exchange.insert (S_exchange.begin (), cell_size, 0.0);
  S_drain.insert (S_drain.begin (), cell_size, 0.0);
  S_external.insert (S_external.begin (), cell_size, 0.0);
  if (S_permanent.size () < cell_size)
    S_permanent.insert (S_permanent.end (), 
                        cell_size - S_permanent.size (),
                        0.0);
  S_root.insert (S_root.begin (), cell_size, 0.0);
  S_decompose.insert (S_decompose.begin (), cell_size, 0.0);
  S_transform.insert (S_transform.begin (), cell_size, 0.0);
  J_primary.insert (J_primary.begin (), edge_size, 0.0);
  J_secondary.insert (J_secondary.begin (), edge_size, 0.0);
  J_matrix.insert (J_matrix.begin (), edge_size, 0.0);
  J_tertiary.insert (J_tertiary.begin (), edge_size, 0.0);
  tillage.insert (tillage.begin (), cell_size, 0.0);
  daisy_assert (tillage.size () == cell_size);
  lag.insert (lag.end (), cell_size - lag.size (), 0.0);
}

double
ChemicalStandard::find_surface_decompose_rate (const Block& al)
{
  if (al.check ("surface_decompose_rate"))
    return al.number ("surface_decompose_rate");
  if (al.check ("surface_decompose_halftime"))
    return halftime_to_rate (al.number ("surface_decompose_halftime"));
  if (al.check ("decompose_rate"))
    return al.number ("decompose_rate");
  
  return halftime_to_rate (al.number ("decompose_halftime"));
}

ChemicalStandard::ChemicalStandard (const BlockModel& al)
  : Chemical (al),
    crop_uptake_reflection_factor 
    /**/ (al.number ("crop_uptake_reflection_factor")),
    canopy_dissipation_rate 
    /**/ (al.check ("canopy_dissipation_rate")
          ? al.number ("canopy_dissipation_rate")
          : (al.check ("canopy_dissipation_halftime")
             ? halftime_to_rate (al.number ("canopy_dissipation_halftime"))
             : al.number ("canopy_dissipation_rate_coefficient"))),
    canopy_washoff_coefficient (al.number ("canopy_washoff_coefficient")),
    surface_decompose_rate (find_surface_decompose_rate (al)),
    diffusion_coefficient_ (al.number ("diffusion_coefficient") * 3600.0),
    decompose_rate (al.check ("decompose_rate")
                    ? al.number ("decompose_rate")
                    : halftime_to_rate (al.number ("decompose_halftime"))),
    decompose_heat_factor_ (al.plf ("decompose_heat_factor")),
    decompose_water_factor_ (al.plf ("decompose_water_factor")),
    decompose_CO2_factor (al.plf ("decompose_CO2_factor")),
    decompose_conc_factor (al.plf ("decompose_conc_factor")),
    decompose_depth_factor (al.plf ("decompose_depth_factor")),
    decompose_lag_increment (al.plf ("decompose_lag_increment")),
    product (map_submodel_const<Product> (al, "decompose_products")),
    C_below_expr (Librarian::build_item<Number> (al, "C_below")),
    C_below_value (-42.42e42),
    initial_expr (Librarian::build_item<Number> (al, "initial")),
    adsorption_ (Librarian::build_item<Adsorption> (al, "adsorption")),
    deposit_ (0.0), 
    spray_ (0.0),
    dissipate_ (0.0),
    harvest_ (0.0),
    residuals (0.0),
    surface_tillage (0.0),
    litter_tillage (0.0),
    snow_storage (al.number ("snow_storage")),
    snow_in (0.0),
    snow_out (0.0),
    canopy_storage (al.number ("canopy_storage")),
    canopy_in (0.0),
    canopy_dissipate (0.0),
    canopy_harvest (0.0),
    canopy_out (0.0),
    litter_storage (al.number ("litter_storage")),
    litter_in (0.0),
    litter_out (0.0),
    surface_storage (al.number ("surface_storage")),
    surface_solute (0.0),
    surface_immobile (surface_storage),
    surface_in (0.0),
    surface_out (0.0),
    surface_mixture (0.0),
    surface_runoff (0.0),
    surface_decompose (0.0),
    surface_transform (0.0),
    surface_release (0.0),
    S_permanent (al.number_sequence ("S_permanent")),
    lag (al.check ("lag")
         ? al.number_sequence ("lag")
         : std::vector<double> ())
{ }

struct NumberInitialC : public Number
{
  const double C;

  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  symbol dimension (const Scope&) const
  { 
    static const symbol unit ("g/cm^3");
    return unit; 
  }
  bool missing (const Scope& scope) const
  { return !scope.check ("Theta"); }
  double value (const Scope& scope) const
  { 
    const double Theta = scope.number ("Theta");
    return C * Theta;
  }

  // Create.
  bool initialize (const Units&, const Scope&, Treelog&)
  { return true; }
  bool check (const Units&, const Scope&, Treelog&) const
  { return true; }

  NumberInitialC (const BlockModel& al)
    : Number (al),
      C (al.number ("C"))
  { }
};

static struct NumberInitialCSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberInitialC (al); }
  NumberInitialCSyntax ()
    : DeclareModel (Number::component, "initial_C", "\
Find initial content from concentration.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare ("C", "g/cm^3", Attribute::Const, "\
Initial concentration in soil water.");
    frame.order ("C");
  }
} NumberInitialC_syntax;

static struct InitialZeroSyntax : public DeclareParam
{ 
  InitialZeroSyntax ()
    : DeclareParam (Number::component, "initial_zero", "const", "\
Initial zero concentration in soil water.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("value", 0.0, "g/cm^3");
  }
} InitialZero_syntax;

static struct ZeroGradientSyntax : public DeclareParam
{ 
  ZeroGradientSyntax ()
    : DeclareParam (Number::component, "zero_gradient", "const", "\
Assume same concentration in groundwater as in the bottom of the soil profile.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("value", -1.0, "g/cm^3");
  }
} ZeroGradient_syntax;

static struct ChemicalStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ChemicalStandard (al); }
  ChemicalStandardSyntax ()
    : DeclareModel (Chemical::component, "default", "\
Read chemical properties as normal Daisy parameters.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  { 
    bool ok = true;

    static bool warned = false;
    if (al.check ("canopy_dissipation_rate_coefficient") && !warned)
      {
        msg.entry ("OBSOLETE: Use 'canopy_dissipation_rate' instead "
                   "of 'canopy_dissipation_rate_coefficient'");
        warned = true;
      }

    if (!al.check ("canopy_dissipation_rate")
        && !al.check ("canopy_dissipation_halftime")
        && !al.check ("canopy_dissipation_rate_coefficient"))
      {
        msg.entry ("\
You must specify 'canopy_dissipation_rate' or 'canopy_dissipation_halftime'");
        ok = false;
      }
    if (al.check ("canopy_dissipation_rate") 
        && al.check ("canopy_dissipation_halftime"))
      {
        msg.entry ("\
You may not specify both 'canopy_dissipation_rate' and \
'canopy_dissipation_halftime'");
        ok = false;
      }

    if (al.check ("surface_decompose_rate") 
        && al.check ("surface_decompose_halftime"))
      {
        msg.entry ("\
You may not specify both 'surface_decompose_rate' and \
'surface_decompose_halftime'");
        ok = false;
      }

    if (!al.check ("decompose_rate") && !al.check ("decompose_halftime"))
      {
        msg.entry ("\
You must specify 'decompose_rate' or 'decompose_halftime'");
        ok = false;

      }
    if (al.check ("decompose_rate") && al.check ("decompose_halftime"))
      {
        msg.entry ("\
You may not specify both 'decompose_rate' and 'decompose_halftime'");
        ok = false;
      }
    return ok;
  }

  static void load_C (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Attribute::Const, "\
Concentration in water."); }

  static void load_C_secondary (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Attribute::Const, "\
Concentration in secondary domain."); }

  static void load_C_primary (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Attribute::Const, "\
Concentration in primary domain."); }

  static void load_M (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Attribute::Const, "\
Total mass per volume water, soil, and air."); }

  static void load_M_primary (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Attribute::Const, "\
Primary domain mass per volume water, soil, and air."); }

  static void load_Ms (Frame& frame)
  { Geometry::add_layer (frame, Attribute::Fraction (), Attribute::Const, "\
Mass in dry soil.\n\
This include all matter in both soil and water, relative to the\n\
dry matter weight.\n\
Only for initialization of the 'M' parameter."); }

  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    Model::load_model (frame);

    // Surface parameters.
    frame.declare_fraction ("crop_uptake_reflection_factor", Attribute::Const, "\
How much of the chemical is reflected at crop uptake.");
    frame.set ("crop_uptake_reflection_factor", 1.0);
    frame.declare ("canopy_dissipation_rate", "h^-1", 
                   Check::fraction (), Attribute::OptionalConst,
                   "How fast does the chemical dissipate on canopy.\n\
You must specify it with either 'canopy_dissipation_halftime' or\n\
'canopy_dissipation_rate'.");
    frame.declare ("canopy_dissipation_halftime", "h", 
                   Check::positive (), Attribute::OptionalConst,
                   "How fast does the chemical dissipate on canopy.\n\
You must specify it with either 'canopy_dissipation_halftime' or\n\
'canopy_dissipation_rate'.");
    frame.declare ("canopy_dissipation_rate_coefficient", "h^-1", 
                   Check::fraction (), Attribute::OptionalConst,
                   "Obsolete alias for 'canopy_dissipation_rate'.");
    frame.declare_fraction ("canopy_washoff_coefficient", Attribute::Const, "\
Fraction of the chemical that follows the water off the canopy.");
    frame.declare ("surface_decompose_rate", "h^-1", 
                   Check::fraction (), Attribute::OptionalConst,
                   "How fast does the chemical decomposee on surface.\n\
You must specify it with either 'surface_decompose_halftime' or\n\
'surface_decompose_rate'.  If neither is specified, 'decompose_rate' is used.");
    frame.declare ("surface_decompose_halftime", "h", 
                   Check::positive (), Attribute::OptionalConst,
                   "How fast does the chemical decompose on surface.\n\
You must specify it with either 'surface_decompose_halftime' or\n\
'surface_decompose_rate'.  If neither is specified, 'decompose_rate' is used.");

    // Soil parameters.
    frame.declare ("diffusion_coefficient", "cm^2/s", Check::non_negative (),
                   Attribute::Const, "Diffusion coefficient.");
    frame.declare ("decompose_rate", "h^-1", Check::fraction (),
                   Attribute::OptionalConst,
                   "How fast the chemical is being decomposed in the soil.\n\
You must specify it with either 'decompose_rate' or 'decompose_halftime'.");
    frame.declare ("decompose_halftime", "h", Check::positive (),
                   Attribute::OptionalConst,
                   "How fast the chemical is being decomposed in the soil.\n\
You must specify it with either 'decompose_rate' or 'decompose_halftime'.");
    frame.declare ("decompose_heat_factor", "dg C", Attribute::None (),
                   Attribute::Const, "Heat factor on decomposition.");
    frame.set ("decompose_heat_factor", PLF::empty ());
    frame.declare ("decompose_water_factor", "cm", Attribute::None (),
                   Attribute::Const,
                   "Water potential factor on decomposition.");
    frame.set ("decompose_water_factor", PLF::empty ());
    frame.declare ("decompose_CO2_factor", "g CO2-C/cm^3/h", Attribute::None (),
                   Attribute::Const,
                   "CO2 development factor on decomposition.");
    frame.set ("decompose_CO2_factor", PLF::always_1 ());
    frame.declare ("decompose_conc_factor", "g/cm^3 H2O", Attribute::None (),
                   Attribute::Const,
                   "Concentration development factor on decomposition.");
    frame.set ("decompose_conc_factor", PLF::always_1 ());
    frame.declare ("decompose_depth_factor", "cm", Attribute::None (),
                   Attribute::Const,
                   "Depth influence on decomposition.");
    frame.set ("decompose_depth_factor", PLF::always_1 ());
    frame.declare ("decompose_lag_increment", 
                   "g/cm^3", "h^-1", Attribute::Const,
                   "Increment lag with the value of this PLF for the current\n\
concentration each hour.  When lag in any cell reaches 1.0,\n\
decomposition begins.  It can never be more than 1.0 or less than 0.0.");
    frame.set ("decompose_lag_increment", PLF::always_1 ());
    frame.declare_object ("C_below", Number::component, 
                          Attribute::Const, Attribute::Singleton, "\
Concentration below the layer of soil being examined.\n\
Use a negative number to indicate same concentration as in lowest cell.");
    frame.declare_submodule_sequence ("decompose_products", Attribute::Const, "\
List of products from decomposition.", ChemicalStandard::Product::load_syntax);
    frame.set_empty ("decompose_products");
    frame.set ("C_below", "zero_gradient");
    frame.declare_object ("initial", Number::component, 
                          Attribute::Const, Attribute::Singleton, "\
Initial content if otherwise unspecified. [g/cm^3]");
    frame.set ("initial", "initial_zero");
    frame.declare_object ("adsorption", Adsorption::component, 
                          Attribute::Const, Attribute::Singleton, "\
Instant equilibrium between sorbed and solute phases.\n\
\n\
Specify the equilibrium model here for chemicals where the sorbed and\n\
solute phases typically reaches equilibrium within a single timestep.\n\
Slower adsorption processes should be modelled as two chemicals, one\n\
with 'none' adsorption and one with 'full' adsorption, and an\n\
'adsorption' reaction between them.");
    frame.set ("adsorption", "none");

    // Management and climate fluxes.
    frame.declare ("deposit", "g/m^2/h", Attribute::LogOnly,
                   "Amount deposited from the atmosphere.");
    frame.declare ("spray", "g/m^2/h", Attribute::LogOnly,
                   "Amount currently being applied.");
    frame.declare ("surface_tillage", "g/m^2/h", Attribute::LogOnly, 
                   "Amount removed from surface due to tillage operations.");
    frame.declare ("litter_tillage", "g/m^2/h", Attribute::LogOnly, 
                   "Amount removed from litter due to tillage operations.");

    // Surface variables.
    frame.declare ("snow_storage", "g/m^2", Attribute::State, 
                   "Stored in the snow pack.");
    frame.set ("snow_storage", 0.0);
    frame.declare ("snow_in", "g/m^2/h", Attribute::LogOnly, 
                   "Entering snow pack.");
    frame.declare ("snow_out", "g/m^2/h", Attribute::LogOnly, 
                   "Leaking from snow pack.");

    frame.declare ("canopy_storage", "g/m^2", Attribute::State, 
                   "Stored on the canopy.");
    frame.set ("canopy_storage", 0.0);
    frame.declare ("canopy_in", "g/m^2/h", Attribute::LogOnly, 
                   "Entering canopy.");
    frame.declare ("canopy_dissipate", "g/m^2/h", Attribute::LogOnly, 
                   "Dissipating from canopy.");
    frame.declare ("canopy_out", "g/m^2/h", Attribute::LogOnly, 
                   "Falling through or off the canopy.");
    frame.declare ("canopy_harvest", "g/m^2/h", Attribute::LogOnly, 
                   "Amount removed with crop harvest.");

    frame.declare ("litter_storage", "g/m^2", Attribute::State, 
                   "Stored in the litter (mulch, surface residuals).");
    frame.set ("litter_storage", 0.0);
    frame.declare ("litter_in", "g/m^2/h", Attribute::LogOnly, 
                   "Entering litter .");
    frame.declare ("litter_out", "g/m^2/h", Attribute::LogOnly, 
                   "Leaking from litter.");

    frame.declare ("surface_storage", "g/m^2", Attribute::State, 
                   "Stored on the soil surface.\n\
This includes the mixing layer, and constitute 'surface_solute'\n\
and 'surface_immobile'.");
    frame.set ("surface_storage", 0.0);
    frame.declare ("surface_solute", "g/m^2", Attribute::LogOnly, 
                   "Stored in the soil water of the mixing layer.\n\
This is part of 'surface_storage'.");
    frame.declare ("surface_immobile", "g/m^2", Attribute::LogOnly, 
                   "Bound to soil particles in the mixing layer.\n\
This is part of 'surface_storage'.");
    frame.declare ("surface_in", "g/m^2/h", Attribute::LogOnly, 
                   "Falling on the bare soil surface.");
    frame.declare ("surface_runoff", "g/m^2/h", Attribute::LogOnly, 
                   "Removed through lateral movement on the soil.");
    frame.declare ("surface_decompose", "g/m^2/h", Attribute::LogOnly, 
                   "Decomposed from the surface.");
    frame.declare ("surface_transform", "g/m^2/h", Attribute::LogOnly, 
                   "Added through transformation to surface.");
    frame.declare ("surface_mixture", "g/m^2/h", Attribute::LogOnly, 
                   "Entering the soil through mixture with ponded water.");
    frame.declare ("surface_out", "g/m^2/h", Attribute::LogOnly, 
                   "Entering the soil with water infiltration.");
    frame.declare ("surface_release", Attribute::Fraction (), Attribute::LogOnly, "\
Fraction of available soil particles released as colloids this timestep.\n\
Only relevant for chemicals representing colloids.\n\
\n\
The idea behind this is that reactions that generate colloids will set the\n\
value of this variable, and then reactions that convert immobile chemicals\n\
into colloid bound chemicals will use it.  For this to work, the reactions\n\
that set the variable must be listed before the reactions that us it.\n\
\n\
Note that the value is relative to the current timestep.");
    frame.declare ("top_storage", "g/m^2", Attribute::LogOnly, "\
Sum of above ground (surface, liter, snow, canopy) storage.");
    frame.declare ("top_loss", "g/m^2/h", Attribute::LogOnly, "\
Amount lost from the system from the surface.\n                         \
This includes runoff, canopy dissipation and harvest, but not soil\n    \
infiltration.  It also includes the net loss through transformation,\n  \
which can be negative.");

    // Soil variables.
    Geometry::add_layer (frame, Attribute::OptionalState, "C", load_C);
    Geometry::add_layer (frame, Attribute::OptionalState, "C_secondary",
                         load_C_secondary);
    Geometry::add_layer (frame, Attribute::LogOnly, "C_primary", load_C_primary);
    Geometry::add_layer (frame, Attribute::OptionalState, "M", load_M);
    Geometry::add_layer (frame, Attribute::LogOnly, "M_primary", load_M_primary);
    Geometry::add_layer (frame, Attribute::OptionalConst, "Ms", load_Ms);
    frame.declare ("M_secondary", "g/cm^3", 
                   Attribute::LogOnly, Attribute::SoilCells, 
                   "Mass in secondary domain.");
    frame.declare ("M_error", "g/cm^3", Attribute::LogOnly, Attribute::SoilCells, 
                   "Mass substracted to avoid negative values.");
    frame.declare ("S_secondary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Secondary matrix source term.");
    frame.declare ("S_primary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Primary matrix source term.");
    frame.declare ("S_tertiary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term for tertiary (macropore) domain.");
    frame.declare ("S_exchange", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Exchange from primary to secondary domain.");
    frame.declare ("S_drain", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term (soil drainage only).");
    frame.declare ("S_external", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "External source, such as incorporated fertilizer.");
    frame.declare ("S_permanent", "g/cm^3/h", Attribute::State, Attribute::SoilCells,
                   "Permanent external source, e.g. subsoil irrigation.");
    std::vector<double> empty;
    frame.set ("S_permanent", empty);
    frame.declare ("S_root", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term (root uptake only, always negative).");
    frame.declare ("S_decompose", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term for decompose, is never positive.");
    frame.declare ("S_transform", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term for transformations other than sorption.");
    frame.declare ("J_primary", "g/cm^2/h", Attribute::LogOnly, Attribute::SoilEdges,
                   "Transportation in primary matrix water (positive up).");
    frame.declare ("J_secondary", "g/cm^2/h", Attribute::LogOnly, Attribute::SoilEdges,
                   "Transportation in secondary matrix water (positive up).");
    frame.declare ("J_matrix", "g/cm^2/h", Attribute::LogOnly, Attribute::SoilEdges,
                   "Transportation in matrix (positive up).");
    frame.declare ("J_tertiary", "g/cm^2/h", Attribute::LogOnly, Attribute::SoilEdges,
                   "Transportation in tertiary water (positive up).");
    frame.declare ("tillage", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Changes during tillage.");
    frame.declare ("lag", Attribute::None (), Attribute::OptionalState, 
                   Attribute::SoilCells,
                   "This state variable grows with lag_increment (C) each hour.\n\
When it reached 1.0, decomposition begins.");
  }
} ChemicalStandard_syntax;

static struct ChemicalNutrientSyntax : public DeclareParam
{
  ChemicalNutrientSyntax ()
    : DeclareParam (Chemical::component, "nutrient", "default", "\
Plants eat this stuff.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("crop_uptake_reflection_factor", 1.0); // Specific uptake code.
    frame.set ("canopy_dissipation_rate", 0.0);
    frame.set ("canopy_washoff_coefficient", 1.0);
    frame.set ("decompose_rate", 0.0);
  }
} ChemicalNutrient_syntax;

static struct ChemicalNitrogenSyntax : public DeclareParam
{
  ChemicalNitrogenSyntax ()
    : DeclareParam (Chemical::component, "N", "nutrient", "\
Non-organic nitrogen.")
  { }
  void load_frame (Frame&) const
  { }
} ChemicalNitrogen_syntax;

static struct InitialNO3Syntax : public DeclareParam
{ 
  InitialNO3Syntax ()
    : DeclareParam (Number::component, "initial_NO3", "initial_C", "\
Initial NO3 concentration in soil water.")
  { }
  void load_frame (Frame& frame) const
  {
    // We initialize to approximatey half the allowed content in
    // drinking water [ 0.5 * 100 mg NO3/l ~= 5.0e-6 g NO3-N/cm^3 ]
    frame.set ("C", 5e-6); 
  }
} InitialNO3_syntax;

static struct ChemicalNO3Syntax : public DeclareParam
{ 
  ChemicalNO3Syntax ()
    : DeclareParam (Chemical::component, "NO3", "N", "\
Nitrate-N.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("diffusion_coefficient", 2.0e-5);
    frame.set ("initial", "initial_NO3");
  }
} ChemicalNO3_syntax;

static struct AdsorptionNH4Syntax : public DeclareParam
{ 
  AdsorptionNH4Syntax ()
    : DeclareParam (Adsorption::component, "NH4", "linear", "\
Adsorption of ammonium.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("K_clay", 117.116);
    frame.set ("K_OC", 117.116);
  }
} AdsorptionNH4_syntax;

static struct InitialNH4Syntax : public DeclareParam
{ 
  InitialNH4Syntax ()
    : DeclareParam (Number::component, "initial_NH4", "initial_C", "\
Initial NH4 concentration in soil water.")
  { }
  void load_frame (Frame& frame) const
  {
    // We initialize to approximatey 5% of the N corresponding to the
    // allowed content of NO3 in drinking water.
    // [ 0.05 * 100 mg/l = 0.5e-6 g/cm^3 ]
    frame.set ("C", 0.55e-6); 
  }
} InitialNH4_syntax;

static struct ChemicalNH4Syntax : public DeclareParam
{ 
  ChemicalNH4Syntax ()
    : DeclareParam (Chemical::component, "NH4", "N", "\
Ammonium-N.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("adsorption", "NH4");
    frame.set ("diffusion_coefficient", 1.8e-5);
    frame.set ("initial", "initial_NH4");
  }
} ChemicalNH4_syntax;

// chemical_std.C ends here.
