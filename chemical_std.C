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
#include "vegetation.h"
#include "bioclimate.h"
#include "litter.h"
#include "smb.h"
#include <sstream>

struct ChemicalBase : public Chemical
{
  const Units& units;
  
  // Units.
  static const symbol g_per_cm3;

  // Parameters.
  const double molar_mass_;
  const double solubility;
  const double solubility_infiltration_factor;
  const double crop_uptake_reflection_factor;
  const double canopy_dissipation_rate;
  const double canopy_washoff_coefficient;
  const double surface_decompose_rate;
  const double litter_decompose_rate;
  const double litter_washoff_coefficient;
  const double litter_diffusion_rate;
  const double diffusion_coefficient_; 
  const double decompose_rate;
  const PLF decompose_conc_factor;
  const PLF decompose_lag_increment;
  const bool enable_surface_products;
  const bool soil_affects_surface_decompose;
  struct Product
  {
    const double fraction;
    const symbol chemical;
    static void load_syntax (Frame&);
    Product (const Block&);
  };
  const auto_vector<const Product*> product;

  const bool drain_secondary;
  const std::unique_ptr<Number> C_below_expr;
  double C_below_value;
  const std::unique_ptr<Number> initial_expr;
  const std::unique_ptr<Adsorption> adsorption_;

  // Management.
  double deposit_;
  double spray_overhead_;
  double spray_surface_;
  double dissipate_surface_;
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
  double canopy_transform;

  double litter_storage;
  double litter_in;
  double litter_decompose;
  double litter_transform;
  double litter_out;
  double litter_leak;
  double litter_diffuse;
  
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
  std::vector<double> M_secondary_; // Content in secondary domain [g/cm^3]
  std::vector<double> M_primary_; // Content in primary domain [g/cm^3]
  std::vector<double> M_total_; // Concentration in soil [g/cm^3]
  std::vector<double> M_error; // Accumulated error [g/cm^3]
  std::vector<double> M_tertiary_; // Content in tertiary domain [g/cm^3]
  std::vector<double> S_secondary_;  // Secondary domain source term.
  std::vector<double> S_primary_;// Primary domain source term.
  std::vector<double> S_exchange;       // Exchange from primary to secondary.
  // Added to the soil matrix from drains indirectly via biopores.
  // This can be non-zero whereever there are drain connected biopores.
  std::vector<double> S_indirect_drain;
  // Added to the soil matrix from drains directly, not via biopores.
  // This is only non-zero in drain nodes.
  std::vector<double> S_soil_drain;
  // Removed from the biopores to the drain. 
  // This is only non-zero in drain nodes.
  std::vector<double> S_p_drain;
  // Biopores to matrix. 
  std::vector<double> S_B2M;
  // Matrix to biopores (negative matrix source).
  // Does not count flow to drain connected biopores.
  std::vector<double> S_M2B;
  std::vector<double> S_external; // External source term, e.g. incorp. fert.
  std::vector<double> S_permanent; // Permanent external source term.
  std::vector<double> S_root;   // Root uptake source term (negative).
  std::vector<double> S_decompose;      // Decompose source term.
  std::vector<double> S_decompose_primary;      // Decompose, prim. dom.
  std::vector<double> S_decompose_secondary;      // Decompose, sec. dom.
  std::vector<double> S_transform;      // Transform source term.
  std::vector<double> decompose_factor;      // Soil factor on decompose term.
  double surface_decompose_factor;	     // ... ditto, near surface.
  std::vector<double> J_primary; // Solute transport in primary matrix water.
  std::vector<double> J_secondary; // Solute transport in secondary matrix.
  std::vector<double> J_matrix;    // Solute transport log in matrix water.
  std::vector<double> J_tertiary; // Solute transport log in tertiary water.
  std::vector<double> tillage;         // Changes during tillage.
  std::vector<double> lag;
  double sink_dt;                            // Suggested timestep [h]
  int sink_cell;                             // Relevant cell.

  void sorption_table (const Soil& soil, const size_t cell, const double Theta, 
                       const double start, const double factor,
                       const int intervals,
                       Treelog& msg) const;

  // Solute.
  const Adsorption& adsorption () const;
  double diffusion_coefficient () const;

  double molar_mass () const
  { return molar_mass_; }

  // Surface content.
  double surface_release_fraction () const; // []
  double surface_immobile_amount () const;  // [g/cm^2]
  double surface_storage_amount () const;  // [g/cm^2]
  double litter_storage_amount () const;  // [g/cm^2]
  double canopy_storage_amount () const;  // [g/cm^2]

  // Soil content.
  double C_below () const; // Concentration in groundwater [g/cm^3]
  double C_secondary (size_t) const;
  double C_primary (size_t) const;
  double C_average (size_t) const;
  double C_to_drain (size_t) const;
  double M_primary (size_t) const;
  double M_secondary (size_t) const;
  double M_total (size_t) const;
  double total_surface (const Geometry&, 
                        double from, double to) const; // [g/cm^2]
  double S_secondary (size_t) const;
  double S_primary (size_t) const;
  
  // Transport.
  void set_macro_flux (size_t e, double value);
  void set_primary (const Soil& soil, const SoilWater& soil_water,
                    const std::vector<double>& M,
                    const std::vector<double>& J);
  void set_secondary (const Soil& soil, const SoilWater& soil_water,
                      const std::vector<double>& M,
                      const std::vector<double>& J);
  void update_matrix (const Soil& soil, const SoilWater& soil_water);
  void set_tertiary (const std::vector<double>& S_p, 
		     const std::vector<double>& J_p);
  void add_tertiary (const std::vector<double>& pM,
		     const std::vector<double>& pJ,
		     const std::vector<double>& pS_B2M,
		     const std::vector<double>& pS_indirect_drain,
		     const std::vector<double>& pS_p_drain);

  // Sink.
  void clear ();
  void add_to_source_secondary (const std::vector<double>&);
  void add_to_source_primary (const std::vector<double>&);
  void add_to_sink_secondary (const std::vector<double>&);
  void add_to_sink_primary (const std::vector<double>&);
  void add_to_root_sink (const std::vector<double>&);
  void add_to_decompose_sink (const std::vector<double>&);
  void add_to_decompose_sink_secondary (const std::vector<double>&);
  void add_to_transform_source (const std::vector<double>&);
  void add_to_transform_sink (const std::vector<double>&);
  void add_to_transform_source_secondary (const std::vector<double>&);
  void add_to_transform_sink_secondary (const std::vector<double>&);
  void add_to_surface_transform_source (double amount  /* [g/cm^2/h] */);
  void add_to_litter_transform_source (double amount  /* [g/cm^2/h] */);
  void add_to_canopy_transform_source (double amount  /* [g/cm^2/h] */);
  void release_surface_colloids (double surface_release);

  // Management.
  void remove_all ();
  double total_content (const Geometry&) const; // [g/m^2]
  void update_C (const Soil&, const SoilWater&);
  void deposit (const double flux); // [g/m^2/h]
  void spray_overhead (const double amount); // [g/m^2]
  void spray_surface (const double amount); // [g/m^2]
  void dissipate_surface (const double amount); // [g/m^2]
  void harvest (const double removed, const double surface);
  void incorporate (const Geometry&, double amount, double from, double to);
  void incorporate (const Geometry&, double amount, const Volume&);
  void mix (const Geometry& geo, const Soil&, const SoilWater&,
            double from, double to, double penetration);
  void swap (const Geometry& geo, const Soil&, const SoilWater&,
             double from, double middle, double to);
  
  // Simulation.
  void tick_source (const Scope&, 
                    const Geometry&, const Soil&, const SoilWater&, 
                    const SoilHeat&, const OrganicMatter&, const Chemistry&, 
                    Treelog&);
  double suggest_dt () const;
  static void divide_loss (const double absolute_loss_rate, 
                           const double first_rate, const double second_rate,
                           double& first, double& second);
  void tick_top (const Vegetation&,
		 const Bioclimate&,
		 const Litter&, 
		 Chemistry&,
                 const double surface_runoff_rate, // [h^-1]
                 const double dt, // [h]
                 Treelog& msg);
  void tick_surface (const double pond,
                     const Geometry& geo, 
                     const Soil& soil, const SoilWater& soil_water, 
                     const double z_mixing, Treelog&);
  void tick_soil (const Geometry&, const Soil&, const SoilWater&, double dt,
                  const Scope&, Treelog&);
  void tick_after (const Geometry&, Treelog&);
  void mixture (const Geometry& geo,
                const double pond /* [mm] */, 
                const double rate /* [h/mm] */,
                const double dt /* [h]*/);
  void infiltrate (const double rate, const double water, const double dt);
  double down ();                 // [g/m^2/h]
  void uptake (const Soil&, const SoilWater&, double dt);
  void decompose (const Geometry& geo,
                  const Soil&, const SoilWater&, const SoilHeat&, 
                  const OrganicMatter&, Chemistry&, double dt, Treelog&);
  void output (Log&) const;
  void debug_cell (std::ostream&, const size_t c) const;

  // Create.
  bool check (const Scope&, 
              const Geometry&, const Soil&, const SoilWater&,
	      const OrganicMatter&, const Chemistry&, Treelog&) const;
  static void fillup (std::vector<double>& v, const size_t size);
  void initialize (const Scope&, const Geometry&,
                   const Soil&, const SoilWater&, const SoilHeat&, Treelog&);
protected:
  ChemicalBase (const BlockModel&);
};

const symbol 
ChemicalBase::g_per_cm3 ("g/cm^3");

void
ChemicalBase::Product::load_syntax (Frame& frame)
{
  frame.declare ("fraction", Attribute::Fraction (), Attribute::Const,
                 "Fraction of decomposed matter that become this chemcial.\n\
\n\
If both chemicals have molar_mass specified, the fraction will be mole\n\
based, otherwise it will be mass based.");
  frame.declare_string ("chemical", Attribute::Const, 
			"Chemical product of decomposed matter.");
  frame.order ("fraction", "chemical");
}

ChemicalBase::Product::Product (const Block& al)
  : fraction (al.number ("fraction")),
    chemical (al.name ("chemical"))
{ }

void 
ChemicalBase::sorption_table (const Soil& soil, const size_t cell, 
			      const double Theta, 
			      const double start, const double factor,
			      const int intervals,
			      Treelog& msg) const
{
  std::ostringstream tmp;
  tmp << "Sorption table for " << objid << " cell " << cell << "\n"
      << "Theta = " << Theta << " []"
      << "; clay = " << soil.clay (cell) << " []"
      << "; OC = " << soil.humus_C (cell) << " []"
      << "; rho_b = " << soil.dry_bulk_density (cell) << " [g/cm^3]\n"
      << "----\n"
      << "C\tM\n"
      << "g/cm^3\tg/cm^3";
  if (cell < soil.size ())
    {
      double C = start;
      for (int i = 0; i < intervals; i++)
        {
          const double M = adsorption_->C_to_M_total (soil, Theta, cell, C);
          tmp << "\n" << C << "\t" << M;
          if (std::isnormal (factor))
            C *= factor;
          else 
            C += start;
        }
      msg.message (tmp.str ());
    }
  else
    {
      tmp << "\nNo such cell";
      msg.error (tmp.str ());
    }
}

const Adsorption&
ChemicalBase::adsorption () const
{ return *adsorption_; }

double
ChemicalBase::diffusion_coefficient () const
{ return diffusion_coefficient_; }

double
ChemicalBase::surface_release_fraction () const
{ return surface_release; }

double 
ChemicalBase::surface_immobile_amount () const
{
  const double m2_per_cm2 = 0.01 * 0.01;
  return surface_immobile * m2_per_cm2; 
}

double 
ChemicalBase::surface_storage_amount () const
{
  const double m2_per_cm2 = 0.01 * 0.01;
  return surface_storage * m2_per_cm2; 
}

double 
ChemicalBase::litter_storage_amount () const
{
  const double m2_per_cm2 = 0.01 * 0.01;
  return litter_storage * m2_per_cm2; 
}

double 
ChemicalBase::canopy_storage_amount () const
{
  const double m2_per_cm2 = 0.01 * 0.01;
  return canopy_storage * m2_per_cm2; 
}

double 
ChemicalBase::C_below () const
{ return C_below_value; }

double 
ChemicalBase::C_secondary (const size_t i) const
{ return C_secondary_[i]; }

double 
ChemicalBase::C_primary (const size_t i) const
{ return C_primary_[i]; }

double 
ChemicalBase::C_average (const size_t i) const
{ return C_avg_[i]; }

double 
ChemicalBase::C_to_drain (const size_t i) const
{ return drain_secondary
    ? C_secondary (i) 
    : C_average (i); }

double 
ChemicalBase::M_secondary (const size_t i) const
{ return M_secondary_[i]; }

double 
ChemicalBase::M_primary (const size_t i) const
{ return M_primary_[i]; }

double 
ChemicalBase::M_total (const size_t i) const
{ return M_total_[i]; }

double
ChemicalBase::total_surface (const Geometry& geo, 
			     const double from, const double to) const
{ return geo.total_surface (M_total_, from, to); }

double 
ChemicalBase::S_secondary (size_t i) const
{ return S_secondary_[i]; }

double 
ChemicalBase::S_primary (size_t i) const
{ return S_primary_[i]; }

void 
ChemicalBase::set_primary (const Soil& soil, const SoilWater& soil_water,
			   const std::vector<double>& M,
			   const std::vector<double>& J)
{
  const size_t cell_size = M.size ();
  daisy_assert (M_primary_.size () == cell_size);
  daisy_assert (M_error.size () == cell_size);

  M_primary_ = M;
  J_primary = J;

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
      C_primary_[c] 
        = adsorption_->M_to_C1 (soil, Theta_primary, c, M_primary_[c]);
    }

  update_matrix (soil, soil_water);
}

void 
ChemicalBase::set_secondary (const Soil& soil, const SoilWater& soil_water,
			     const std::vector<double>& M,
			     const std::vector<double>& J)
{
  M_secondary_ = M;
  J_secondary = J;
  update_matrix (soil, soil_water);
}

void 
ChemicalBase::update_matrix (const Soil& soil, const SoilWater& soil_water)
{
  // Update cells.
  const size_t cell_size = M_total_.size ();
  daisy_assert (C_avg_.size () == cell_size);
  daisy_assert (C_primary_.size () == cell_size);
  daisy_assert (M_primary_.size () == cell_size);
  daisy_assert (C_secondary_.size () == cell_size);
  daisy_assert (M_secondary_.size () == cell_size);

  for (size_t c = 0; c < cell_size; c++)
    {
      daisy_assert (M_primary_[c] >= 0.0);
      daisy_assert (M_secondary_[c] >= 0.0);
      M_total_[c] = M_primary_[c] + M_secondary_[c];
      daisy_assert (M_total_[c] >= 0.0);
      
      const double Theta_primary = soil_water.Theta_primary (c);
      const double Theta_secondary = soil_water.Theta_secondary (c);
      const double Theta_matrix = Theta_primary + Theta_secondary;
      
      if (Theta_secondary > 0.0)
        C_secondary_[c]
          = adsorption_->M_to_C2 (soil, Theta_secondary, c, M_secondary_[c]);
      else
	{
	  if (M_secondary_[c] > 1e-90)
	    {
	      std::ostringstream tmp;
	      tmp << "T2[" << c << "] = " << Theta_secondary
		  << "; M2 = " << M_secondary_[c];
	      daisy_warning (tmp.str ());
	    }
	    
	    
	  C_secondary_[c] = C_primary_[c];
	}

      C_avg_[c] 
        = (Theta_primary * C_primary_[c] + Theta_secondary * C_secondary_[c]) 
        / Theta_matrix;
    }

  const size_t edge_size = J_matrix.size ();
  daisy_assert (J_primary.size () == edge_size);
  daisy_assert (J_secondary.size () == edge_size);
  for (size_t e = 0; e < edge_size; e++)
    J_matrix[e] = J_primary[e] + J_secondary[e];
}

void 
ChemicalBase::set_tertiary (const std::vector<double>& S_p, 
			    const std::vector<double>& J_p)
{
  const size_t cell_size = S_p.size ();
  daisy_assert (S_B2M.size () == cell_size);
  daisy_assert (S_M2B.size () == cell_size);
  for (size_t i = 0; i < cell_size; i++)
    {
      if (S_p[i] > 0)
	{
	  S_B2M[i] = S_p[i];
	  S_M2B[i] = 0.0;
	}
      else
	{
	  S_B2M[i] = 0.0;
	  S_M2B[i] = -S_p[i];
	}	
    }
  add_to_source_secondary (S_p);
  daisy_assert (J_tertiary.size () == J_p.size ());
  J_tertiary = J_p;
}

void 
ChemicalBase::add_tertiary (const std::vector<double>& pM,
			    const std::vector<double>& pJ,
			    const std::vector<double>& pS_M2B,
			    const std::vector<double>& pS_indirect_drain,
			    const std::vector<double>& pS_p_drain)
{
  const size_t cell_size = M_tertiary_.size ();
  const size_t edge_size = J_tertiary.size ();
  daisy_assert (pM.size () == cell_size);
  daisy_assert (pJ.size () == edge_size);
  daisy_assert (pS_M2B.size () == cell_size);
  daisy_assert (pS_indirect_drain.size () == cell_size);
  daisy_assert (S_p_drain.size () == cell_size);
  daisy_assert (M_tertiary_.size () == cell_size);
  daisy_assert (J_tertiary.size () == edge_size);
  daisy_assert (S_B2M.size () == cell_size);
  daisy_assert (S_indirect_drain.size () == cell_size);
  daisy_assert (S_p_drain.size () == cell_size);
  add_to_sink_secondary (pS_M2B);
  add_to_sink_secondary (pS_indirect_drain);
  for (size_t i = 0; i < cell_size; i++)
    {
      M_tertiary_[i] += pM[i];
      const double M2B = pS_M2B[i];
      if (M2B < 0.0)
	S_B2M[i] -= M2B;
      else
	S_M2B[i] += M2B;
      S_indirect_drain[i] += pS_indirect_drain[i];
      S_p_drain[i] += pS_p_drain[i];
    }

  for (size_t i = 0; i < edge_size; i++)
    J_tertiary[i] += pJ[i];
}

void
ChemicalBase::clear ()
{
  deposit_ = 0.0;
  spray_overhead_ = 0.0;
  spray_surface_ = 0.0;
  dissipate_surface_ = 0.0;
  harvest_ = 0.0;
  residuals = 0.0;
  surface_tillage = 0.0;
  litter_tillage = 0.0;
  canopy_transform = 0.0;
  litter_transform = 0.0;
  surface_transform = 0.0;
  surface_release = 0.0;
  // Don't clear M_tertiary here, it may be needed for initial log content.
  std::fill (S_secondary_.begin (), S_secondary_.end (), 0.0);
  std::fill (S_primary_.begin (), S_primary_.end (), 0.0);
  std::fill (S_external.begin (), S_external.end (), 0.0);
  std::fill (S_root.begin (), S_root.end (), 0.0);
  std::fill (S_decompose.begin (), S_decompose.end (), 0.0);
  std::fill (S_decompose_primary.begin (), S_decompose_primary.end (), 0.0);
  std::fill (S_decompose_secondary.begin (), S_decompose_secondary.end (), 0.0);
  std::fill (S_transform.begin (), S_transform.end (), 0.0);
  std::fill (decompose_factor.begin (), decompose_factor.end (), 1.0);
  std::fill (J_primary.begin (), J_primary.end (), 0.0);
  std::fill (J_secondary.begin (), J_secondary.end (), 0.0);
  std::fill (J_matrix.begin (), J_matrix.end (), 0.0);
  std::fill (J_tertiary.begin (), J_tertiary.end (), 0.0);
  std::fill (tillage.begin (), tillage.end (), 0.0);
  
}

void
ChemicalBase::add_to_source_secondary (const std::vector<double>& v)
{
  daisy_assert (S_secondary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_secondary_[i] += v[i];
      daisy_assert (std::isfinite (S_secondary_[i]));
    }
}


void
ChemicalBase::add_to_source_primary (const std::vector<double>& v)
{
  daisy_assert (S_primary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_primary_[i] += v[i];
      daisy_assert (std::isfinite (S_primary_[i]));
    }
}

void
ChemicalBase::add_to_sink_secondary (const std::vector<double>& v)
{
  daisy_assert (S_secondary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_secondary_[i] -= v[i];
      daisy_assert (std::isfinite (S_secondary_[i]));
    }
}

void
ChemicalBase::add_to_sink_primary (const std::vector<double>& v)
{
  daisy_assert (S_primary_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_primary_[i] -= v[i];
      daisy_assert (std::isfinite (S_primary_[i]));
    }
}

void
ChemicalBase::add_to_root_sink (const std::vector<double>& v)
{
  daisy_assert (S_root.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_root[i] -= v[i];
  add_to_sink_secondary (v);
}

void
ChemicalBase::add_to_decompose_sink (const std::vector<double>& v)
{
  daisy_assert (S_decompose.size () >= v.size ());
  daisy_assert (S_decompose_primary.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      daisy_assert (v[i] >= 0.0);
      S_decompose[i] -= v[i];
      S_decompose_primary[i] -= v[i];
    }
  add_to_sink_primary (v);
}

void
ChemicalBase::add_to_decompose_sink_secondary (const std::vector<double>& v)
{
  daisy_assert (S_decompose.size () >= v.size ());
  daisy_assert (S_decompose_secondary.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      daisy_assert (v[i] >= 0.0);
      S_decompose[i] -= v[i];
      S_decompose_secondary[i] -= v[i];
    }
  add_to_sink_secondary (v);
}

void
ChemicalBase::add_to_transform_sink (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] -= v[i];
  add_to_sink_primary (v);
}

void
ChemicalBase::add_to_transform_source (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] += v[i];
  add_to_source_primary (v);
}

void
ChemicalBase::add_to_transform_sink_secondary (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] -= v[i];
  add_to_sink_secondary (v);
}

void
ChemicalBase::add_to_transform_source_secondary (const std::vector<double>& v)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] += v[i];
  add_to_source_secondary (v);
}

void 
ChemicalBase::add_to_litter_transform_source (const double amount /* [g/cm^2/h] */)
{
  const double m2_per_cm2 = 0.01 * 0.01;
  litter_transform += amount / m2_per_cm2;
}

void 
ChemicalBase::add_to_surface_transform_source (const double amount /* [g/cm^2/h] */)
{
  const double m2_per_cm2 = 0.01 * 0.01;
  surface_transform += amount / m2_per_cm2;
}

void 
ChemicalBase::add_to_canopy_transform_source (const double amount /* [g/cm^2/h] */)
{
  const double m2_per_cm2 = 0.01 * 0.01;
  canopy_transform += amount / m2_per_cm2;
}

void
ChemicalBase::release_surface_colloids (const double surface_release_value)
{
  if (std::isnormal (surface_release))
    throw "Multiple reactions setting surface release";
  surface_release = surface_release_value;
}

void
ChemicalBase::remove_all ()	
{
  snow_storage = 0.0;
  canopy_storage = 0.0;
  litter_storage = 0.0;
  surface_storage = 0.0;
  std::fill (C_avg_.begin (), C_avg_.end (), 0.0);
  std::fill (C_secondary_.begin (), C_secondary_.end (), 0.0);
  std::fill (M_secondary_.begin (), M_secondary_.end (), 0.0);
  std::fill (M_total_.begin (), M_total_.end (), 0.0);
  std::fill (M_primary_.begin (), M_primary_.end (), 0.0);
  std::fill (C_primary_.begin (), C_primary_.end (), 0.0);
}

double				// [g/m^2]
ChemicalBase::total_content (const Geometry& geo) const
{ return snow_storage + canopy_storage + litter_storage + surface_storage
    + geo.total_surface (M_total_) * 10000.0 /* [cm^2/m^2] */; } 

void
ChemicalBase::update_C (const Soil& soil, const SoilWater& soil_water)
{
  if (adsorption_->full ())
    // Always zero.
    return;

  const size_t cell_size = M_total_.size ();
  daisy_assert (C_primary_.size () == cell_size);
  daisy_assert (C_secondary_.size () == cell_size);
  daisy_assert (C_avg_.size () == cell_size);
  daisy_assert (M_primary_.size () == cell_size);
  daisy_assert (M_secondary_.size () == cell_size);
  
  for (size_t c = 0; c < cell_size; c++)
    {
      const double T1 = soil_water.Theta_primary (c);
      const double T2 = soil_water.Theta_secondary (c);
      const double M = M_total_[c];
      const double M2 = (T2 > 0.0)
	// Water in secondary domain.
	? std::min (M_secondary_[c], M)
	// No water in secondary domain.
	: 0.0;
      const double M1 = M - M2;
      daisy_assert (T1 > 0.0);
      daisy_assert (M1 >= 0.0);
      const double C1 = adsorption_->M_to_C1 (soil, T1, c, M1);
      const double C2 = (T2 > 0.0)
	? adsorption_->M_to_C2 (soil, T2, c, M2)
	: C1;
      const double C = (C1 * T1 + C2 * T2) / (T1 + T2);

      // Update.
      C_primary_[c] = C1;
      C_secondary_[c] = C2;
      C_avg_[c] = C;
      M_primary_[c] = M1;
      M_secondary_[c] = M2;
    }
}
  
void 
ChemicalBase::deposit (const double flux) // [g/m^2/h]
{ deposit_ += flux; }

void 
ChemicalBase::spray_overhead (const double amount) // [g/m^2]
{ spray_overhead_ += amount; }

void 
ChemicalBase::spray_surface (const double amount) // [g/m^2]
{ spray_surface_ += amount; }

void 
ChemicalBase::dissipate_surface (const double amount) // [g/m^2]
{ dissipate_surface_ += amount; }

void 
ChemicalBase::harvest (const double removed, const double surface)
{ 
  const double new_storage 
    = canopy_storage + harvest_ - residuals;
  const double gone = new_storage * removed;
  harvest_ += gone * (1.0 - surface); 
  residuals += gone * surface;
}

void 
ChemicalBase::incorporate (const Geometry& geo, const double amount,
			   const double from, const double to)
{ 
  daisy_assert (amount >= 0.0);
  daisy_assert (from <= 0.0);
  daisy_assert (to <= from);
  const double m2_per_cm2 = 0.01 * 0.01;
  geo.add_surface (S_external, from, to, m2_per_cm2 * amount);
}

void 
ChemicalBase::incorporate (const Geometry& geo, const double amount,
			   const Volume& volume)
{ 
  daisy_assert (amount >= 0.0);
  const double m2_per_cm2 = 0.01 * 0.01;
  geo.add_surface (S_external, volume, m2_per_cm2 * amount);
}

void 
ChemicalBase::mix (const Geometry& geo,
		   const Soil& soil, const SoilWater& soil_water, 
		   const double from, const double to,
		   const double penetration)
{ 
  // Removed from surface.
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
  daisy_assert (penetration <= 1.0);
  daisy_assert (penetration >= 0.0);
  const double surface_removed = surface_storage * penetration;
  surface_tillage += surface_removed;
  surface_storage -= surface_removed;
  daisy_assert (surface_storage >= 0.0);
  surface_solute *= (1.0 - penetration);
  surface_immobile *= (1.0 - penetration);
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
  const double litter_removed = litter_storage * penetration;
  litter_tillage += litter_removed;
  litter_storage -= litter_removed;
  const double removed = surface_removed + litter_removed;

  // Add to soil.
  const double m2_per_cm2 = 0.01 * 0.01;
  const double penetrated = removed * m2_per_cm2;
  geo.add_surface (M_total_, from, to, penetrated);
  geo.add_surface (tillage, from, to, penetrated);

  // Mix.
  geo.mix (M_total_, from, to, tillage);
  update_C (soil, soil_water);
}

void 
ChemicalBase::swap (const Geometry& geo,
		    const Soil& soil, const SoilWater& soil_water,
		    const double from, const double middle, const double to)
{ 
  geo.swap (M_total_, from, middle, to, tillage);
  update_C (soil, soil_water);
}

void 
ChemicalBase::tick_source (const Scope& scope, const Geometry& geo,
			   const Soil& soil, const SoilWater& soil_water, 
			   const SoilHeat& soil_heat, 
			   const OrganicMatter& organic, 
			   const Chemistry& chemistry, Treelog& msg)
{ 
  const size_t cell_size = geo.cell_size ();

  sink_dt = 0.0;
  sink_cell = Geometry::cell_error;
  for (size_t c = 0; c < cell_size; c++)
    {
      const double Theta = soil_water.Theta (c);
      const double Theta_secondary = soil_water.Theta_secondary (c);
      const bool has_secondary =  Theta_secondary > 1e-9 * Theta;
      const double S = soil_water.S_forward_sink (c);
      const double C = this->C_secondary (c);
      const double C_avg = this->C_average (c);
      const double M_total = this->M_total (c);
      const double M_solute = C_avg * Theta;
      const double M_secondary = has_secondary 
        ? this->M_secondary (c)
        : 0.0;
      
      const double dt 
        = chemistry.find_dt (S, C, M_secondary, M_solute, M_total);
      if (std::isnormal (dt)
          && (!std::isnormal (sink_dt) || std::fabs (sink_dt) > std::fabs (dt)))
        {
          daisy_assert (dt > 0);
          sink_dt = dt;
          sink_cell = c;
        }
    }
}

double 
ChemicalBase::suggest_dt () const
{ return sink_dt; }

void
ChemicalBase::divide_loss (const double absolute_loss_rate, 
			   const double first_rate, 
			   const double second_rate,
			   double& first, double& second)
{
  const double relative_loss_rate = first_rate + second_rate;
  if (std::isnormal (relative_loss_rate))
    {
      const double X // Representative storage.
        = absolute_loss_rate / relative_loss_rate;
      first = X * first_rate;
      second = X * second_rate;
      daisy_approximate (absolute_loss_rate, first + second);
    }
  else
    {
      first = 0.0;
      second = 0.0;
      daisy_assert (iszero (absolute_loss_rate));
    }
}

void 
ChemicalBase::tick_top (const Vegetation& vegetation,
			const Bioclimate& bioclimate,
			const Litter& litter,
			Chemistry& chemistry,
			const double surface_runoff_rate, // [h^-1]
			const double dt, // [h]
			Treelog& msg)
{
  TREELOG_MODEL (msg);
  const double m2_per_cm2 = 0.01 * 0.01 ; // [m^2/cm^2]

  // Import from vegatation and bioclimate.
  const double snow_water_storage = bioclimate.get_snow_storage (); // [mm]
  const double snow_leak_rate = bioclimate.snow_leak_rate (dt); // [h^-1]
  const double canopy_cover = vegetation.cover (); // [];
  const double canopy_leak_rate = bioclimate.canopy_leak_rate (dt); // [h^-1]
  const double litter_cover = litter.cover (); // []
  const double litter_leak_rate = bioclimate.litter_leak_rate (dt); // [h^-1]
  const double litter_surface_wash_off_rate
    = bioclimate.litter_wash_off_rate (dt); // [h^-1]

  // Fluxify management operations.
  spray_overhead_ /= dt;
  spray_surface_ /= dt;
  dissipate_surface_ /= dt;
  harvest_ /= dt;
  residuals /= dt;
  surface_tillage /= dt;
  litter_tillage /= dt;



  const double old_storage = snow_storage + canopy_storage + litter_storage;

  // Snow pack
  snow_in = spray_overhead_ + deposit_;
  const double old_snow_storage = snow_storage;
  if (snow_water_storage > 1e-5)
    first_order_change (old_snow_storage, snow_in, snow_leak_rate, dt,
			snow_storage, snow_out);
  else
    {
      snow_storage = 0.0;
      snow_out = snow_in + old_snow_storage / dt;
    }

  // Canopy.
  canopy_in = snow_out * canopy_cover;
  const double canopy_bypass = snow_out - canopy_in;
  canopy_harvest = harvest_;

  const double old_canopy_storage = canopy_storage;
  const double canopy_absolute_input_rate 
    = canopy_in - canopy_harvest - residuals + canopy_transform;

  double canopy_washoff;
  if (canopy_leak_rate < 0.0)
    {
      // Special case: No water left on canopy, yet there is overflow.
      // Wash it all off.
      if (canopy_storage > 1e-1)
	{
	  TREELOG_MODEL (msg);
	  msg.message ("Evacuating canopy");
	}
      canopy_washoff = old_canopy_storage / dt + canopy_absolute_input_rate;
      canopy_dissipate = 0.0;
      canopy_storage = 0.0;
    }
  else
    {
      const double canopy_washoff_rate 
	= canopy_washoff_coefficient * canopy_leak_rate;
      double canopy_absolute_loss_rate;
      first_order_change (old_canopy_storage, 
			  canopy_absolute_input_rate, 
			  canopy_dissipation_rate + canopy_washoff_rate, dt,
			  canopy_storage, canopy_absolute_loss_rate);
      divide_loss (canopy_absolute_loss_rate, 
		   canopy_washoff_rate, canopy_dissipation_rate,
		   canopy_washoff, canopy_dissipate);
    }
  canopy_out = canopy_washoff + residuals;

  // Litter
  const double below_canopy = canopy_out + canopy_bypass + spray_surface_;
  litter_in = (below_canopy - residuals) * litter_cover + residuals;
  const double litter_bypass = below_canopy - litter_in;

  const double old_litter_storage = litter_storage;

  // Water stored in the litter leaking.
  const double litter_washoff_rate 
    = litter_washoff_coefficient * litter_leak_rate;

  // Diffusion from litter to water dripping past it on the surface.
  const double litter_diffuse_rate
    = litter.diffuse ()
    ? std::min (litter_diffusion_rate, litter_surface_wash_off_rate)
    : 0.0;

  // Decompose rate adjusted by litter conditions.
  const double litter_decompose_rate_adjusted
    = litter_decompose_rate * litter.decompose_factor ();
  
  double litter_absolute_loss_rate;
  first_order_change (old_litter_storage, litter_in + litter_transform,
		      litter_decompose_rate_adjusted
		      + litter_washoff_rate
		      + litter_diffuse_rate, dt,
                      litter_storage, litter_absolute_loss_rate);
  divide_loss (litter_absolute_loss_rate, 
               litter_decompose_rate_adjusted,
	       litter_washoff_rate + litter_diffuse_rate,
               litter_decompose, litter_out);
  divide_loss (litter_out, litter_washoff_rate, litter_diffuse_rate,
	       litter_leak, litter_diffuse);
 
  // Surface
  surface_in = litter_out + litter_bypass;
  const double old_surface_storage = surface_storage;
  double surface_absolute_loss_rate;
  const double decompose_used = (soil_affects_surface_decompose)
    ? ( surface_decompose_rate * surface_decompose_factor)
    : surface_decompose_rate;
    
  first_order_change (old_surface_storage, surface_in + surface_transform, 
                      surface_runoff_rate + decompose_used, dt,
                      surface_storage, surface_absolute_loss_rate);
  if (surface_storage < 0.0)
    {
      std::ostringstream tmp;
      tmp << "old_surface_storage = " << old_surface_storage 
          << ", surface_in = " << surface_in 
          << ", surface_transform = " << surface_transform
          << ", surface_runoff_rate = " << surface_runoff_rate 
          << ", surface_decompose_rate = " << surface_decompose_rate
          << ", surface_decompose_factor = " << surface_decompose_factor
	  << ", decompose_used = " << decompose_used
          << ", dt = " << dt
          << ", surface_storage = " << surface_storage 
          << ", surface_absolute_loss_rate = " << surface_absolute_loss_rate;
      msg.debug (tmp.str ());
      surface_decompose = surface_storage / dt;
      surface_runoff = 0.0;
      surface_storage = 0.0;
    }
  divide_loss (surface_absolute_loss_rate,
               decompose_used, surface_runoff_rate,
               surface_decompose, surface_runoff);

  // Metabolites
  if (enable_surface_products)
    for (size_t i = 0; i < product.size (); i++)
      {
	const symbol name = product[i]->chemical;
	if (chemistry.know (name))
	  {
	    Chemical& chemical = chemistry.find (name); 
	    const double fraction = product[i]->fraction; // []
	    const double factor = fraction *
	      ((molar_mass () > 0.0 && chemical.molar_mass () > 0.0)
	       ? chemical.molar_mass () / molar_mass ()
	       : 1.0);		// []

	    const double created	// [g/cm^2/h] 
	      = surface_decompose /* [g/m^2/h] */
	      * factor /* [] */
	      * m2_per_cm2; /* [m^2/cm^2] */;
	    chemical.add_to_surface_transform_source (created);
	  }
      }
  
  // Mass balance.
  const double new_storage = snow_storage + canopy_storage + litter_storage;
  const double input
    = spray_overhead_ + spray_surface_ + deposit_
    + canopy_transform + litter_transform;
  const double output = surface_in + canopy_harvest + canopy_dissipate 
    + litter_decompose;
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

  // Surface volatilization also bypass system.
  spray_surface_ += dissipate_surface_;
  surface_in += dissipate_surface_;
  surface_decompose += dissipate_surface_;

}

void
ChemicalBase::tick_surface (const double pond /* [cm] */,
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
      const double C 
        = adsorption_->M_to_C_total (soil, Theta, cell, M); // [g/cm^3]
      daisy_assert (C >= 0.0);
      // Accumulate based on cell surface area.
      const double area = geo.edge_area (edge); // [cm^2]
      total_area += area;
      surface_solute += C * area * Theta;       // [g/cm]
      daisy_assert (surface_solute >= 0.0);

#if 0
      if (M < 1e-20)
	continue;
      
      std::ostringstream tmp;
      tmp << "C = " << C
	  << "; M = " << M
	  << "; A = " << (M - Theta * C)
	  << "; Theta = " << Theta
	  << "; Theta_pond = " << Theta_pond;
      msg.message (tmp.str ());
#endif
    }
  
  // Convert solute back to surface dimensions.
  daisy_assert (approximate (total_area, geo.surface_area ()));
  const double surface_area = geo.surface_area () * m2_per_cm2; // [m^2]
  daisy_assert (surface_solute >= 0.0);
  surface_solute *= z_mixing;   // [g]
  surface_solute /= surface_area; // [g/m^2]
  daisy_assert (surface_solute >= 0.0);

  // The immobile is the rest.
  surface_immobile = surface_storage - surface_solute;
  if (surface_immobile < 0.0)
    {
      // #ifdef BUG_FREUNDLICH
      daisy_approximate (surface_solute, surface_storage);
      // #endif
      surface_immobile = 0.0;
      surface_solute = surface_storage;
    }

  // Check that full and no adsorption is calculated right.
  static const symbol none ("none");
  if (adsorption_->full ())
    daisy_approximate (surface_storage, surface_immobile);
  else if (adsorption_->objid == none)
    daisy_approximate (surface_storage, surface_solute);
}

void                            // Called just before solute movement.
ChemicalBase::tick_soil (const Geometry& geo,
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
  std::fill (M_tertiary_.begin (), M_tertiary_.end (), 0.0);
  std::fill (S_indirect_drain.begin (), S_indirect_drain.end (), 0.0);
  std::fill (S_soil_drain.begin (), S_soil_drain.end (), 0.0);
  std::fill (S_p_drain.begin (), S_p_drain.end (), 0.0);
  std::fill (S_B2M.begin (), S_B2M.end (), 0.0);
  std::fill (S_M2B.begin (), S_M2B.end (), 0.0);
  std::fill (J_tertiary.begin (), J_tertiary.end (), 0.0);

  // Fluxify management operations.
  daisy_assert (S_external.size () == cell_size);
  daisy_assert (tillage.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      S_external[c] /= dt;
      tillage[c] /= dt;
    }

  // Permanent source.
  for (size_t c = 0; c < cell_size; c++)
    S_external[c] += S_permanent[c];

  // Pumping water.
  for (size_t c = 0; c < cell_size; c++)
    {
      const double S_pump = soil_water.S_incorp (c);
      if (S_pump < 0.0)
        continue;

      // We really should go down in timesteps here instead.
      const double S_min = -0.5 * M_total_[c] / dt;
      S_external[c] += std::max (-S_pump * C_to_drain (c),
                                 S_min);
    }
  
  // Tillage + pumping + external source.
  add_to_source_secondary (S_external); 
 
  // Drainage.
  for (size_t c = 0; c < cell_size; c++)
    {
      // We really should go down in timesteps here instead.
      const double S_min = -0.5 * M_total_[c] / dt;
      S_soil_drain[c] = std::max (-soil_water.S_soil_drain (c) * C_to_drain (c),
				  S_min);
    }
  add_to_source_secondary (S_soil_drain); 

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
      const double M_sec = M_secondary (c);
      if (Theta_sec_new < 1e-6)
        // Move all to primary domain.
        {
          S_exchange[c] = -M_sec / dt;
          continue;
        }
      
      // Find alpha.
      const double alpha = soil.alpha (c);

      // The exchange rate based on solute.
      const double C_prim = C_primary (c);
      const double C_sec = C_secondary (c);

      // Find mass of solutes (sorbed mass ignored) at start of timestep.
      const double Theta_prim_old = soil_water.Theta_primary_old (c);
      const double MS1 = C_prim * Theta_prim_old;
      const double MS2 = C_sec * Theta_sec_old;
      const double MS = MS1 + MS2;

      // Find average concentration at start of timestep.
      const double Theta = Theta_prim_old + Theta_sec_old;
      const double C_avg = MS / Theta;

      // Find necessary change in domain solute towards average concentration.
      const double MS1_goal = C_avg * Theta_prim_old;
      const double MS2_goal = C_avg * Theta_sec_old;
      const double MS1_loss = alpha * (MS1 - MS1_goal);
      const double MS2_gain = alpha * (MS2_goal - MS2);
      if (!approximate (MS + MS1_loss * dt, MS + MS2_gain * dt))
        {
          std::ostringstream tmp;
          tmp << "1: Theta = " << Theta_prim_old << "; C = " << C_prim
              << "; M = " << MS1 << "; Goal = " << MS1_goal
              << " Loss: " << MS1_loss << "\n"
              << "2: Theta = " << Theta_sec_old << "; C = " << C_sec
              << "; M = " << MS2 << "; Goal = " << MS2_goal
              << " Gain: " << MS2_gain << "\n"
              << "X: Theta = " << Theta<< "; C = " << C_avg
              << "; M = " << MS << "\n";
          msg.error (tmp.str ());
        }

      // Use it.
      S_exchange[c] = MS1_loss;      
    }
  add_to_sink_primary (S_exchange); 
  add_to_source_secondary (S_exchange); 
}

void
ChemicalBase::tick_after (const Geometry&, Treelog&)
{ }

void 
ChemicalBase::mixture (const Geometry& geo,
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
ChemicalBase::infiltrate (const double rate /* [h^-1] */, 
			  const double water /* [mm] */,
			  const double dt /* [h] */)
{
  daisy_approximate (surface_storage, surface_solute + surface_immobile);
  daisy_assert (surface_storage >= 0.0);
  daisy_assert (surface_solute >= 0.0);
  daisy_assert (surface_immobile >= 0.0);
  if (surface_storage * 1.001 < surface_solute)
    {
      std::ostringstream tmp;
      tmp << "surface_storage = " << surface_storage 
          << ", surface_solute = " << surface_solute;
      daisy_bug (tmp.str ());
    }
  if (rate * dt > 1.001)
    {
      std::ostringstream tmp;
      tmp << "rate = " << rate << ", dt = " << dt;
      daisy_bug (tmp.str ());
    }    

  // Limit outflow concentration.
  const double max_out /* [g/m^2] */
    = 1000 /* [cm^3/l] */ 
    * solubility /* [g/cm^3] */
    * solubility_infiltration_factor /* [] */ 
    * water /* [mm] = [l/m^2] */;
  daisy_assert (max_out >= 0.0);

  surface_out = std::min (surface_solute * rate, max_out / dt);

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
ChemicalBase::down ()                 // [g/m^2/h]
{ return surface_out + surface_mixture; }

void 
ChemicalBase::uptake (const Soil& soil, 
		      const SoilWater& soil_water,
		      const double dt)
{
  std::vector<double> uptaken (soil.size (), 0.0);

  const double rate = 1.0 - crop_uptake_reflection_factor;
  
  for (unsigned int i = 0; i < soil.size (); i++)
    // We need to use C_average in case secondary domain is emptied.
    uptaken[i] = C_average (i) * soil_water.S_root (i) * rate;
  
  add_to_root_sink (uptaken);
}

void 
ChemicalBase::decompose (const Geometry& geo,
			 const Soil& soil, 
			 const SoilWater& soil_water,
			 const SoilHeat& soil_heat,
			 const OrganicMatter& organic_matter,
			 Chemistry& chemistry, const double dt, Treelog&)
{
  if (!std::isnormal (decompose_rate))
    return;

  const size_t cell_size = geo.cell_size ();

  // Update lag time.
  if (decompose_lag_increment.size () > 0)
    {
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
    }

  std::vector<double> decomposed (cell_size, NAN);
  std::vector<double> decomposed_primary (cell_size, NAN);
  std::vector<double> decomposed_secondary (cell_size, NAN);

  // Basic decompose rate.
  for (size_t c = 0; c < cell_size; c++)
    {
      // Basic rate.
      double rate_primary;
      double rate_secondary;
      double factor = decompose_soil_factor (c, geo,
					     soil, soil_water, soil_heat,
					     organic_matter);
      decompose_factor[c] = factor;
      rate_primary = rate_secondary = decompose_rate * factor;


      // Adjust for concentration.
      if (decompose_conc_factor.size () > 0)
	{
	  rate_primary *= decompose_conc_factor (C_primary (c));
	  rate_secondary *= decompose_conc_factor (C_secondary (c));
	  factor *= decompose_conc_factor (C_average (c));
	}

      // Decomposition.
      decomposed_primary[c] = M_primary (c) * rate_primary;
      decomposed_secondary[c] = M_secondary (c) * rate_secondary;
      decompose_factor[c] = factor;
    }
  surface_decompose_factor = geo.content_hood (decompose_factor,
					       Geometry::cell_above);

  this->add_to_decompose_sink (decomposed_primary);
  this->add_to_decompose_sink_secondary (decomposed_secondary);

  for (size_t i = 0; i < product.size (); i++)
    {
      const symbol name = product[i]->chemical;
      if (chemistry.know (name))
        {
          Chemical& chemical = chemistry.find (name);
          const double fraction = product[i]->fraction;
	  const double factor = fraction *
	    ((molar_mass () > 0.0 && chemical.molar_mass () > 0.0)
	     ? chemical.molar_mass () / molar_mass ()
	     : 1.0);

          std::vector<double> created_primary = decomposed_primary;
          std::vector<double> created_secondary = decomposed_secondary;
          for (size_t c = 0; c < cell_size; c++)
            {
              created_primary[c] *= factor;
              created_secondary[c] *= factor;
            }
          chemical.add_to_transform_source (created_primary);
          chemical.add_to_transform_source_secondary (created_secondary);
        }
    }
}

void
ChemicalBase::output (Log& log) const
{
  // Parameters.
  output_derived (adsorption_, "adsorption", log);

  // Management and climate fluxes.
  output_value (deposit_, "deposit", log);
  output_value (spray_overhead_, "spray_overhead", log);
  output_value (spray_surface_, "spray_surface", log);
  output_value (spray_overhead_ + spray_surface_, "spray", log);
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
  output_variable (canopy_transform, log);
  output_variable (litter_storage, log);
  output_variable (litter_in, log);
  output_variable (litter_decompose, log);
  output_variable (litter_transform, log);
  output_variable (litter_out, log);
  output_variable (litter_leak, log);
  output_variable (litter_diffuse, log);
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
  output_value (canopy_dissipate + canopy_harvest - canopy_transform
		+ litter_decompose - litter_transform
                + surface_runoff + surface_decompose - surface_transform,
                "top_loss", log);
  output_value (C_avg_, "C", log);
  output_value (C_secondary_, "C_secondary", log);
  output_value (C_primary_, "C_primary", log);
  output_value (M_total_, "M", log);
  output_value (M_secondary_, "M_secondary", log);
  output_value (M_primary_, "M_primary", log);
  output_value (M_error, "M_error", log);
  output_value (M_tertiary_, "M_tertiary", log);
  output_value (S_secondary_, "S_secondary", log);
  output_value (S_primary_, "S_primary", log);
  output_variable (S_exchange, log);
  output_variable (S_indirect_drain, log);
  output_variable (S_soil_drain, log);
  output_variable (S_p_drain, log);
  output_variable (S_B2M, log);
  output_variable (S_M2B, log);
  output_variable (S_external, log);
  output_variable (S_permanent, log);
  output_variable (S_root, log);
  output_variable (S_decompose, log);
  output_variable (S_decompose_primary, log);
  output_variable (S_decompose_secondary, log);
  output_variable (S_transform, log);
  output_variable (decompose_factor, log);
  output_variable (surface_decompose_factor, log);
  output_variable (J_primary, log);
  output_variable (J_secondary, log);
  output_variable (J_matrix, log);
  output_variable (J_tertiary, log);
  output_variable (tillage, log);
  output_variable (lag, log);
  if (std::isnormal (sink_dt))
    {
      output_value (sink_dt, "dt", log);
      output_variable (sink_cell, log);
    }
}

void 
ChemicalBase::debug_cell (std::ostream& out, const size_t c) const
{
  out << ", C_avg_ = " << C_avg_[c]
      << ", C_secondary_" << C_secondary_[c]
      << ", C_primary_ = " << C_primary_[c]
      << ", M_secondary_ = " << M_secondary_[c]
      << ", M_primary_ = " << M_primary_[c]
      << ", M_total_ = " << M_total_[c]
      << ", M_error = " << M_error[c]
      << ", M_tertiary = " << M_tertiary_[c]
      << ", S_secondary_ = " << S_secondary_[c]
      << ", S_primary_ = " << S_primary_[c]
      << ", S_exchange = " << S_exchange[c]
      << ", S_indirect_drain = " << S_indirect_drain[c]
      << ", S_soil_drain = " << S_soil_drain[c]
      << ", S_p_drain = " << S_p_drain[c]
      << ", S_B2M = " << S_B2M[c]
      << ", S_M2B = " << S_M2B[c]
      << ", S_external = " << S_external[c]
      << ", S_permanent = " << S_permanent[c]
      << ", S_root = " << S_root[c]
      << ", S_decompose = " << S_decompose[c]
      << ", S_decompose_primary = " << S_decompose_primary[c]
      << ", S_decompose_secondary = " << S_decompose_secondary[c]
      << ", S_transform = " << S_transform[c]
      << ", decompose_factor = " << decompose_factor[c]
      << ", tillage = " << tillage[c]
      << ", lag = " << lag[c]
      << ", sink_dt = " << sink_dt
      << ", sink_cell = " << sink_cell;
}

bool 
ChemicalBase::check (const Scope& scope, 
		     const Geometry& geo, 
		     const Soil& soil, const SoilWater& soil_water,
		     const OrganicMatter&, const Chemistry& chemistry, 
		     Treelog& msg) const
{
  TREELOG_MODEL (msg);

  const size_t cell_size = geo.cell_size ();

  // Warn against untraced chemicals.
  if (!chemistry.know (objid) && !chemistry.ignored (objid))
    msg.warning ("This chemical will not be traced");
  else 
    for (size_t i = 0; i < product.size (); i++)
      {
        const symbol chemical = product[i]->chemical;
	Treelog::Open nest (msg, chemical);
        if (!chemistry.know (chemical) && !chemistry.ignored (chemical))
          msg.warning ("Decompose product will not be traced");
	else
	  {
	    const Chemical& product = chemistry.find (chemical);
	    
	    if (molar_mass () < 0.0 && product.molar_mass () < 0.0)
	      msg.debug ("Decompose product convertion will be mass based");
	    else if (molar_mass () > 0.0 && product.molar_mass () > 0.0)
	      msg.debug ("Decompose product convertion will be mole based");
	    else
	      msg.warning ("Decompose product convertion will be mass based, because molar mass is only specified for one of the chemicals");
	    
	  }
      }

  bool ok = true;

  if (!C_below_expr->check_dim (units, scope, g_per_cm3, msg))
    ok = false;

  const bool solid = adsorption_->full ();

  for (size_t i = 0; i < cell_size; i++)
    {
      const double Theta_primary = soil_water.Theta_primary (i);
      const double Theta_secondary = soil_water.Theta_secondary (i);
      const double M = M_total_[i];
      const double M_secondary = M_secondary_[i];
      const double M_primary = M_primary_[i];
      const double C = C_avg_[i];
      const double C_secondary = C_secondary_[i];
      const double C_primary = C_primary_[i];
          
      try 
        {   
          if (Theta_secondary > 0.0 && 
              !approximate (adsorption_->M_to_C2 (soil, Theta_secondary, i, 
                                                  M_secondary),
                            C_secondary))
            {
              std::ostringstream tmp;
              tmp << "Theta_secondary = " << Theta_secondary 
                  << ", M_secondary = " << M_secondary
                  << ", M2C = " << (adsorption_->M_to_C2 (soil, Theta_secondary,
                                                          i, M_secondary))
                  << ", C = " << C_secondary;
              msg.message (tmp.str ());
              throw "C_secondary does not match M_secondary";
            }
          if (!approximate (adsorption_->M_to_C1 (soil, Theta_primary, i, 
                                                  M_primary),
                            C_primary))
            throw "C_primary does not match M_primary";
          if (!approximate (M_primary, M - M_secondary)
	      && !approximate (M, M_primary + M_secondary, 0.01))
            throw "M_primary should be M - M_secondary"; 
          if (M_secondary > M * 1.0001)
            throw "M_secondary > M";
          if (M_primary > M  * 1.0001)
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
          std::stringstream tmp1;
          tmp1 << objid << "[" << i << "]";
          Treelog::Open next (msg, tmp1.str ());
          std::stringstream tmp2;
          tmp2 << error;
          tmp2 << "\nTheta_primary = " << Theta_primary;
          tmp2 << "\nTheta_secondary = " << Theta_secondary;
          tmp2 << "\nM = " << M;
          tmp2 << "\nM_secondary = " << M_secondary;
          tmp2 << "\nM_primary = " << M_primary;
          tmp2 << "\nC = " << C;
          tmp2 << "\nC_secondary = " << C_secondary;
          tmp2 << "\nC_primary = " << C_primary;
          msg.error (tmp2.str ());
          ok = false;
        }
    }
  return ok;
}

void 
ChemicalBase::fillup (std::vector<double>& v, const size_t size)
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
ChemicalBase::initialize (const Scope& parent_scope,
			  const Geometry& geo,
			  const Soil& soil, const SoilWater& soil_water, 
			  const SoilHeat& soil_heat,
			  Treelog& msg)
{
  TREELOG_MODEL (msg);
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  C_below_expr->initialize (units, parent_scope, msg);

  std::vector<double> Ms;
  geo.initialize_layer (C_avg_, frame (), "C", msg);
  geo.initialize_layer (C_secondary_, frame (), "C_secondary", msg);
  geo.initialize_layer (M_secondary_, frame (), "M_secondary", msg);
  geo.initialize_layer (M_total_, frame (), "M", msg);
  geo.initialize_layer (Ms, frame (), "Ms", msg);

  fillup (C_avg_, cell_size);
  fillup (C_secondary_, cell_size);
  fillup (M_secondary_, cell_size);
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
      Treelog::Open nest (msg, "cell", i, "loop");
      daisy_assert (M_primary_.size () == i);

      const double Theta = soil_water.Theta (i);
      const double Theta_primary = soil_water.Theta_primary (i);
      const double Theta_secondary = soil_water.Theta_secondary (i);
      daisy_assert (approximate (Theta, Theta_secondary + Theta_primary));
      const bool has_C_secondary = C_secondary_.size () > i;
      const bool has_M_secondary = M_secondary_.size () > i;
      const bool has_C_avg  = C_avg_.size () > i;
      const bool has_M_total  = M_total_.size () > i;
      daisy_assert (has_C_avg || has_M_total);
      daisy_assert (!has_C_avg || C_avg_[i] >= 0);
      daisy_assert (!has_C_secondary || C_secondary_[i] >= 0);
      daisy_assert (!has_M_secondary || M_secondary_[i] >= 0);
      daisy_assert (!has_M_total || M_total_[i] >= 0);
      
      if (iszero (Theta_secondary))
        // No secondary water.
        {
          if (!has_C_avg)
            C_avg_.push_back (adsorption_->M_to_C1 (soil, Theta, i, 
                                                    M_total_[i]));
          M_primary_.push_back (adsorption_->C_to_M1 (soil, Theta, i, 
                                                      C_avg_[i])); 
          if (!has_M_secondary)
            M_secondary_.push_back (0.0);
          if (!has_M_total) 
            M_total_.push_back (M_primary_[i] + M_secondary_[i]);
          if (!has_C_secondary)
            C_secondary_.push_back (C_avg_[i]);
          C_primary_.push_back (C_avg_[i]);
        }
      else if (!has_C_secondary && !has_M_secondary)
        // Secondary water in equilibrium.
        {
	  Treelog::Open nest (msg, "cell", i, "b");
          daisy_assert (has_C_avg || has_M_total);

          if (!has_C_avg)
            C_avg_.push_back (adsorption_->M_to_C_total (soil, Theta, i, 
                                                         M_total_[i]));
          if (!has_M_total) 
            M_total_.push_back (adsorption_->C_to_M_total (soil, Theta, i, 
                                                           C_avg_[i])); 
          C_primary_.push_back (C_avg_[i]);
          C_secondary_.push_back (C_avg_[i]);
          M_primary_.push_back (adsorption_->C_to_M1 (soil, Theta_primary, i, 
                                                      C_primary_[i]));
          M_secondary_.push_back (adsorption_->C_to_M2 (soil, 
                                                        Theta_secondary, i, 
                                                        C_secondary_[i]));
        }
      else if (has_C_avg)
        // Average and secondary concentrations known.
        {
	  Treelog::Open nest (msg, "cell", i, "c");
          daisy_assert (has_C_secondary || has_M_secondary);
          if (!has_C_secondary)
            C_secondary_.push_back (adsorption_->M_to_C2 (soil, Theta_secondary,
                                                          i, 
                                                          M_secondary_[i]));
          if (!has_M_secondary)
            M_secondary_.push_back (adsorption_->C_to_M2 (soil, Theta_secondary,
                                                          i, 
                                                          C_secondary_[i]));
          
          // Theta * C_a = Theta_i * C_i + Theta_m * C_m
          // => C_i = (Theta * C_a - Theta_m * C_m) / Theta_i
          C_primary_.push_back (std::max ((Theta * C_avg_[i]
					   - Theta_secondary * C_secondary_[i])
					  / Theta_primary,
					  0.0));
          M_primary_.push_back (adsorption_->C_to_M1 (soil, Theta_primary, i, 
                                                      C_primary_[i]));
          if (!has_M_total)
            M_total_.push_back (M_primary_[i] + M_secondary_[i]);
        }
      else
        // Averarage concentration and total matter known.
        {
	  Treelog::Open nest (msg, "cell", i, "d");
          daisy_assert (has_C_secondary || has_M_secondary);
          if (!has_C_secondary)
            C_secondary_.push_back (adsorption_->M_to_C2 (soil, Theta_secondary,
                                                          i, 
                                                          M_secondary_[i]));
          if (!has_M_secondary)
            M_secondary_.push_back (adsorption_->C_to_M2 (soil, Theta_secondary,
                                                          i, 
                                                          C_secondary_[i]));
          daisy_assert (has_M_total);
          M_primary_.push_back (std::max (M_total_[i] - M_secondary_[i], 0.0));
          C_primary_.push_back (adsorption_->M_to_C1 (soil, 
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
  daisy_assert (M_secondary_.size () == cell_size);
  daisy_assert (M_primary_.size () == cell_size);
  daisy_assert (M_total_.size () == cell_size);
  M_error.insert (M_error.begin (), cell_size, 0.0);
  M_tertiary_.resize (cell_size, 0.0);
  S_secondary_.resize (cell_size, 0.0);
  S_primary_.resize (cell_size, 0.0);
  S_exchange.resize (cell_size, 0.0);
  S_indirect_drain.resize (cell_size, 0.0);
  S_soil_drain.resize (cell_size, 0.0);
  S_p_drain.resize (cell_size, 0.0);
  S_B2M.resize (cell_size, 0.0);
  S_M2B.resize (cell_size, 0.0);
  S_external.resize (cell_size, 0.0);
  S_permanent.resize (cell_size, 0.0);
  S_root.resize (cell_size, 0.0);
  S_decompose.resize (cell_size, 0.0);
  S_decompose_primary.resize (cell_size, 0.0);
  S_decompose_secondary.resize (cell_size, 0.0);
  S_transform.resize (cell_size, 0.0);
  decompose_factor.resize (cell_size, 1.0);
  J_primary.resize (edge_size, 0.0);
  J_secondary.resize (edge_size, 0.0);
  J_matrix.resize (edge_size, 0.0);
  J_tertiary.resize (edge_size, 0.0);
  tillage.resize (cell_size, 0.0);
  lag.resize (cell_size, 0.0);
}

ChemicalBase::ChemicalBase (const BlockModel& al)
  : Chemical (al),
    units (al.units ()),
    molar_mass_ (al.number ("molar_mass", -1.0)),
    solubility (al.number ("solubility")),
    solubility_infiltration_factor
    /**/ (al.number ("solubility_infiltration_factor")),
    crop_uptake_reflection_factor 
    /**/ (al.number ("crop_uptake_reflection_factor")),
    canopy_dissipation_rate 
    /**/ (al.check ("canopy_dissipation_rate")
          ? al.number ("canopy_dissipation_rate")
          : (al.check ("canopy_dissipation_halftime")
             ? halftime_to_rate (al.number ("canopy_dissipation_halftime"))
             : al.number ("canopy_dissipation_rate_coefficient"))),
    canopy_washoff_coefficient (al.number ("canopy_washoff_coefficient")),
    surface_decompose_rate (al.check ("surface_decompose_rate")
                            ? al.number ("surface_decompose_rate")
                            : (al.check ("surface_decompose_halftime")
                               ? halftime_to_rate (al.number ("surface_decompose_halftime"))
                               : canopy_dissipation_rate)),
    litter_decompose_rate (al.check ("litter_decompose_rate")
			   ? al.number ("litter_decompose_rate")
			   : (al.check ("litter_decompose_halftime")
			      ? halftime_to_rate (al.number ("litter_decompose_halftime"))
			      : canopy_dissipation_rate)),
    litter_washoff_coefficient (al.number ("litter_washoff_coefficient")),
    litter_diffusion_rate (al.number ("litter_diffusion_rate")),
    diffusion_coefficient_ (al.number ("diffusion_coefficient") * 3600.0),
    decompose_rate (al.check ("decompose_rate")
                    ? al.number ("decompose_rate")
                    : halftime_to_rate (al.number ("decompose_halftime"))),
    decompose_conc_factor (al.plf ("decompose_conc_factor")),
    decompose_lag_increment (al.plf ("decompose_lag_increment")),
    enable_surface_products (al.flag ("enable_surface_products")),
    soil_affects_surface_decompose (al.flag ("soil_affects_surface_decompose")),
    product (map_submodel_const<Product> (al, "decompose_products")),
    drain_secondary (al.flag ("drain_secondary")),
    C_below_expr (Librarian::build_item<Number> (al, "C_below")),
    C_below_value (-42.42e42),
    initial_expr (Librarian::build_item<Number> (al, "initial")),
    adsorption_ (Librarian::build_item<Adsorption> (al, "adsorption")),
    deposit_ (0.0), 
    spray_overhead_ (0.0),
    spray_surface_ (0.0),
    dissipate_surface_ (0.0),
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
    canopy_transform (0.0),
    litter_storage (al.number ("litter_storage")),
    litter_in (0.0),
    litter_decompose (0.0),
    litter_transform (0.0),
    litter_out (0.0),
    litter_leak (0.0),
    litter_diffuse (0.0),
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
    surface_decompose_factor (1.0),
    lag (al.check ("lag")
         ? al.number_sequence ("lag")
         : std::vector<double> ()),
    sink_dt (NAN),
    sink_cell (Geometry::cell_error)
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
Initial concentration in soil water with sorbtion ignored.\n\
That is M = C * Theta.");
    frame.order ("C");
  }
} NumberInitialC_syntax;

static struct InitialZeroSyntax : public DeclareParam
{ 
  InitialZeroSyntax ()
    : DeclareParam (Number::component, "initial_zero", "const", "\
Initial zero content.")
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

static struct ChemicalBaseSyntax : public DeclareBase
{
  ChemicalBaseSyntax ()
    : DeclareBase (Chemical::component, "base", "\
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

    if (al.check ("litter_decompose_rate") 
        && al.check ("litter_decompose_halftime"))
      {
        msg.entry ("\
You may not specify both 'litter_decompose_rate' and \
'litter_decompose_halftime'");
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
  { Geometry::add_layer (frame, "g/cm^3", Check::non_negative (),
			 Attribute::Const, "\
Concentration in water."); }

  static void load_C_secondary (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Check::non_negative (),
			 Attribute::Const, "\
Concentration in secondary domain."); }

  static void load_C_primary (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Check::non_negative (),
			 Attribute::Const, "\
Concentration in primary domain."); }

  static void load_M (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Check::non_negative (),
			 Attribute::Const, "\
Total mass per volume water, soil, and air."); }

  static void load_M_secondary (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Check::non_negative (),
			 Attribute::Const, "\
Secondary domain mass per volume water, soil, and air."); }

  static void load_M_primary (Frame& frame)
  { Geometry::add_layer (frame, "g/cm^3", Check::non_negative (),
			 Attribute::Const, "\
Primary domain mass per volume water, soil, and air."); }

  static void load_Ms (Frame& frame)
  { Geometry::add_layer (frame, Attribute::Fraction (), Check::none (),
			 Attribute::Const, "\
Mass in dry soil.\n\
This include all matter in both soil and water, relative to the\n\
dry matter weight.\n\
Only for initialization of the 'M' parameter."); }

  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    Model::load_model (frame);

    // Surface parameters.
    frame.declare ("molar_mass", "g/mol",
		   Check::non_negative (), Attribute::OptionalConst, "\
Molar mass of the chemical.");
    frame.declare ("solubility", "g/cm^3", 
                   Check::non_negative (), Attribute::Const, "\
Maximal concentration in water at 20 dg C.");
    frame.set ("solubility", 1.0);
    frame.declare ("solubility_infiltration_factor", Attribute::None (),
                   Check::non_negative (), Attribute::Const, "\
Adjustment for maximum concentration in infiltrated water.");
    frame.set ("solubility_infiltration_factor", 1.0);
    frame.declare_fraction ("crop_uptake_reflection_factor", 
                            Attribute::Const, "\
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
                   "How fast does the chemical decompose on surface.\n\
You must specify it with either 'surface_decompose_halftime' or\n\
'surface_decompose_rate'.  If neither is specified,\n\
'canopy_dissipation_rate' is used.");
    frame.declare ("surface_decompose_halftime", "h", 
                   Check::positive (), Attribute::OptionalConst,
                   "How fast does the chemical decompose on surface.\n\
You must specify it with either 'surface_decompose_halftime' or\n\
'surface_decompose_rate'.  If neither is specified,\n\
'canopy_dissipation_rate' is used.");
    frame.declare ("litter_decompose_rate", "h^-1", 
                   Check::fraction (), Attribute::OptionalConst,
                   "How fast does the chemical decompose on litter.\n\
You must specify it with either 'litter_decompose_halftime' or\n\
'litter_decompose_rate'.  If neither is specified,\n\
'canopy_dissipation_rate' is used.");
    frame.declare_fraction ("litter_washoff_coefficient", Attribute::Const, "\
Fraction of the chemical that follows the water off the litter.");
    frame.set ("litter_washoff_coefficient", 1.0);
    frame.declare ("litter_diffusion_rate", "h^-1", Attribute::Const, "\
How fast chemical diffuse to water passing on surface.");
    frame.set ("litter_diffusion_rate", 0.0);
    frame.declare ("litter_decompose_halftime", "h", 
                   Check::positive (), Attribute::OptionalConst,
                   "How fast does the chemical decompose on litter.\n\
You must specify it with either 'litter_decompose_halftime' or\n\
'litter_decompose_rate'.  If neither is specified,\n\
'surface_decompose_rate' is used.");

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

    frame.declare ("decompose_conc_factor", "g/cm^3 H2O", Attribute::None (),
                   Attribute::Const,
                   "Concentration development factor on decomposition.");
    frame.set ("decompose_conc_factor", PLF::empty ());
    frame.declare ("decompose_lag_increment", 
                   "g/cm^3", "h^-1", Attribute::Const,
                   "Increment lag with the value of this PLF for the current\n\
concentration each hour.  When lag in any cell reaches 1.0,\n\
decomposition begins.  It can never be more than 1.0 or less than 0.0.\n\
By default, there is no lag.");
    frame.set ("decompose_lag_increment", PLF::empty ());
    frame.declare_boolean ("enable_surface_products", Attribute::Const, "\
True if metabolites of this chemical can be generated on the surface.");
    frame.set ("enable_surface_products", false);
    frame.declare_boolean ("soil_affects_surface_decompose",
			   Attribute::Const, "\
True if soil conditions affect surface decomposition.");
    frame.set ("soil_affects_surface_decompose", false);
    
    frame.declare_boolean ("drain_secondary", Attribute::Const, "\
Concentration in secondary soil water user for drainage.\n\
If you set this to true the concentration in the secondary domain is used\n\
for concentration in drain water.  Otherwise, the average concentration is\n\
the matix is used.  Using the secondary domain is more physically correct,\n\
but also more likely to give unstable results.");
    frame.set ("drain_secondary", false);
    frame.declare_object ("C_below", Number::component, 
                          Attribute::Const, Attribute::Singleton, "\
Concentration below the layer of soil being examined.\n\
Use a negative number to indicate same concentration as in lowest cell.");
    frame.declare_submodule_sequence ("decompose_products", Attribute::Const, "\
List of products from decomposition.", ChemicalBase::Product::load_syntax);
    frame.set_empty ("decompose_products");
    frame.set ("C_below", "zero_gradient");
    frame.declare_object ("initial", Number::component, 
                          Attribute::Const, Attribute::Singleton, "\
Initial content (M) if otherwise unspecified. [g/cm^3].");
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
    frame.declare ("spray_overhead", "g/m^2/h", Attribute::LogOnly,
                   "Amount currently being applied above canopy.");
    frame.declare ("spray_surface", "g/m^2/h", Attribute::LogOnly,
                   "Amount currently being applied below canopy.");
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
    frame.declare ("canopy_transform", "g/m^2/h", Attribute::LogOnly, 
                   "Added through transformation to canopy.");
    frame.declare ("canopy_harvest", "g/m^2/h", Attribute::LogOnly, 
                   "Amount removed with crop harvest.");

    frame.declare ("litter_storage", "g/m^2", Attribute::State, 
                   "Stored in the litter (mulch, surface residuals).");
    frame.set ("litter_storage", 0.0);
    frame.declare ("litter_in", "g/m^2/h", Attribute::LogOnly, 
                   "Entering litter .");
    frame.declare ("litter_decompose", "g/m^2/h", Attribute::LogOnly, 
                   "Decomposed from the litter.");
    frame.declare ("litter_transform", "g/m^2/h", Attribute::LogOnly, 
                   "Added through transformation in litter layer.");
    frame.declare ("litter_out", "g/m^2/h", Attribute::LogOnly, 
                   "Leaking from litter.");
    frame.declare ("litter_leak", "g/m^2/h", Attribute::LogOnly, 
                   "Leaking from bottom litter.");
    frame.declare ("litter_diffuse", "g/m^2/h", Attribute::LogOnly, 
                   "Diffusing from litter to water passing on surface.");

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
    Geometry::add_layer (frame, Attribute::LogOnly, "C_primary", 
                         load_C_primary);
    Geometry::add_layer (frame, Attribute::OptionalState, "M", load_M);
    Geometry::add_layer (frame, Attribute::OptionalState, "M_secondary",
                         load_M_secondary);
    Geometry::add_layer (frame, Attribute::LogOnly, "M_primary", 
                         load_M_primary);
    Geometry::add_layer (frame, Attribute::OptionalConst, "Ms", load_Ms);
    frame.declare ("M_secondary", "g/cm^3", 
                   Attribute::LogOnly, Attribute::SoilCells, 
                   "Mass in secondary domain.");
    frame.declare ("M_error", "g/cm^3", Attribute::LogOnly, Attribute::SoilCells, 
                   "Mass substracted to avoid negative values.");
    frame.declare ("M_tertiary", "g/cm^3", Attribute::LogOnly, Attribute::SoilCells, 
                   "Mass in tertiary domain (biopores).");
    frame.declare ("S_secondary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Secondary matrix source term.");
    frame.declare ("S_primary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Primary matrix source term.");
    frame.declare ("S_exchange", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Exchange from primary to secondary domain.");
    frame.declare ("S_indirect_drain", "g/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, "\
Added to the soil matrix from drains indirectly via biopores.\n\
This can be non-zero whereever there are drain connected biopores.");
    frame.declare ("S_soil_drain", "g/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, "\
Added to the soil matrix from drains directly, not via biopores.\n\
This is only non-zero in drain nodes.");
    frame.declare ("S_p_drain", "g/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, "\
Removed from the biopores to the drain.\n\
This is only non-zero in drain nodes.");
    frame.declare ("S_B2M", "g/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, "\
Biopores to matrix.");
    frame.declare ("S_M2B", "g/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, "\
Matrix to biopores (negative matrix source).\n\
Does not count flow to drain connected biopores.");
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
    frame.declare ("S_decompose_primary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term for decompose in primary domain, is never positive.");
    frame.declare ("S_decompose_secondary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term for decompose in secondary domain, is never positive.");
    frame.declare ("S_transform", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Source term for transformations other than sorption.");
    frame.declare ("decompose_factor", Attribute::None (), Attribute::LogOnly, Attribute::SoilCells,
                   "Decompose factor due to soil conditions.");
    frame.declare ("surface_decompose_factor", Attribute::None (),
		   Attribute::LogOnly, 
                   "Decompose factor due to soil conditions near surface.");
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
    frame.declare ("dt", "h", Attribute::LogOnly, "\
Suggested timestep length based on sink terms.");
    frame.declare_integer ("sink_cell", Attribute::LogOnly, "\
Cell with largest forward sink compared to available matter.");
  }
} ChemicalBase_syntax;

// The 'default' chemical model.

struct ChemicalStandard : public ChemicalBase 
{
  const PLF decompose_heat_factor;
  const double T_scale;		 // Scale to T_ref []
  const PLF decompose_water_factor;
  const PLF decompose_CO2_factor;
  const PLF decompose_depth_factor;
  const int decompose_SMB_pool;
  const double decompose_SMB_KM; // [g C/cm^3]
  const double SMB_scale;	// Scale to SMB_ref []
  
  double find_T_factor_raw (const double T) const
  {
    if (decompose_heat_factor.size () < 1)
      return Abiotic::f_T0 (T);

    return decompose_heat_factor (T);
  }

  double find_T_factor (const double T) const
  { return T_scale * find_T_factor_raw (T); }

  double find_SMB_factor_raw (const double SMB_C) const
  {
    if (decompose_SMB_KM > 0.0)
      return SMB_C / (decompose_SMB_KM + SMB_C);

    return 1.0;
  }

  double find_SMB_factor (const double SMB_C) const
  {
    return SMB_scale * find_SMB_factor_raw (SMB_C);
  }

  double decompose_soil_factor (size_t c,
				const Geometry&, const Soil&, 
				const SoilWater&, const SoilHeat&, 
				const OrganicMatter&) const;

  bool check (const Scope& scope, const Geometry& geo, 
	      const Soil& soil, const SoilWater& soil_water,
	      const OrganicMatter& organic_matter, const Chemistry& chemistry, 
	      Treelog& msg) const
  {
    bool ok = ChemicalBase::check (scope, geo, soil, soil_water,
				   organic_matter, chemistry, msg);
    if (decompose_SMB_KM > 0.0
	&& decompose_SMB_pool >= organic_matter.get_smb ().size ())
      {
	msg.error ("'decompose_smb_pool' too high");
	ok = false;
      }
    return ok;
  }

  ChemicalStandard (const BlockModel& al)
    : ChemicalBase (al),
      decompose_heat_factor (al.plf ("decompose_heat_factor")),
      T_scale (Abiotic::find_T_scale (al)), 
      decompose_water_factor (al.plf ("decompose_water_factor")),
      decompose_CO2_factor (al.plf ("decompose_CO2_factor")),
      decompose_depth_factor (al.plf ("decompose_depth_factor")),
      decompose_SMB_pool (al.integer ("decompose_SMB_pool")),
      decompose_SMB_KM (al.number ("decompose_SMB_KM")),
      SMB_scale (Abiotic::find_SMB_scale (al))
  { }
};

double
ChemicalStandard::decompose_soil_factor
/**/ (size_t c, const Geometry& geo,
      const Soil& soil, const SoilWater& soil_water, const SoilHeat& soil_heat, 
      const OrganicMatter& organic_matter) const
{
  double factor = 1.0;

  // Adjust for depth.
  factor *= decompose_depth_factor (geo.cell_z (c));
  
  // Adjust for heat.
  factor *= find_T_factor (soil_heat.T (c));
  
  // Adjust for moisture.
  if (decompose_water_factor.size () < 1)
    factor *= Abiotic::f_h (soil_water.h (c));
  else
    factor *= decompose_water_factor (soil_water.h (c));

  // Adjust for biological activity.
  if (decompose_CO2_factor.size () > 0)
    factor *= decompose_CO2_factor (organic_matter.CO2 (c));

  if (decompose_SMB_KM > 0.0)
    {
      // MichaelisMenten kineticsa
      const std::vector <SMB*>& smb = organic_matter.get_smb ();
      daisy_assert (decompose_SMB_pool >= 0);
      if (decompose_SMB_pool >= smb.size ())
	throw "Unknown SMB pool";
      const SMB& pool = *smb[decompose_SMB_pool];
      daisy_assert (pool.C.size () > c);
      const double C = pool.C[c];
      const double f_N = find_SMB_factor (C);
      factor *= f_N;
    }
  return factor;
}

static struct ChemicalStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ChemicalStandard (al); }
  void load_frame (Frame& frame) const
  {
    Abiotic::load_frame (frame);
    frame.declare ("decompose_CO2_factor", "g CO2-C/cm^3/h", Attribute::None (),
                   Attribute::Const,
                   "CO2 development factor on decomposition.");
    frame.set ("decompose_CO2_factor", PLF::empty ());
    frame.declare ("decompose_depth_factor", "cm", Attribute::None (),
                   Attribute::Const,
                   "Depth influence on decomposition.");
    frame.set ("decompose_depth_factor", PLF::always_1 ());
  }
  ChemicalStandardSyntax ()
    : DeclareModel (Chemical::component, "default", "base", "\
Read chemical properties as normal Daisy parameters.")
  { }
} ChemicalStandard_syntax;

// The 'FOCUS' chemical model.

struct ChemicalFOCUS : public ChemicalBase 
{
  const double B;
  const double alpha;
  const double T_ref;
  const std::vector<double> z;
  const std::vector<double> z_factor;

  double decompose_soil_factor (size_t c,
				const Geometry&, const Soil&, 
				const SoilWater&, const SoilHeat&, 
				const OrganicMatter&) const;

  ChemicalFOCUS (const BlockModel& al)
    : ChemicalBase (al),
      B (al.number ("B")),
      alpha (al.number ("alpha")),
      T_ref (al.number ("T_ref")),
      z (al.number_sequence ("z")),
      z_factor (al.number_sequence ("z_factor"))
  { }
};

double
ChemicalFOCUS::decompose_soil_factor (size_t c,
				      const Geometry& geo,
				      const Soil& soil, 
				      const SoilWater& soil_water,
				      const SoilHeat& soil_heat, 
				      const OrganicMatter&) const
{
  // Depth.
  const double depth = geo.cell_z (c);

  double depth_factor = 0.0;
  for (size_t i = 0; i < z.size (); i++)
    if (depth > z[i])
      {
	depth_factor = z_factor[i];
	break;
      }

  // Water.
  const double Theta = soil_water.Theta (c);
  const double h_ice = soil_water.h_ice (c);
  const double h_wp = -15000.0;
  const double h_fc = -100.0;
  const double Theta_wp_05 = 0.5 * soil.Theta (c, h_wp, h_ice);
  const double Theta_fc = soil.Theta (c, h_fc, h_ice);

  // From MACRO 5.2 changes documents:
  const double water_factor 
    = (Theta < Theta_wp_05)
    ? 0.0 
    : ((Theta < Theta_fc)
       ? std::pow ((Theta - Theta_wp_05) / (Theta_fc - Theta_wp_05), B)
       : 1.0);
  daisy_assert (water_factor >= 0.0);
  daisy_assert (water_factor <= 1.0);

  // Heat.
  const double T = soil_heat.T (c);

  // From MACRO 5.0 technical documentation:
  const double heat_factor 
    = (T < 0.0)
    ? 0.0
    : ((T > 5.0)
       ? std::exp (alpha * (T - T_ref))
       : 0.2 * T * std::exp (alpha * (5.0 - T_ref)));
  daisy_assert (heat_factor >= 0.0);
  daisy_assert (heat_factor <= 50.0);

  return depth_factor * water_factor * heat_factor;
}

static struct ChemicalFOCUSSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ChemicalFOCUS (al); }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "focus2002", "focussw2002");
    frame.declare ("B", Attribute::None (), Check::none (),
		   Attribute::Const, "Soil moisture effect parameter.");
    frame.set_cited ("B", 0.49, "Section 7.4.5", "focussw2002");
    frame.declare ("T_ref", "dg C", Check::none (),
		   Attribute::Const, "Reference temperature.");
    frame.set_cited ("T_ref", 20.0, "Section 7.4.2", "focussw2002");
    frame.declare ("alpha", "K^-1", Check::none (),
		   Attribute::Const, "Temperature effect parameter.");
    frame.set_cited ("alpha", 0.0948, "Section 7.4.4", "focussw2002");
    frame.declare ("z", "cm", Check::negative (), Attribute::Const,
		   Attribute::Variable, "\
End of each interval.");
    frame.set_check ("z", VCheck::decreasing ());
    frame.set ("z", std::vector<double> ({ -30.0, -60.0, -100.0 }));
    frame.declare ("z_factor", Attribute::None (), Check::non_negative (),
		   Attribute::Const, Attribute::Variable, "\
Factor to apply to decompose rate at each interval.");
    frame.set ("z_factor", std::vector<double> ({1.0, 0.5, 0.3}));

    // 'base' paramaters.
    frame.set_cited ("canopy_dissipation_halftime", 10.0 * 24.0,
		     "Section 7.4.10", "focussw2002");
    frame.set_cited ("canopy_washoff_coefficient", 0.075, "\
Washoff in FOCUS use FEXTRC = 0.05.\n\
CWC = IC * FEXTRC\n\
CWC = canopy_washoff_coefficient in Daisy\n\
IC = intercepted water in Daisy\n\
Default interception capacity in Daisy is LAI * 0.5 mm\n\
The FEXTRC parameter is based on an experiment with mature cotton.\n\
If we assume this is LAI=3, then corresponding CWC is 0.075.",
		     "focussw2002");
    frame.set_cited ("crop_uptake_reflection_factor", 1.0,
		     "Recommended value.", "focusgw2002");
    frame.set_cited ("diffusion_coefficient", 5e-6,
		     "Section 7.3.5", "focussw2002");
  }
  ChemicalFOCUSSyntax ()
    : DeclareModel (Chemical::component, "FOCUS", "base", "\
Pesticides inspired by FOCUS surface water.\n\
\n\
Depth, moisture, and heat effect according to the FOCUS Surface Water WG.\n\
\n\
Water factor is zero below 0.5 Theta_wp, one above field capacity, and \n\
((Theta - 0.5 Theta_wp) / (Theta_fc - 0.5 Theta_wp))^B\n\
otherwise. From MACRO 5.2.\n\
\n\
Heat factor is zero below zero degrees,\n\
0.2 T e^(alpha (5 - T_ref)) below 5 dg C, and\n\
and e^(alpha (T - T_ref)) above 5 dg C. From MACRO 5.0.\n\
\n\
Depth factor is specified by 'z' and 'z_factor'.")
  { }
} ChemicalFOCUS_syntax;

// The 'nutrient' chemical model.

struct ChemicalNutrient : public ChemicalBase 
{
  double decompose_soil_factor (size_t c,
				const Geometry&, const Soil&, 
				const SoilWater&, const SoilHeat&, 
				const OrganicMatter&) const
  { return 1.0; }
  ChemicalNutrient (const BlockModel& al)
    : ChemicalBase (al)
  { }
};

static struct ChemicalNutrientSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ChemicalNutrient (al); }
  void load_frame (Frame& frame) const
  {
    frame.set ("crop_uptake_reflection_factor", 1.0); // Specific uptake code.
    frame.set ("canopy_dissipation_rate", 0.0);
    frame.set ("canopy_washoff_coefficient", 1.0);
    frame.set ("decompose_rate", 0.0);
  }
  ChemicalNutrientSyntax ()
    : DeclareModel (Chemical::component, "nutrient", "base", "\
Plants eat this stuff.")
  { }
} ChemicalNutrient_syntax;

static struct ChemicalNitrogenSyntax : public DeclareParam
{
  ChemicalNitrogenSyntax ()
    : DeclareParam (Chemical::component, "N", "nutrient", "\
Nitrogen.")
  { }
  void load_frame (Frame&) const
  { }
} ChemicalNitrogen_syntax;

static struct ChemicalMINSyntax : public DeclareParam
{
  ChemicalMINSyntax ()
    : DeclareParam (Chemical::component, "MIN", "N", "\
Non-organic nitrogen.")
  { }
  void load_frame (Frame&) const
  { }
} ChemicalMIN_syntax;

static struct InitialNO3Syntax : public DeclareParam
{ 
  InitialNO3Syntax ()
    : DeclareParam (Number::component, "initial_NO3", "initial_C", "\
Initial NO3 concentration in soil water.")
  { }
  void load_frame (Frame& frame) const
  {
    // We initialize to approximatey half the allowed content in
    // drinking water [ 0.5 * 50 mg NO3/l ~= 5.0e-6 g NO3-N/cm^3 ]
    frame.set ("C", 5e-6); 
  }
} InitialNO3_syntax;

static struct ChemicalNO3Syntax : public DeclareParam
{ 
  ChemicalNO3Syntax ()
    : DeclareParam (Chemical::component, "NO3", "MIN", "\
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
    // [ 0.05 * 50 mg/l = 0.5e-6 g/cm^3 ]
    frame.set ("C", 0.55e-6); 
  }
} InitialNH4_syntax;

static struct ChemicalNH4Syntax : public DeclareParam
{ 
  ChemicalNH4Syntax ()
    : DeclareParam (Chemical::component, "NH4", "MIN", "\
Ammonium-N.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("adsorption", "NH4");
    frame.set ("diffusion_coefficient", 1.8e-5);
    frame.set ("initial", "initial_NH4");
  }
} ChemicalNH4_syntax;

static struct ChemicalDONSyntax : public DeclareParam
{ 
  ChemicalDONSyntax ()
    : DeclareParam (Chemical::component, "DON", "N", "\
Nitrate-N.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_cited ("diffusion_coefficient", 1.9e-6, "900 Da, 2.5 nm",
		     "hendry2003geochemical");
    // Max. for Dp, optimized at 1.46 h-1 for glyphosate. JV 2021-10-27
    frame.set ("litter_diffusion_rate", 1.5); 
  }
} ChemicalDON_syntax;

static struct ChemicalCarbonSyntax : public DeclareParam
{
  ChemicalCarbonSyntax ()
    : DeclareParam (Chemical::component, "C", "nutrient", "\
Carbon.")
  { }
  void load_frame (Frame&) const
  { }
} ChemicalCarbon_syntax;

static struct ChemicalDOCSyntax : public DeclareParam
{ 
  ChemicalDOCSyntax ()
    : DeclareParam (Chemical::component, "DOC", "C", "\
Nitrate-N.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_cited ("diffusion_coefficient", 1.9e-6, "900 Da, 2.5 nm",
		     "hendry2003geochemical");
    // Max. for Dp, optimized at 1.46 h-1 for glyphosate. JV 2021-10-27
    frame.set ("litter_diffusion_rate", 1.5); 
  }
} ChemicalDOC_syntax;

// chemical_std.C ends here.
