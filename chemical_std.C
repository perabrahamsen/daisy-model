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
#include "organic_matter.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "soil.h"
#include "geometry.h"
#include "abiotic.h"
#include "adsorption.h"
#include "chemistry.h"
#include "log.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "plf.h"
#include "check.h"
#include "librarian.h"
#include "number.h"
#include "scope_soil.h"
#include "vcheck.h"
#include "memutils.h"
#include "submodeler.h"
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
    static void load_syntax (Syntax&, AttributeList&);
    Product (Block&);
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

  // Surface state and log.
  double snow_storage;
  double snow_in;
  double snow_out;
  
  double canopy_storage;
  double canopy_in;
  double canopy_dissipate;
  double canopy_harvest;
  double canopy_out;

  double surface_storage;
  double surface_in;
  double surface_out;
  double surface_mixture;
  double surface_runoff;
  double surface_decompose;

  // Soil state and log.
  std::vector<double> C_avg_;   // Concentration in soil solution [g/cm^3]
  std::vector<double> C_mobile_;   // Mobile conc. in soil solution [g/cm^3]
  std::vector<double> C_immobile_; // Immobile conc. in soil solution [g/cm^3]
  std::vector<double> M_immobile_; // Immobile conc. in soil [g/cm^3]
  std::vector<double> M_total_;	// Concentration in soil [g/cm^3]
  std::vector<double> S_mobile_;  // Mobile source term.
  std::vector<double> S_immobile_;// Immobile source term.
  std::vector<double> S_p_;	// Source term for macropores only.
  std::vector<double> S_drain;	// Source term for soil drainage only.
  std::vector<double> S_external; // External source term, e.g. incorp. fert.
  std::vector<double> S_permanent; // Permanent external source term.
  std::vector<double> S_root;	// Root uptake source term (negative).
  std::vector<double> S_decompose;	// Decompose source term.
  std::vector<double> S_transform;	// Transform source term.
  std::vector<double> J;		// Solute transport log in matrix.
  std::vector<double> J_p;             // Solute transport log in macropores.
  std::vector<double> tillage;         // Changes during tillage.
  std::vector<double> lag;

  // Utilities.
  double decompose_heat_factor (const double T) const;
  double decompose_water_factor (const double h) const;

  // Solute.
  const Adsorption& adsorption () const;
  double diffusion_coefficient () const;

  // Soil content.
  double C_below () const; // Concentration in groundwater [g/cm^3]
  double C_mobile (size_t) const;
  double C_immobile (size_t) const;
  double M_immobile (size_t) const;
  double M_total (size_t) const;
  double total_surface (const Geometry&, 
			double from, double to) const; // [g/cm^2]
  double S_mobile (size_t) const;
  double S_immobile (size_t) const;
  double S_p (size_t) const;
  
  // Transport.
  void set_macro_flux (size_t e, double value);
  void set_matrix_flux (size_t e, double value);
  void set_uniform (const Soil& soil, const SoilWater& soil_water, size_t c,
                    double M_total);
  void set_mixed (const Soil& soil, const SoilWater& soil_water, size_t c,
                  double M_total, double C_mobile);

  // Sink.
  void clear ();
  void add_to_source_mobile (const std::vector<double>&, double dt);
  void add_to_source_immobile (const std::vector<double>&, double dt);
  void add_to_sink_mobile (const std::vector<double>&, double dt);
  void add_to_sink_immobile (const std::vector<double>&, double dt);
  void add_to_root_sink (const std::vector<double>&, double dt);
  void add_to_decompose_sink (const std::vector<double>&, double dt);
  void add_to_transform_source (const std::vector<double>&, double dt);
  void add_to_transform_sink (const std::vector<double>&, double dt);

  // Management.
  void update_C (const Soil& soil, const SoilWater& soil);
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
                 const double cover, // [],
                 const double canopy_leak_rate, // [h^-1]
                 const double surface_runoff_rate, // [h^-1]
                 const double dt, // [h]
		 Treelog& msg);
  void tick_soil (const size_t cell_size, const SoilWater&, double dt,
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
  bool check (const Geometry&, const Soil&, const SoilWater&, 
	      const Chemistry&, const Scope&, Treelog&) const;
  static void fillup(std::vector<double>& v, const size_t size);
  void initialize (const AttributeList&, const Geometry&,
                   const Soil&, const SoilWater&, const SoilHeat&, Treelog&);
  static double find_surface_decompose_rate (Block& al);
  ChemicalStandard (Block&);
};

const symbol 
ChemicalStandard::g_per_cm3 ("g/cm^3");

void
ChemicalStandard::Product::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("fraction", Syntax::Fraction (), Syntax::Const,
	      "Fraction of decomposed matter that become this chemcial.");
  syntax.add ("chemical", Syntax::String, Syntax::Const, 
	      "Chemical product of decomposed matter.");
  syntax.order ("fraction", "chemical");
}

ChemicalStandard::Product::Product (Block& al)
  : fraction (al.number ("fraction")),
    chemical (al.identifier ("chemical"))
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
ChemicalStandard::C_below () const
{ return C_below_value; }

double 
ChemicalStandard::C_mobile (size_t i) const
{ return C_mobile_[i]; }

double 
ChemicalStandard::C_immobile (size_t i) const
{ return C_immobile_[i]; }

double 
ChemicalStandard::M_immobile (size_t i) const
{ return M_immobile_[i]; }

double 
ChemicalStandard::M_total (size_t i) const
{ return M_total_[i]; }

double
ChemicalStandard::total_surface (const Geometry& geo, 
				 const double from, const double to) const
{ return geo.total_surface (M_total_, from, to); }

double 
ChemicalStandard::S_mobile (size_t i) const
{ return S_mobile_[i]; }

double 
ChemicalStandard::S_immobile (size_t i) const
{ return S_immobile_[i]; }

double 
ChemicalStandard::S_p (size_t i) const
{ return S_p_[i]; }

void 
ChemicalStandard::set_macro_flux (const size_t e, const double value)
{ J_p[e] = value; }

void
ChemicalStandard::set_matrix_flux (const size_t e, const double value)
{ J[e] = value; }

void 
ChemicalStandard::set_uniform (const Soil& soil, const SoilWater& soil_water,
                               const size_t c, const double M_total)
{ 
  const double C_avg
    = adsorption_->M_to_C (soil, soil_water.Theta (c), c, M_total);
  M_immobile_[c] = M_total; 
  M_total_[c] = M_total;
  C_mobile_[c] = C_avg;
  C_immobile_[c] = C_avg;
  C_avg_[c] = C_avg;
}

void 
ChemicalStandard::set_mixed (const Soil& soil, const SoilWater& soil_water,
                             const size_t c,
                             const double M_total, const double C_mobile)
{ 
  const double Theta_total = soil_water.Theta (c);
  const double Theta_mobile = soil_water.Theta_mobile (c);
  const double Theta_immobile = soil_water.Theta_immobile (c);
  daisy_approximate (Theta_total, Theta_mobile + Theta_immobile);
  const double M_mobile = C_mobile * Theta_mobile;
  const double M_immobile = M_total - M_mobile;
  const double C_immobile 
    = adsorption_->M_to_C (soil, Theta_immobile, c, M_immobile);
  const double C_avg 
    = (C_mobile * Theta_mobile + C_immobile * Theta_immobile) / Theta_total;

  M_immobile_[c] = M_immobile; 
  M_total_[c] = M_total;
  C_mobile_[c] = C_mobile;
  C_immobile_[c] = C_immobile; 
  C_avg_[c] = C_avg;
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
  std::fill (S_mobile_.begin (), S_mobile_.end (), 0.0);
  std::fill (S_immobile_.begin (), S_immobile_.end (), 0.0);
  std::fill (S_external.begin (), S_external.end (), 0.0);
  std::fill (S_root.begin (), S_root.end (), 0.0);
  std::fill (S_decompose.begin (), S_decompose.end (), 0.0);
  std::fill (S_transform.begin (), S_transform.end (), 0.0);
  std::fill (tillage.begin (), tillage.end (), 0.0);
}

void
ChemicalStandard::add_to_source_mobile (const std::vector<double>& v,
                                        const double dt)
{
  daisy_assert (S_mobile_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_mobile_[i] += v[i];
      daisy_assert (std::isfinite (S_mobile_[i]));
    }
}


void
ChemicalStandard::add_to_source_immobile (const std::vector<double>& v,
                                        const double dt)
{
  daisy_assert (S_immobile_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_immobile_[i] += v[i];
      daisy_assert (std::isfinite (S_immobile_[i]));
    }
}

void
ChemicalStandard::add_to_sink_mobile (const std::vector<double>& v,
                                      const double dt)
{
  daisy_assert (S_mobile_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_mobile_[i] -= v[i];
      daisy_assert (std::isfinite (S_mobile_[i]));
    }
}

void
ChemicalStandard::add_to_sink_immobile (const std::vector<double>& v,
                                        const double dt)
{
  daisy_assert (S_immobile_.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_immobile_[i] -= v[i];
      daisy_assert (std::isfinite (S_immobile_[i]));
    }
}

void
ChemicalStandard::add_to_root_sink (const std::vector<double>& v,
				    const double dt)
{
  daisy_assert (S_root.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_root[i] -= v[i];
  add_to_sink_mobile (v, dt);
}

void
ChemicalStandard::add_to_decompose_sink (const std::vector<double>& v,
					const double dt)
{
  daisy_assert (S_decompose.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_decompose[i] -= v[i];
  add_to_sink_immobile (v, dt);
}

void
ChemicalStandard::add_to_transform_sink (const std::vector<double>& v,
					 const double dt)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] -= v[i];
  add_to_sink_immobile (v, dt);
}

void
ChemicalStandard::add_to_transform_source (const std::vector<double>& v,
					   const double dt)
{
  daisy_assert (S_transform.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_transform[i] += v[i];
  add_to_source_immobile (v, dt);
}

void
ChemicalStandard::update_C (const Soil& soil, const SoilWater& soil_water)
{
  for (size_t i = 0; i < C_immobile_.size (); i++)
    {
      C_avg_[i] = adsorption_->M_to_C (soil, soil_water.Theta (i),
                                       i, M_total_[i]);
      switch (soil_water.mobile_solute (i))
        {
        case SoilWater::immobile:
          C_mobile_[i] = C_avg_[i];
          C_immobile_[i] = C_avg_[i];
          M_immobile_[i] = M_total_[i];
          break;
        case SoilWater::mobile:
          C_mobile_[i] = C_avg_[i];
          C_immobile_[i] = C_mobile_[i];
          M_immobile_[i] = M_total_[i];
          break;
        case SoilWater::mixed:
          C_mobile_[i] = C_avg_[i];
          C_immobile_[i] = C_avg_[i];
          M_immobile_[i] = M_total_[i] - C_mobile_[i] 
            * soil_water.Theta_mobile (i);
          break;
        }
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
  const double removed = surface_storage * penetration;
  surface_tillage += removed / dt;
  surface_storage -= removed;
  
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
                            const double cover, // [],
                            const double canopy_leak_rate, // [h^-1]
                            const double surface_runoff_rate, // [h^-1]
                            const double dt, // [h]
			    Treelog& msg)
{
  const double old_storage = snow_storage + canopy_storage;

  // Snow pack
  snow_in = spray_ + deposit_;
  snow_storage += snow_in * dt;

  snow_out = snow_storage * snow_leak_rate;
  snow_storage -= snow_out * dt;

  // Canopy.
  canopy_in = snow_out * cover;
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

  // Surface
  surface_in = canopy_out + (snow_out - canopy_in);
  surface_runoff = surface_storage * surface_runoff_rate; 
  surface_decompose = surface_storage * surface_decompose_rate; 
  surface_storage += (surface_in - surface_runoff - surface_decompose) * dt;

  // Mass balance.
  const double new_storage = snow_storage + canopy_storage;
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

void                            // Called just before solute movement.
ChemicalStandard::tick_soil (const size_t cell_size,
			     const SoilWater& soil_water,
			     const double dt,
			     const Scope& scope,
			     Treelog& msg)
{
  // Find C below.
  if (!C_below_expr->tick_value (C_below_value, g_per_cm3, scope, msg))
    C_below_value = -1.0;

  // Initialize.
  std::fill (S_p_.begin (), S_p_.end (), 0.0);
  std::fill (J_p.begin (), J_p.end (), 0.0);

  // Permanent source.
  for (size_t i = 0; i < cell_size; i++)
    S_external[i] += S_permanent[i];
  add_to_source_mobile (S_external, dt); 
 
  // Drainage.
  for (size_t i = 0; i < cell_size; i++)
    S_drain[i] = -soil_water.S_drain (i) * C_mobile_[i];
  add_to_source_mobile (S_drain, dt); 
}

void 
ChemicalStandard::mixture (const Geometry& geo,
                           const double pond /* [mm] */, 
                           const double rate /* [h/mm] */,
                           const double dt /* [h]*/)
{
  // Make sure we have something to mix.
  if (pond < 1e-6 || rate < 1e-99)
    {
      surface_mixture = 0.0;
      return;
    }

  // Mix them.
  const double soil_conc
    = geo.content_at (static_cast<const Chemical&> (*this),
                      &Chemical::C_mobile, 0.0)
    * (100.0 * 100.0) / 10.0; // [g/cm^3/] -> [g/m^2/mm]
  const double storage_conc = surface_storage / pond;// [g/m^2/mm]
  
  surface_mixture // [g/m^2/h]
    = std::min (surface_storage / dt, (storage_conc - soil_conc) / rate);
  surface_storage -= surface_mixture * dt;
}

void 
ChemicalStandard::infiltrate (const double rate, const double dt)
{
  surface_out = surface_storage * rate;
  surface_storage -= surface_out * dt;
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
    uptaken[i] = C_mobile (i) * soil_water.S_root (i) * rate;
  
  add_to_root_sink (uptaken, dt);
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
      lag[c] += this->decompose_lag_increment (C_immobile_[c]) * dt;
      
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
	= this->decompose_conc_factor (C_immobile_[c]);
      const double depth_factor
	= this->decompose_depth_factor (geo.z (c));
      const double rate
	= decompose_rate * heat_factor * water_factor * CO2_factor
	* conc_factor * depth_factor;
      decomposed[c] = M_immobile (c) * rate;
    }

  this->add_to_decompose_sink (decomposed, dt);

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
	  chemical.add_to_transform_source (created, dt);
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

  // Surface.
  output_variable (snow_storage, log);
  output_variable (snow_in, log);
  output_variable (snow_out, log);
  output_variable (canopy_storage, log);
  output_variable (canopy_in, log);
  output_variable (canopy_dissipate, log);
  output_variable (canopy_harvest, log);
  output_variable (canopy_out, log);
  output_variable (surface_storage, log);
  output_variable (surface_in, log);
  output_variable (surface_runoff, log);
  output_variable (surface_decompose, log);
  output_variable (surface_mixture, log);
  output_variable (surface_out, log);
  output_value (snow_storage + canopy_storage + surface_storage,
		"top_storage", log);
  output_value (canopy_dissipate + canopy_harvest + surface_runoff 
                + surface_decompose,
		"top_loss", log);
  output_value (C_avg_, "C", log);
  output_value (C_mobile_, "C_mobile", log);
  output_value (C_immobile_, "C_immobile", log);
  output_value (M_total_, "M", log);
  output_value (M_immobile_, "M_immobile", log);
  output_value (S_mobile_, "S_mobile", log);
  output_value (S_immobile_, "S_immobile", log);
  output_value (S_p_, "S_p", log);
  output_variable (S_drain, log);
  output_variable (S_external, log);
  output_variable (S_permanent, log);
  output_variable (S_root, log);
  output_variable (S_decompose, log);
  output_variable (S_transform, log);
  output_variable (J, log);
  output_variable (J_p, log);
  output_variable (tillage, log);
  output_variable (lag, log);
}

bool 
ChemicalStandard::check (const Geometry& geo, 
			 const Soil& soil, const SoilWater& soil_water,
			 const Chemistry& chemistry,
			 const Scope& scope, Treelog& msg) const
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

  if (!C_below_expr->check_dim (scope, g_per_cm3, msg))
    ok = false;

  const bool solid = adsorption_->full ();

  for (size_t i = 0; i < cell_size; i++)
    {
      try 
	{   
	  const double Theta_immobile = soil_water.Theta_immobile (i);
	  const double Theta_mobile = soil_water.Theta_mobile (i);
	  const double M = M_total_[i];
	  const double M_immobile = M_immobile_[i];
          const double C = C_avg_[i];
          const double C_mobile = C_mobile_[i];
          const double C_immobile = C_immobile_[i];
          
          if (M_immobile > M)
            throw "M_immobile > M";
          switch (soil_water.mobile_solute (i))
            {
            case SoilWater::immobile:
              if (!approximate (C_mobile, C))
                throw "C_mobile should be C for immobile water";
              if (!approximate (C_immobile, C))
                throw "C_immobile should be C for immobile water";
              if (!approximate (M_immobile, M))
                throw "M_immobile should be M for immobile water";
              if (!approximate (adsorption_->M_to_C (soil, Theta_immobile, i,
                                                     M_immobile), C_immobile))
                throw "\
Solute C_immobile does not match M_immobile for immobile water";
              break;
            case SoilWater::mobile:
              if (!approximate (C_mobile, C))
                throw "C_mobile should be C for mobile water";
              if (!approximate (C_immobile, C_mobile))
                throw "C_immobile should be C_mobile for mobile water";
              if (!approximate (M_immobile, M))
                throw "M_immobile should be M for mobile water";
              break;
            case SoilWater::mixed:
              if (!approximate (M_immobile, M - C_mobile * Theta_mobile))
                throw 
                  "M_immobile should be M - C_mobile * Theta for mixed water"; 
              if (!approximate (adsorption_->M_to_C (soil, Theta_immobile, i, 
                                                     M_immobile),
                                C_immobile))
                throw "\
Solute C_immobile does not match M_immobile for mixed water";
              break;
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
ChemicalStandard::fillup(std::vector<double>& v, const size_t size)
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
ChemicalStandard::initialize (const AttributeList& al,
                              const Geometry& geo,
                              const Soil& soil, const SoilWater& soil_water, 
			      const SoilHeat& soil_heat,
                              Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  C_below_expr->initialize (msg);

  std::vector<double> Ms;
  geo.initialize_layer (C_avg_, al, "C", msg);
  geo.initialize_layer (C_mobile_, al, "C_mobile", msg);
  geo.initialize_layer (M_total_, al, "M", msg);
  geo.initialize_layer (Ms, al, "Ms", msg);

  fillup (C_avg_, cell_size);
  fillup (C_mobile_, cell_size);
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
	  initial_expr->initialize (msg);
	  ScopeSoil scope (soil, soil_water, soil_heat);
	  for (size_t c = 0; c < soil.size (); c++)
	    { 
	      scope.set_cell (c);
	      double value = 0.0;
	      if (!initial_expr->tick_value (value, g_per_cm3, scope, msg))
		msg.error ("Could not evaluate 'inital_expr'");
	      M_total_.push_back (value);
	    }
	}
    }

  for (size_t i = 0; i < cell_size; i++)
    {
      const double Theta_mobile = soil_water.Theta_mobile (i);
      const double Theta_immobile = soil_water.Theta_immobile (i);
      const double Theta = soil_water.Theta (i);
      daisy_assert (approximate (Theta, Theta_mobile + Theta_immobile));
      const bool has_C_mobile = C_mobile_.size () > i;
      const bool has_C_avg  = C_avg_.size () > i;
      const bool has_M_total  = M_total_.size () > i;
      daisy_assert (has_C_avg || has_M_total);

      switch (soil_water.mobile_solute (i))
        {
        case SoilWater::immobile:
          daisy_assert (iszero (Theta_mobile));
          if (!has_C_avg)
            C_avg_.push_back (adsorption_->M_to_C (soil, Theta, i, 
                                                   M_total_[i]));
          if (!has_M_total) 
            M_total_.push_back (adsorption_->C_to_M (soil, Theta, i,
                                                     C_avg_[i])); 
          if (!has_C_mobile)
            C_mobile_.push_back (C_avg_[i]);
          C_immobile_.push_back (C_avg_[i]);
          M_immobile_.push_back (M_total_[i]);
          break;
        case SoilWater::mobile:
          daisy_assert (iszero (Theta_immobile));
          if (!has_C_avg)
            C_avg_.push_back (adsorption_->M_to_C (soil, Theta, i,
                                                   M_total_[i]));
          if (!has_M_total) 
            M_total_.push_back (adsorption_->C_to_M (soil, Theta, i, 
                                                     C_avg_[i])); 
          if (!has_C_mobile)
            C_mobile_.push_back (C_avg_[i]);
          C_immobile_.push_back (C_avg_[i]);
          M_immobile_.push_back (M_total_[i]);
          break;
        case SoilWater::mixed:
          if (has_C_mobile)
            {
              if (has_C_avg)
                {
                  // Theta * C_a = Theta_i * C_i + Theta_m * C_m
                  // => C_i = (Theta * C_a - Theta_m * C_m) / Theta_i
                  C_immobile_.push_back ((Theta * C_avg_[i]
                                          - Theta_mobile * C_mobile_[i])
                                         / Theta_immobile);
                  M_immobile_.push_back (adsorption_->C_to_M (soil, 
                                                              Theta_immobile, 
                                                              i, 
                                                              C_immobile_[i]));
                  M_total_.push_back (M_immobile_[i] 
                                      + Theta_mobile * C_mobile_[i]);
                }
              else
                {
                  daisy_assert (has_M_total);
                  M_immobile_.push_back (M_total_[i]
                                         - C_mobile_[i] * Theta_mobile);
                  C_immobile_.push_back (adsorption_->M_to_C (soil, 
                                                              Theta_immobile,
                                                              i,
                                                              M_immobile_[i]));
                  C_avg_.push_back ((C_mobile_[i] * Theta_mobile
                                     + C_immobile_[i] * Theta_immobile)
                                    / Theta);
                }
            }
          else
            {
              if (!has_C_avg)
                C_avg_.push_back (adsorption_->M_to_C (soil, Theta, i,
                                                      M_total_[i]));
              if (!has_M_total) 
                M_total_.push_back (adsorption_->C_to_M (soil, Theta, i, 
                                                         C_avg_[i])); 
              C_mobile_.push_back (C_avg_[i]);
              C_immobile_.push_back (C_avg_[i]);
              M_immobile_.push_back (M_total_[i] 
                                     - C_mobile_[i] * Theta_mobile);
            }
          break;
        }
    }

  daisy_assert (C_mobile_.size () == cell_size);
  daisy_assert (C_immobile_.size () == cell_size);
  daisy_assert (M_immobile_.size () == cell_size);
  
  S_mobile_.insert (S_mobile_.begin (), cell_size, 0.0);
  S_immobile_.insert (S_immobile_.begin (), cell_size, 0.0);
  S_p_.insert (S_p_.begin (), cell_size, 0.0);
  S_drain.insert (S_drain.begin (), cell_size, 0.0);
  S_external.insert (S_external.begin (), cell_size, 0.0);
  if (S_permanent.size () < cell_size)
    S_permanent.insert (S_permanent.end (), 
			cell_size - S_permanent.size (),
			0.0);
  S_root.insert (S_root.begin (), cell_size, 0.0);
  S_decompose.insert (S_decompose.begin (), cell_size, 0.0);
  S_transform.insert (S_transform.begin (), cell_size, 0.0);
  J.insert (J.begin (), edge_size, 0.0);
  J_p.insert (J_p.begin (), edge_size, 0.0);
  tillage.insert (tillage.begin (), cell_size, 0.0);
  lag.insert (lag.end (), cell_size - lag.size (), 0.0);
}

double
ChemicalStandard::find_surface_decompose_rate (Block& al)
{
  if (al.check ("surface_decompose_rate"))
    return al.number ("surface_decompose_rate");
  if (al.check ("surface_decompose_halftime"))
    return halftime_to_rate (al.number ("surface_decompose_halftime"));
  if (al.check ("decompose_rate"))
    return al.number ("decompose_rate");
  
  return halftime_to_rate (al.number ("decompose_halftime"));
}

ChemicalStandard::ChemicalStandard (Block& al)
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
    snow_storage (al.number ("snow_storage")),
    snow_in (0.0),
    snow_out (0.0),
    canopy_storage (al.number ("canopy_storage")),
    canopy_in (0.0),
    canopy_dissipate (0.0),
    canopy_harvest (0.0),
    canopy_out (0.0),
    surface_storage (al.number ("surface_storage")),
    surface_in (0.0),
    surface_out (0.0),
    surface_mixture (0.0),
    surface_runoff (0.0),
    surface_decompose (0.0),
    S_permanent (al.number_sequence ("S_permanent")),
    lag (al.check ("lag")
	 ? al.number_sequence ("lag")
	 : std::vector<double> ())
{ }

static struct ChemicalStandardSyntax
{
  static Model& make (Block& al);
  static bool check_alist (const AttributeList& al, Treelog& err);

  static void load_syntax (Syntax& syntax, AttributeList& alist);
  static void load_nutrient (Syntax& syntax, AttributeList& alist);
  static void initial_C (const double value, AttributeList& alist);
  static void load_NO3 (Syntax& syntax, AttributeList& alist);
  static void load_NH4 (Syntax& syntax, AttributeList& alist);

  static void build_default ();
  static void build_nutrient ();
  static void build_NO3 ();
  static void build_NH4 ();

  ChemicalStandardSyntax ()
  {
    build_default ();
    build_nutrient ();
    build_NO3 ();
    build_NH4 ();
  }
} ChemicalStandard_syntax;

Model& 
ChemicalStandardSyntax::make (Block& al)
{ return *new ChemicalStandard (al); }

bool 
ChemicalStandardSyntax::check_alist (const AttributeList& al, Treelog& msg)
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

void
ChemicalStandardSyntax::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
    
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this parameterization."); 
  alist.add ("description", "\
Read chemical properties as normal Daisy parameters.");

  // Surface parameters.
  syntax.add_fraction ("crop_uptake_reflection_factor", Syntax::Const, "\
How much of the chemical is reflected at crop uptake.");
  alist.add ("crop_uptake_reflection_factor", 1.0);
  syntax.add ("canopy_dissipation_rate", "h^-1", 
	      Check::fraction (), Syntax::OptionalConst,
	      "How fast does the chemical dissipate on canopy.\n\
You must specify it with either 'canopy_dissipation_halftime' or\n\
'canopy_dissipation_rate'.");
  syntax.add ("canopy_dissipation_halftime", "h", 
	      Check::positive (), Syntax::OptionalConst,
	      "How fast does the chemical dissipate on canopy.\n\
You must specify it with either 'canopy_dissipation_halftime' or\n\
'canopy_dissipation_rate'.");
  syntax.add ("canopy_dissipation_rate_coefficient", "h^-1", 
	      Check::fraction (), Syntax::OptionalConst,
	      "Obsolete alias for 'canopy_dissipation_rate'.");
  syntax.add_fraction ("canopy_washoff_coefficient", Syntax::Const, "\
Fraction of the chemical that follows the water off the canopy.");
  syntax.add ("surface_decompose_rate", "h^-1", 
	      Check::fraction (), Syntax::OptionalConst,
	      "How fast does the chemical decomposee on surface.\n\
You must specify it with either 'surface_decompose_halftime' or\n\
'surface_decompose_rate'.  If neither is specified, 'decompose_rate' is used.");
  syntax.add ("surface_decompose_halftime", "h", 
	      Check::positive (), Syntax::OptionalConst,
	      "How fast does the chemical decompose on surface.\n\
You must specify it with either 'surface_decompose_halftime' or\n\
'surface_decompose_rate'.  If neither is specified, 'decompose_rate' is used.");

  // Soil parameters.
  syntax.add ("diffusion_coefficient", "cm^2/s", Check::non_negative (),
	      Syntax::Const, "Diffusion coefficient.");
  syntax.add ("decompose_rate", "h^-1", Check::fraction (),
	      Syntax::OptionalConst,
	      "How fast the chemical is being decomposed in the soil.\n\
You must specify it with either 'decompose_rate' or 'decompose_halftime'.");
  syntax.add ("decompose_halftime", "h", Check::positive (),
	      Syntax::OptionalConst,
	      "How fast the chemical is being decomposed in the soil.\n\
You must specify it with either 'decompose_rate' or 'decompose_halftime'.");
  syntax.add ("decompose_heat_factor", "dg C", Syntax::None (),
	      Syntax::Const, "Heat factor on decomposition.");
  alist.add ("decompose_heat_factor", PLF::empty ());
  syntax.add ("decompose_water_factor", "cm", Syntax::None (),
	      Syntax::Const,
	      "Water potential factor on decomposition.");
  alist.add ("decompose_water_factor", PLF::empty ());
  syntax.add ("decompose_CO2_factor", "g CO2-C/cm^3", Syntax::None (),
	      Syntax::Const,
	      "CO2 development factor on decomposition.");
  PLF no_factor;
  no_factor.add (0.0, 1.0);
  no_factor.add (1.0, 1.0);
  alist.add ("decompose_CO2_factor", no_factor);
  syntax.add ("decompose_conc_factor", "g/cm^3 H2O", Syntax::None (),
	      Syntax::Const,
	      "Concentration development factor on decomposition.");
  alist.add ("decompose_conc_factor", no_factor);
  syntax.add ("decompose_depth_factor", "cm", Syntax::None (),
	      Syntax::Const,
	      "Depth influence on decomposition.");
  alist.add ("decompose_depth_factor", no_factor);
  syntax.add ("decompose_lag_increment", 
	      "g/cm^3/h", Syntax::Fraction (), Syntax::Const,
	      "Increment lag with the value of this PLF for the current\n\
concentration each hour.  When lag in any cell reaches 1.0,\n\
decomposition begins.  It can never be more than 1.0 or less than 0.0.");
  alist.add ("decompose_lag_increment", no_factor);
  syntax.add_object ("C_below", Number::component, 
		     Syntax::Const, Syntax::Singleton, "\
Concentration below the layer of soil being examined.\n\
Use a negative number to indicate same concentration as in lowest cell.");
  AttributeList minus_one;
  minus_one.add ("value", -1.0, "g/cm^3");
  minus_one.add ("type", "const");
  syntax.add_submodule_sequence ("decompose_products", Syntax::Const, "\
List of products from decomposition.", ChemicalStandard::Product::load_syntax);
  alist.add ("decompose_products", std::vector<const AttributeList*> ());
  alist.add ("C_below", minus_one);
  syntax.add_object ("initial", Number::component, 
		     Syntax::Const, Syntax::Singleton, "\
Initial content if otherwise unspecified. [g/cm^3]");
  AttributeList zero;
  zero.add ("value", 0.0, "g/cm^3");
  zero.add ("type", "const");
  alist.add ("initial", zero);
  syntax.add_object ("adsorption", Adsorption::component, 
		     Syntax::Const, Syntax::Singleton, "\
Instant equilibrium between sorbed and solute phases.\n\
\n\
Specify the equilibrium model here for chemicals where the sorbed and\n\
solute phases typically reaches equilibrium within a single timestep.\n\
Slower adsorption processes should be modelled as two chemicals, one\n\
with 'none' adsorption and one with 'full' adsorption, and an\n\
'adsorption' reaction between them.");
  alist.add ("adsorption", Adsorption::none_model ());

  // Management and climate fluxes.
  syntax.add ("deposit", "g/m^2/h", Syntax::LogOnly,
	      "Amount deposited from the atmosphere.");
  syntax.add ("spray", "g/m^2/h", Syntax::LogOnly,
	      "Amount currently being applied.");
  syntax.add ("surface_tillage", "g/m^2/h", Syntax::LogOnly, 
	      "Amount removed from surface due to tillage operations.");
    
  // Surface variables.
  syntax.add ("snow_storage", "g/m^2", Syntax::State, 
	      "Stored in the snow pack.");
  alist.add ("snow_storage", 0.0);
  syntax.add ("snow_in", "g/m^2/h", Syntax::LogOnly, 
	      "Entering snow pack..");
  syntax.add ("snow_out", "g/m^2/h", Syntax::LogOnly, 
	      "Leaking from snow pack.");

  syntax.add ("canopy_storage", "g/m^2", Syntax::State, 
	      "Stored on the canopy.");
  alist.add ("canopy_storage", 0.0);
  syntax.add ("canopy_in", "g/m^2/h", Syntax::LogOnly, 
	      "Entering canopy.");
  syntax.add ("canopy_dissipate", "g/m^2/h", Syntax::LogOnly, 
	      "Dissipating from canopy.");
  syntax.add ("canopy_out", "g/m^2/h", Syntax::LogOnly, 
	      "Falling through or off the canopy.");
  syntax.add ("canopy_harvest", "g/m^2/h", Syntax::LogOnly, 
	      "Amount removed with crop harvest.");

  syntax.add ("surface_storage", "g/m^2", Syntax::State, 
	      "Stored on the soil surface.");
  alist.add ("surface_storage", 0.0);
  syntax.add ("surface_in", "g/m^2/h", Syntax::LogOnly, 
	      "Falling on the bare soil surface.");
  syntax.add ("surface_runoff", "g/m^2/h", Syntax::LogOnly, 
	      "Removed through lateral movement on the soil.");
  syntax.add ("surface_decompose", "g/m^2/h", Syntax::LogOnly, 
	      "Decomposed from the surface.");
  syntax.add ("surface_mixture", "g/m^2/h", Syntax::LogOnly, 
	      "Entering the soil through mixture with ponded water.");
  syntax.add ("surface_out", "g/m^2/h", Syntax::LogOnly, 
	      "Entering the soil with water infiltration.");

  syntax.add ("top_storage", "g/m^2", Syntax::LogOnly, 
	      "Som of above ground (surface, snow, canopy) storage.");
  syntax.add ("top_loss", "g/m^2/h", Syntax::LogOnly, "\
Amount lost from the system from the surface.\n\
This includes runoff, canopy dissipation and harvest, but not soil\n\
infiltration..");

  // Soil variables.
  Geometry::add_layer (syntax, Syntax::OptionalState, "C", "g/cm^3",
		       "Concentration in water.");
  Geometry::add_layer (syntax, Syntax::OptionalState, "C_mobile", "g/cm^3",
		       "Concentration in mobile water.");
  Geometry::add_layer (syntax, Syntax::LogOnly, "C_immobile", "g/cm^3",
		       "Concentration in immobile water.");
  Geometry::add_layer (syntax, Syntax::OptionalState, "M", "g/cm^3", 
		       "Total mass per volume water, soil, and air.");
  Geometry::add_layer (syntax, Syntax::LogOnly, "M_immobile", "g/cm^3", 
		       "Immobile mass per volume water, soil, and air.");
  Geometry::add_layer (syntax, Syntax::OptionalConst,
		       "Ms", Syntax::Fraction (), "Mass in dry soil.\n\
This include all matter in both soil and water, relative to the\n\
dry matter weight.\n\
Only for initialization of the 'M' parameter.");
  syntax.add ("S_mobile", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Mobile source term.");
  syntax.add ("S_immobile", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Immobile source term.");
  syntax.add ("S_p", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (macropore transport only).");
  syntax.add ("S_drain", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (soil drainage only).");
  syntax.add ("S_external", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "External source, such as incorporated fertilizer.");
  syntax.add ("S_permanent", "g/cm^3/h", Syntax::State, Syntax::Sequence,
	      "Permanent external source, e.g. subsoil irrigation.");
  std::vector<double> empty;
  alist.add ("S_permanent", empty);
  syntax.add ("S_root", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (root uptake only, always negative).");
  syntax.add ("S_decompose", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term for decompose, is never positive.");
  syntax.add ("S_transform", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term for transformations other than sorption.");
  syntax.add ("J", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in matrix (positive up).");
  syntax.add ("J_p", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in macropores (positive up).");
  syntax.add ("tillage", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Changes during tillage.");
  syntax.add ("lag", Syntax::None (), Syntax::OptionalState, Syntax::Sequence,
	      "This state variable grows with lag_increment (C) each hour.\n\
When it reached 1.0, decomposition begins.");
}

void
ChemicalStandardSyntax::load_nutrient (Syntax& syntax, AttributeList& alist)
{ 
  load_syntax (syntax, alist);
  alist.add ("description", "Plants eat this stuff.");
  alist.add ("crop_uptake_reflection_factor", 1.0); // Specific uptake code.
  alist.add ("canopy_dissipation_rate", 0.0);
  alist.add ("canopy_washoff_coefficient", 1.0);
  alist.add ("decompose_rate", 0.0);
}

void
ChemicalStandardSyntax::initial_C (const double value, AttributeList& alist)
{
  AttributeList initial;
  initial.add ("type", "*");
  std::vector<const AttributeList*> operands;
  AttributeList factor;
  factor.add ("type", "const");
  factor.add ("value", value, "g/cm^3");
  operands.push_back (&factor);
  AttributeList Theta;
  Theta.add ("type", "get");
  Theta.add ("name", "Theta");
  Theta.add ("dimension", Syntax::none ());
  operands.push_back (&Theta);
  initial.add ("operands", operands);
  alist.add ("initial", initial);
}

void
ChemicalStandardSyntax::load_NO3 (Syntax& syntax, AttributeList& alist)
{
  load_nutrient (syntax, alist);
  alist.add ("description", "Nitrate-N.");
  alist.add ("diffusion_coefficient", 2.0e-5);
  // We initialize to approximatey half the allowed content in
  // drinking water [ 0.5 * 100 mg NO3/l ~= 5.0e-6 g NO3-N/cm^3 ]
  initial_C (5e-6, alist);
}

void
ChemicalStandardSyntax::load_NH4 (Syntax& syntax, AttributeList& alist)
{ 
  load_nutrient (syntax, alist);
  alist.add ("description", "Ammonium-N.");
  AttributeList linear;
  linear.add ("type", "linear");
  linear.add ("K_clay", 117.116);
  linear.add ("K_OC", 117.116);
  alist.add ("adsorption", linear);
  alist.add ("diffusion_coefficient", 1.8e-5);
  // We initialize to approximatey 5% of the N corresponding to the
  // allowed content of NO3 in drinking water.
  // [ 0.05 * 100 mg/l = 0.5e-6 g/cm^3 ]
  initial_C (0.5e-6, alist);
}

void
ChemicalStandardSyntax::build_default ()
{
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  ChemicalStandardSyntax::load_syntax (syntax, alist);
  Librarian::add_type (Chemical::component, "default", alist, syntax, &make);
}

void
ChemicalStandardSyntax::build_nutrient ()
{
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  ChemicalStandardSyntax::load_nutrient (syntax, alist);
  alist.add ("type", "default");
  Librarian::add_type (Chemical::component, "nutrient", alist, syntax, &make);
}

void
ChemicalStandardSyntax::build_NO3 ()
{
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  ChemicalStandardSyntax::load_NO3 (syntax, alist);
  alist.add ("type", "nutrient");
  Librarian::add_type (Chemical::component, "NO3", alist, syntax, &make);
}

void
ChemicalStandardSyntax::build_NH4 ()
{
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  ChemicalStandardSyntax::load_NH4 (syntax, alist);
  alist.add ("type", "nutrient");
  Librarian::add_type (Chemical::component, "NH4", alist, syntax, &make);
}

const AttributeList& 
Chemical::NO3_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ChemicalStandardSyntax::load_NO3 (dummy, alist);
      alist.add ("type", "NO3");
    }
  return alist;
}

const AttributeList& 
Chemical::NH4_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ChemicalStandardSyntax::load_NH4 (dummy, alist);
      alist.add ("type", "NH4");
    }
  return alist;
}

// chemical_std.C ends here.
