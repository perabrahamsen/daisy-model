// soil_water.C
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

#define BUILD_DLL

#include "soil_water.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "groundwater.h"
#include "log.h"
#include "submodel.h"
#include "block.h"
#include "secondary.h"
#include <sstream>

void
SoilWater::clear ()
{
  fill (S_sum_.begin (), S_sum_.end (), 0.0);
  // S_drain unnecessary?
  fill (S_drain_.begin (), S_drain_.end (), 0.0);
  fill (S_root_.begin (), S_root_.end (), 0.0);
  fill (S_incorp_.begin (), S_incorp_.end (), 0.0);
  fill (tillage_.begin (), tillage_.end (), 0.0);
  fill (S_ice_.begin (), S_ice_.end (), 0.0);
}

void 
SoilWater::freeze (const Soil&, const size_t c, const double rate /* [h^-1] */)
{
  static const double rho_water = 1.0; // [g/cm^3]
  static const double rho_ice = 0.917; // [g/cm^3]

  daisy_assert (c < S_ice_.size ());
  daisy_assert (c < S_sum_.size () );
  S_sum_[c] += rate;
  S_ice_[c] -= rate * rho_water / rho_ice;
}

void
SoilWater::drain (const std::vector<double>& v)
{
  daisy_assert (S_sum_.size () == v.size ());
  daisy_assert (S_root_.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_sum_[i] += v[i];
      S_drain_[i] += v[i];
    }
}

void
SoilWater::root_uptake (const std::vector<double>& v)
{
  daisy_assert (S_sum_.size () == v.size ());
  daisy_assert (S_root_.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_sum_[i] += v[i];
      S_root_[i] += v[i];
    }
}

double 
SoilWater::content_surface (const Geometry& geo, 
                            const double from, const double to) const
{ return geo.total_surface (Theta_, from, to); }

double
SoilWater::Theta_ice (const Soil& soil, const size_t i, const double h) const
{ return soil.Theta (i, h, h_ice (i)); }
 
void 
SoilWater::set_content (const size_t i, const double h, const double Theta)
{
  daisy_assert (i < h_.size ());
  h_[i] = h;
  daisy_assert (i < Theta_.size ());
  Theta_[i] = Theta;
}
 
void
SoilWater::set_flux (const size_t i, const double q)
{
  daisy_assert (i < q_matrix_.size ());
  q_matrix_[i] = q;
}

void 
SoilWater::set_matrix (const std::vector<double>& h,
                       const std::vector<double>& Theta,
                       const std::vector<double>& q)
{
  daisy_assert (h_.size () == h.size ());
  h_ = h;
  daisy_assert (Theta_.size () == Theta.size ());
  Theta_ = Theta;
  daisy_assert (q_matrix_.size () == q.size ());
  q_matrix_ = q;
}

void 
SoilWater::set_tertiary (const std::vector<double>& S_p, 
                         const std::vector<double>& q_p)
{
  const size_t cell_size = S_p.size ();
  daisy_assert (S_sum_.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    S_sum_[c] += S_p[c];
  S_p_ = S_p;
  q_tertiary_ = q_p;
}

void
SoilWater::tick (const size_t cell_size, const Soil& soil, 
                 const double dt, Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater");

  // Ice first.
  for (size_t i = 0; i < cell_size; i++)
    {
      X_ice_[i] -= S_ice_[i];
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);

      // Move extra ice to buffer.
      const double available_space
	= Theta_sat - Theta_[i] - X_ice_[i] + S_sum_[i] * dt;
      if (available_space < 0.0)
	{
	  X_ice_[i] += available_space;
	  X_ice_buffer_[i] -= available_space;
	}
      else if (X_ice_buffer_[i] > 0.0)
	{
	  if (X_ice_buffer_[i] < available_space)
	    { 
	      X_ice_[i] += X_ice_buffer_[i];
	      X_ice_buffer_[i] = 0.0;
	    }
	  else
	    {
	      X_ice_[i] += available_space;
	      X_ice_buffer_[i] -= available_space;
            }
	}

      if (X_ice_[i] < 0.0)
	{
	  if (X_ice_[i] < -1e-13)
	    {
	      std::ostringstream tmp;
	      tmp << "X_ice[" << i << "] = " << X_ice_[i]
                  << " (S_sum[i] = " << S_sum_[i] << ")";
	      msg.error (tmp.str ());
	    }
          X_ice_buffer_[i] += X_ice_[i];
	  X_ice_[i] = 0.0;
	}

      // Update ice pressure.
      h_ice_[i] = soil.h (i, Theta_sat - X_ice_[i]);
    }

  // External sink.
  for (size_t i = 0; i < cell_size; i++)
    {
      S_incorp_[i] += S_permanent_[i];
      S_sum_[i] += S_incorp_[i];
    }

  // Remember old values.
  h_old_ = h_;
  Theta_old_ = Theta_;
  Theta_primary_old_ = Theta_primary_;
  Theta_secondary_old_ = Theta_secondary_;
}

void
SoilWater::tick_after (const Geometry& geo,
                       const Soil& soil, const SoilHeat& soil_heat, 
                       const bool initial,
                       Treelog&)
{
  // We need old K for primary/secondary flux division.
  std::vector<double> K_old = K_;

  // Update cells.
  const size_t cell_size = geo.cell_size ();
  daisy_assert (K_.size () == cell_size);
  daisy_assert (h_.size () == cell_size);
  daisy_assert (h_ice_.size () == cell_size);
  daisy_assert (K_old.size () == cell_size);
  daisy_assert (Theta_.size () == cell_size);
  daisy_assert (Theta_primary_.size () == cell_size);
  daisy_assert (Theta_secondary_.size () == cell_size);

  for (size_t c = 0; c < cell_size; c++)
    {
      K_[c] = soil.K (c, h_[c], h_ice_[c], soil_heat.T (c));
      
      const Secondary& secondary = soil.secondary_domain (c);
      if (secondary.none ())
        {
          // Only one domain in this horizon.
          Theta_primary_[c] = Theta_[c];
          Theta_secondary_[c] = 0.0;
        }
      else  
        {
          // Two matrix domains.
          const double h_lim = secondary.h_lim ();
          if (h_[c] <= h_lim)
            {
              // Sedondary domain not activated.
              Theta_primary_[c] = Theta_[c];
              Theta_secondary_[c] = 0.0;
            }
          else 
            {
              // Secondary domain activated.
              const double Theta_lim = soil.Theta (c, h_lim, h_ice_[c]);
              Theta_primary_[c] = Theta_lim;
              Theta_secondary_[c] = Theta_[c] - Theta_lim;
            }
        }
    }

  // Initialize
  if (initial)
    {
      K_old = K_;
      Theta_primary_old_ = Theta_primary_;
      Theta_secondary_old_ = Theta_secondary_;
    }
  daisy_assert (K_old.size () == cell_size);
  daisy_assert (Theta_primary_old_.size () == cell_size);
  daisy_assert (Theta_secondary_old_.size () == cell_size);

  // Update edges.
  const size_t edge_size = geo.edge_size ();
  daisy_assert (q_matrix_.size () == edge_size);
  daisy_assert (q_primary_.size () == edge_size);
  daisy_assert (q_secondary_.size () == edge_size);
  daisy_assert (q_matrix_.size () == edge_size);
  daisy_assert (q_matrix_.size () == edge_size);
  for (size_t e = 0; e < edge_size; e++)
    {
      // By default, all flux is in primary domain.
      q_primary_[e] = q_matrix_[e];
      q_secondary_[e] = 0.0;
      
      // Find average K.
      double K_edge = 0.0;
      double K_lim = 0.0;

      // Contributions from target.
      const int to = geo.edge_to (e);
      if (geo.cell_is_internal (to))
        {
          
          if (iszero (Theta_secondary_old (to)) || 
              iszero (Theta_secondary (to)))
            continue;
          K_edge += 0.5 * (K_old[to] + K (to));

          const Secondary& secondary = soil.secondary_domain (to);
          daisy_assert (!secondary.none ());
          const double h_lim = secondary.h_lim ();
          K_lim += soil.K (to, h_lim, h_ice (to), soil_heat.T (to));
        }
      
      // Contributions from source.
      const int from = geo.edge_from (e);
      if (geo.cell_is_internal (from))
        {
          
          if (iszero (Theta_secondary_old (from)) || 
              iszero (Theta_secondary (from)))
            continue;
          K_edge += 0.5 * (K_old[from] + K (from));

          const Secondary& secondary = soil.secondary_domain (from);
          daisy_assert (!secondary.none ());
          const double h_lim = secondary.h_lim ();
          K_lim += soil.K (from, h_lim, h_ice (from), soil_heat.T (from));
        }
      
      daisy_assert (K_lim > 0.0);
      daisy_assert (K_edge > 0.0);
      
      // BUGLET:  We use in effect arithmetic average here for K.
      // This may not have been what was used for calculating q matrix.
      const double K_factor = K_lim / K_edge;
      if (K_factor < 0.99999)
        {
          q_primary_[e] = q_matrix_[e] * K_factor;
          q_secondary_[e] = q_matrix_[e] - q_primary_[e];
        }
    }
}

void 
SoilWater::incorporate (const Geometry& geo, const double amount,
                        const double from, const double to)

{ geo.add_surface (S_incorp_, from, to, -amount); }

void 
SoilWater::incorporate (const Geometry& geo, const double amount,
                        const Volume& volume)

{ geo.add_surface (S_incorp_, volume, -amount); }

void
SoilWater::mix (const Geometry& geo, const Soil& soil, 
                const SoilHeat& soil_heat, const double from, 
                const double to, const double dt, Treelog& msg)
{
  geo.mix (Theta_, from, to, tillage_, dt);
  for (size_t i = 0; i < soil.size(); i++)
    h_[i] = soil.h (i, Theta_[i]);
  
  tick_after (geo, soil,  soil_heat, false, msg);
}

void
SoilWater::swap (const Geometry& geo, const Soil& soil, 
                 const SoilHeat& soil_heat, const double from, 
                 const double middle, const double to,
                 const double dt, Treelog& msg)
{
  geo.swap (Theta_, from, middle, to, tillage_, dt);

  for (size_t i = 0; i < soil.size(); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      if (Theta_[i] > Theta_sat)
	{
	  std::ostringstream tmp;
	  tmp << "BUG: Theta[ " << i << "] (" << Theta_[i]
              << ") > Theta_sat (" << Theta_sat << ")";
	  msg.error (tmp.str ());
	  Theta_[i] = Theta_sat;
	}
      h_[i] = soil.h (i, Theta_[i]);
    }
  tick_after (geo, soil,  soil_heat, false, msg);
}

void 
SoilWater::output (Log& log) const
{
  output_value (h_, "h", log);
  output_value (Theta_, "Theta", log);
  output_value (Theta_primary_, "Theta_primary", log);
  output_value (Theta_secondary_, "Theta_secondary", log);
  output_value (S_sum_, "S_sum", log);
  output_value (S_root_, "S_root", log);
  output_value (S_drain_, "S_drain", log);
  output_value (S_incorp_, "S_incorp", log);
  output_value (tillage_, "tillage", log);
  output_value (S_p_, "S_p", log);
  output_value (S_permanent_, "S_permanent", log);
  output_value (S_ice_, "S_ice", log);
  output_value (X_ice_, "X_ice", log);
  output_value (X_ice_buffer_, "X_ice_buffer", log);
  output_value (h_ice_, "h_ice", log);
  output_value (q_matrix_, "q", log);
  output_value (q_tertiary_, "q_p", log);
  output_value (K_, "K", log);
}

double
SoilWater::MaxExfiltration (const Geometry& geo,
                            const Soil& soil, const double T) const
{
  const size_t edge_size = geo.edge_size ();
  
  double total_area = 0.0;
  double sum = 0.0;

  for (size_t e = 0; e < edge_size; e++)
    if (geo.edge_to (e) == Geometry::cell_above)
      {
        const size_t n = geo.edge_from (e);
        const double area = geo.edge_area (e);
        total_area += area;
        sum += (soil.K (n, h (n), h_ice (n), T) / soil.Cw2 (n, h (n))) 
          * ((Theta (n) - soil.Theta_res (n)) / geo.z (n))
          * area;
      }
  daisy_assert (approximate (total_area, geo.surface_area ()));
  return - sum / total_area;
}

double
SoilWater::MaxExfiltration (const Geometry& geo, const size_t edge,
                            const Soil& soil, const double T) const
{
  const size_t n = geo.edge_other (edge, Geometry::cell_above);
  return - (soil.K (n, h (n), h_ice (n), T) / soil.Cw2 (n, h (n))) 
    * ((Theta (n) - soil.Theta_res (n)) / geo.z (n));
}

double
SoilWater::infiltration (const Geometry& geo) const
{
  const size_t edge_size = geo.edge_size ();
  
  double sum = 0.0;

  for (size_t e = 0; e < edge_size; e++)
    if (geo.edge_to (e) == Geometry::cell_above)
      sum -= q_matrix (e) * geo.edge_area (e);
  
  const double mm_per_cm = 10.0;
  return mm_per_cm * sum / geo.surface_area ();
}

bool 
SoilWater::check (const size_t n, Treelog& msg) const
{
  bool ok = true;

  if (Theta_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << Theta_.size () << " Theta values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (h_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << h_.size () << " h values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (X_ice_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << X_ice_.size () << " X_ice values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (X_ice_buffer_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << X_ice_buffer_.size () 
          << " X_ice_buffer values";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void
SoilWater::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "SoilWater");

  Geometry::add_layer (syntax, Syntax::OptionalState, 
                       "h", "cm", "Soil water pressure.");
  Geometry::add_layer (syntax, Syntax::OptionalState,
                       "Theta", Syntax::Fraction (),
                       "Soil water content.");
  syntax.add ("Theta_primary", "cm^3/cm^3", Syntax::LogOnly, 
              "Water content in primary matrix system.\n\
Conventionally, this is the intra-aggregate pores.");
  syntax.add ("Theta_secondary", "cm^3/cm^3", Syntax::LogOnly, 
              "Water content in secondary matrix system.\n\
Conventionally, this is the inter-aggregate pores.");
  syntax.add ("S_sum", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Total water sink (due to root uptake and macropores).");
  syntax.add ("S_root", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to root uptake.");
  syntax.add ("S_drain", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to soil drainage.");
  syntax.add ("S_incorp", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Incorporated water sink, typically from subsoil irrigation.");
  syntax.add ("tillage", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Changes in water content due to tillage operations.");
  syntax.add ("S_p", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink (due to macropores).");
  syntax.add ("S_permanent", "cm^3/cm^3/h", Syntax::State, Syntax::Sequence,
	      "Permanent water sink, e.g. subsoil irrigation.");
  alist.add ("S_permanent", std::vector<double> ());
  syntax.add ("S_ice", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Ice sink (due to thawing or freezing).");
  syntax.add_fraction ("X_ice", Syntax::OptionalState, Syntax::Sequence,
		       "Ice volume fraction in soil.");
  syntax.add ("X_ice_buffer", Syntax::None (), 
	      Syntax::OptionalState, Syntax::Sequence,
	      "Ice volume that didn't fit the soil durin freezing.");
  syntax.add ("h_ice", Syntax::None (), Syntax::LogOnly, Syntax::Sequence,
	      "Pressure at which all air is out of the matrix.\n\
When there are no ice, this is 0.0.  When there are ice, the ice is\n\
presummed to occupy the large pores, so it is h (Theta_sat - X_ice).");
  syntax.add ("q", "cm/h", Syntax::LogOnly, Syntax::Sequence,
	      "Matrix water flux (positive numbers mean upward).");  
  syntax.add ("q_p", "cm/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water flux in macro pores (positive numbers mean upward).");
  syntax.add ("K", "cm/h", Syntax::LogOnly, Syntax::Sequence,
	      "Hydraulic conductivity.");
}

void
SoilWater::initialize (const AttributeList& al, const Geometry& geo,
                       const Soil& soil, const SoilHeat& soil_heat,
                       const Groundwater& groundwater, Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater");

  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  // Ice must be first.
  if (al.check ("X_ice"))
    {
      X_ice_ = al.number_sequence ("X_ice");
      if (X_ice_.size () == 0)
	X_ice_.push_back (0.0);
      while (X_ice_.size () < cell_size)
	X_ice_.push_back (X_ice_[X_ice_.size () - 1]);
    }
  else 
    X_ice_.insert (X_ice_.begin (), cell_size, 0.0);

  if (al.check ("X_ice_buffer"))
    {
      X_ice_buffer_ = al.number_sequence ("X_ice_buffer");
      if (X_ice_buffer_.size () == 0)
	X_ice_buffer_.push_back (0.0);
      while (X_ice_buffer_.size () < cell_size)
	X_ice_buffer_.push_back (X_ice_buffer_[X_ice_buffer_.size () - 1]);
    }
  else 
    X_ice_buffer_.insert (X_ice_buffer_.begin (), cell_size, 0.0);

  for (size_t i = 0; i < cell_size; i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      daisy_assert (Theta_sat >= X_ice_[i]);
      h_ice_.push_back (soil.h (i, Theta_sat - X_ice_[i]));
    }
  daisy_assert (h_ice_.size () == cell_size);

  geo.initialize_layer (Theta_, al, "Theta", msg);
  geo.initialize_layer (h_, al, "h", msg);

  for (size_t i = 0; i < Theta_.size () && i < h_.size (); i++)
    {
      const double Theta_h = soil.Theta (i, h_[i], h_ice (i));
      if (!approximate (Theta_[i], Theta_h))
	{
	  std::ostringstream tmp;
	  tmp << "Theta[" << i << "] (" << Theta_[i] << ") != Theta (" 
              << h_[i] << ") (" << Theta_h << ")";
	  msg.error (tmp.str ());
	}
      Theta_[i] = Theta_h;
    }
  if (Theta_.size () > 0)
    {
      while (Theta_.size () < cell_size)
	Theta_.push_back (Theta_[Theta_.size () - 1]);
      if (h_.size () == 0)
	for (size_t i = 0; i < cell_size; i++)
	  h_.push_back (soil.h (i, Theta_[i]));
    }
  if (h_.size () > 0)
    {
      while (h_.size () < cell_size)
	h_.push_back (h_[h_.size () - 1]);
      if (Theta_.size () == 0)
	for (size_t i = 0; i < cell_size; i++)
	  Theta_.push_back (soil.Theta (i, h_[i], h_ice (i)));
    }
  daisy_assert (h_.size () == Theta_.size ());

  // Groundwater based pressure.
  if (h_.size () == 0)
    {
      if (groundwater.table () > 0.0)
	{
	  const double h_pF2 = -100.0; // pF 2.0;
	  for (size_t i = 0; i < cell_size; i++)
	    {
	      h_.push_back (h_pF2);
	      Theta_.push_back (soil.Theta (i, h_pF2, h_ice_[i]));
	    }
	}
      else
	{
	  const double table = groundwater.table ();
	  
	  for (size_t i = 0; i < cell_size; i++)
	    {
	      h_.push_back (std::max (-100.0, table - geo.z (i)));
	      Theta_.push_back (soil.Theta (i, h_[i], h_ice_[i]));
	    }
	}
    }
  daisy_assert (h_.size () == cell_size);

  // Sources.
  S_sum_.insert (S_sum_.begin (), cell_size, 0.0);
  S_root_.insert (S_root_.begin (), cell_size, 0.0);
  S_drain_.insert (S_incorp_.begin (), cell_size, 0.0);
  S_incorp_.insert (S_incorp_.begin (), cell_size, 0.0);
  tillage_.insert (tillage_.begin (), cell_size, 0.0);
  S_p_.insert (S_p_.begin (), cell_size, 0.0);
  if (S_permanent_.size () < cell_size)
    S_permanent_.insert (S_permanent_.end (), cell_size - S_permanent_.size (),
                         0.0);
  S_ice_.insert (S_ice_.begin (), cell_size, 0.0);

  // Fluxes.
  q_primary_.insert (q_primary_.begin (), edge_size, 0.0);
  q_secondary_.insert (q_secondary_.begin (), edge_size, 0.0);
  q_matrix_.insert (q_matrix_.begin (), edge_size, 0.0);
  q_tertiary_.insert (q_tertiary_.begin (), edge_size, 0.0);

  // Update conductivity and primary/secondary water.
  Theta_primary_.insert (Theta_primary_.begin (), cell_size, -42.42e42);
  Theta_secondary_.insert (Theta_secondary_.begin (), cell_size, -42.42e42);
  K_.insert (K_.begin (), cell_size, 0.0);
  tick_after (geo, soil,  soil_heat, true, msg);

  // We just assume no changes.
  h_old_ = h_;
  Theta_old_ = Theta_;
}

SoilWater::SoilWater (Block& al)
  : S_permanent_ (al.number_sequence ("S_permanent"))
{ }

SoilWater::~SoilWater ()
{ }

static Submodel::Register 
soil_water_submodel ("SoilWater", SoilWater::load_syntax);

// soil_water.C ends here.
