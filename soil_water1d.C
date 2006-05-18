// soil_water1d.C
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


#include "soil_water1d.h"
#include "log.h"
#include "alist.h"
#include "uzmodel.h"
#include "geometry1d.h"
#include "soil.h"
#include "surface.h"
#include "groundwater.h"
#include "submodeler.h"
#include "macro.h"
#include "syntax.h"
#include "mathlib.h"
#include "submodel.h"
#include <sstream>
#include <memory>

static const double rho_water = 1.0; // [g/cm^3]
static const double rho_ice = 0.917; // [g/cm^3]

void
SoilWater1D::clear ()
{
  clear_base ();
  fill (S_ice_.begin (), S_ice_.end (), 0.0);
}

void
SoilWater1D::drain (const std::vector<double>& v)
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
SoilWater1D::freeze (const Soil&, const std::vector<double>& v)
{
  daisy_assert (v.size () == S_ice_.size ());
  daisy_assert (S_sum_.size () == v.size ());
  for (size_t i = 0; i < v.size (); i++)
    {
      S_sum_[i] += v[i];
      S_ice_[i] -= v[i] * rho_water / rho_ice;
    }
}

double
SoilWater1D::pF (size_t i) const
{
  if (h (i) < 0.0)
    return log10 (-h (i));
  else
    return 0.0;
}

void
SoilWater1D::macro_tick (const Geometry1D& geo, 
                         const Soil& soil, Surface& surface, 
                         Treelog& msg)
{
  if (!macro.get ())			// No macropores.
    return;

  // Calculate preferential flow first.
  std::fill (S_p_.begin (), S_p_.end (), 0.0);
  std::fill (q_p_.begin (), q_p_.end (), 0.0);
  macro->tick (geo, soil, 0, soil.size () - 1, surface, h_ice_, h_, Theta_,
	       S_sum_, S_p_, q_p_, msg);
}

void
SoilWater1D::tick (const Geometry1D& geo,
                   const Soil& soil,
                   const SoilHeat& soil_heat, 
                   Surface& surface, Groundwater& groundwater,
                   Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater1D");

  // Ice first.
  for (size_t i = 0; i < soil.size (); i++)
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
	      tmp << "BUG: X_ice[" << i << "] = " << X_ice_[i]
                  << " (S_sum[i] = " << S_sum_[i] << ")";
	      msg.error (tmp.str ());
	    }
          X_ice_buffer_[i] += X_ice_[i];
	  X_ice_[i] = 0.0;
	}

      // Update ice pressure.
      h_ice_[i] = soil.h (i, Theta_sat - X_ice_[i]);
    }

  // External source.
  for (size_t i = 0; i < soil.size (); i++)
    {
      S_incorp_[i] += S_permanent_[i];
      S_sum_[i] += S_incorp_[i];
    }

  // Remember old values.
  Theta_old_ = Theta_;
  h_old_ = h_;

  // Limit for groundwater table.
  size_t last  = soil.size () - 1;
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      daisy_assert (soil.size () > 1);
      if (groundwater.table () <= geo.zplus (soil.size () - 2))
	throw ("Groundwater table in or below lowest cell.");
      last = geo.interval_plus (groundwater.table ());
      if (last >=  soil.size () - 1)
	daisy_assert ("Groundwater too low.");
      // Pressure at the last cell is equal to the water above it.
      for (size_t i = last + 1; i < soil.size (); i++)
	{
	  h_old_[i] = groundwater.table () - geo.z (i);
	  h_[i] = groundwater.table () - geo.z (i);
	}
    }

  // Limit for ridging.
  const size_t first = surface.soil_top () ? surface.last_cell () : 0;
  bool ok = true;

  // Calculate matrix flow next.
  try
    {
      ok = top->tick (msg, geo, soil, soil_heat,
                      first, surface,
                      last, groundwater,
                      S_sum_, h_old_, Theta_old_, h_ice_,
                      h_, Theta_, q_);
    }
  catch (const char* error)
    {
      msg.warning (std::string ("UZ problem: ") + error);
      ok = false;
    }
  catch (const std::string& error)
    {
      msg.warning (std::string ("UZ problem: ") + error);
      ok = false;
    }
  if (!ok)
    {
      msg.message ("Using reserve uz model.");
      reserve->tick (msg, geo, soil, soil_heat,
                     first, surface,
                     last, groundwater,
                     S_sum_, h_old_, Theta_old_, h_ice_,
                     h_, Theta_, q_);
    }

  for (size_t i = last + 2; i <= soil.size (); i++)
    {
      q_[i] = q_[i-1];
      q_p_[i] = q_p_[i-1];
    }

  // Update Theta below groundwater table.
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      for(size_t i = last + 1; i < soil.size (); i++)
	Theta_[i] = soil.Theta (i, h_[i], h_ice_[i]);
    }

  // Update surface and groundwater reservoirs.
  const bool top_accepted = surface.accept_top (msg, q_[0] * dt);
  daisy_assert (top_accepted);
  const bool bottom_accepted 
    = groundwater.accept_bottom ((q_[last + 1] + q_p_[last + 1]) * dt);
  daisy_assert (bottom_accepted);
}

bool 
SoilWater1D::check (size_t n, Treelog& msg) const
{
  bool ok = check_base (n, msg);

  // Check ice.
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
SoilWater1D::output (Log& log) const
{
  output_base (log);
  output_value (S_p_, "S_p", log);
  output_value (S_permanent_, "S_permanent", log);
  output_value (S_ice_, "S_ice", log);
  output_value (X_ice_, "X_ice", log);
  output_value (X_ice_buffer_, "X_ice_buffer", log);
  output_value (h_ice_, "h_ice", log);
  output_value (q_, "q", log);
  output_value (q_p_, "q_p", log);
  output_derived (top, "UZtop", log);
}

double
SoilWater1D::MaxExfiltration (const Geometry& geo,
                              const Soil& soil, double T) const
{
  return - ((soil.K (0, h_[0], h_ice_[0], T) / soil.Cw2 (0, h_[0])) 
	    * ((Theta_[0] - soil.Theta_res (0)) / geo.z(0)));
}

void
SoilWater1D::initialize (const AttributeList& al,
                         const Geometry1D& geo,
                         const Soil& soil,
                         const Groundwater& groundwater, 
                         Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater1D");
  const size_t size = geo.cell_size ();

  // Initialize ice.
  if (al.check ("X_ice"))
    {
      X_ice_ = al.number_sequence ("X_ice");
      if (X_ice_.size () == 0)
	X_ice_.push_back (0.0);
      while (X_ice_.size () < size)
	X_ice_.push_back (X_ice_[X_ice_.size () - 1]);
    }
  else 
    X_ice_.insert (X_ice_.begin (), size, 0.0);

  if (al.check ("X_ice_buffer"))
    {
      X_ice_buffer_ = al.number_sequence ("X_ice_buffer");
      if (X_ice_buffer_.size () == 0)
	X_ice_buffer_.push_back (0.0);
      while (X_ice_buffer_.size () < size)
	X_ice_buffer_.push_back (X_ice_buffer_[X_ice_buffer_.size () - 1]);
    }
  else 
    X_ice_buffer_.insert (X_ice_buffer_.begin (), size, 0.0);

  for (size_t i = 0; i < size; i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      daisy_assert (Theta_sat >= X_ice_[i]);
      h_ice_.push_back (soil.h (i, Theta_sat - X_ice_[i]));
    }
  daisy_assert (h_ice_.size () == size);

  // Initialize base (requires ice!).
  initialize_base (al, geo, soil, msg);

  // Sources.
  S_p_.insert (S_p_.begin (), size, 0.0);
  if (S_permanent_.size () < size)
    S_permanent_.insert (S_permanent_.end (), size - S_permanent_.size (), 
                         0.0);
  S_ice_.insert (S_ice_.begin (), size, 0.0);

  // Fluxes.
  q_.insert (q_.begin (), size + 1, 0.0);
  q_p_.insert (q_p_.begin (), size + 1, 0.0);

  // Groundwater based pressure.
  if (h_.size () == 0)
    {
      if (groundwater.table () > 0.0)
	{
	  const double h_pF2 = -100.0; // pF 2.0;
	  for (size_t i = 0; i < size; i++)
	    {
	      h_.push_back (h_pF2);
	      Theta_.push_back (soil.Theta (i, h_pF2, h_ice_[i]));
	    }
	}
      else
	{
	  const double table = groundwater.table ();
	  
	  for (size_t i = 0; i < size; i++)
	    {
	      h_.push_back (std::max (-100.0, table - geo.z (i)));
	      Theta_.push_back (soil.Theta (i, h_[i], h_ice_[i]));
	    }
	}
    }
  daisy_assert (h_.size () == size);

  // We just assume no changes.
  Theta_old_ = Theta_;
  h_old_ = h_;

  // Macropores.
  if (al.check ("macro"))
    macro.reset (Librarian<Macro>::build_free (msg, al.alist ("macro"), 
                                               "macro"));
  else if (soil.humus (0) + soil.clay (0) > 0.05)
    // More than 5% clay (and humus) in first horizon.
    {
      // Find first non-clay layer.
      size_t lay = 1;
      while (lay < size && soil.humus (lay) + soil.clay (lay) > 0.05)
	lay++;

      // Don't go below 1.5 m.
      double height = std::max (geo.zplus (lay-1), -150.0);

      // Don't go below drain pipes.
      if (groundwater.is_pipe ())
	height = std::max (height, groundwater.pipe_height ());

      // Add them.
      macro = Macro::create (height);

      msg.debug ("Adding macropores");
    }

  // Let 'macro' choose the default method to average K values in 'uz'.
  const bool has_macropores = (macro.get () && !macro->none ());
  top->has_macropores (has_macropores);
}

SoilWater1D::SoilWater1D (Block& al)
  : SoilWater (al),
    S_permanent_ (al.number_sequence ("S_permanent")),
    top (Librarian<UZmodel>::build_item (al, "UZtop")),
    reserve (Librarian<UZmodel>::build_item (al, "UZreserve")),
    macro (NULL)
{ }

SoilWater1D::~SoilWater1D ()
{ }

void
SoilWater1D::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  load_base (syntax, alist);
  alist.add ("submodel", "SoilWater1D");
  alist.add ("description", "Water content of a vertical soil column.");

  syntax.add ("UZtop", Librarian<UZmodel>::library (),
	      "Main water transport model in unsaturated zone.");
  alist.add ("UZtop", UZmodel::default_model ());
  syntax.add ("UZreserve", Librarian<UZmodel>::library (),
	      "Reserve transport model if UZtop fails.");
  alist.add ("UZreserve", UZmodel::reserve_model ());
  syntax.add ("S_p", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink (due to macropores).");
  syntax.add ("S_permanent", "h^-1", Syntax::State, Syntax::Sequence,
	      "Permanent water sink, e.g. subsoil irrigation.");
  std::vector<double> empty;
  alist.add ("S_permanent", empty);
  syntax.add ("S_ice", "h^-1", Syntax::LogOnly, Syntax::Sequence,
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

  syntax.add ("macro", Librarian<Macro>::library (),
	      Syntax::OptionalState, Syntax::Singleton,
	      "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
}

static Submodel::Register 
soil_water_1d_submodel ("SoilWater1D", SoilWater1D::load_syntax);
