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


#include "soil_water.h"
#include "log.h"
#include "alist.h"
#include "uzmodel.h"
#include "soil.h"
#include "surface.h"
#include "groundwater.h"
#include "syntax.h"
#include "mathlib.h"
#include "tmpstream.h"
#include "submodel.h"

void
SoilWater::clear (const Geometry&)
{
  fill (S_sum_.begin (), S_sum_.end (), 0.0);
  fill (S_root_.begin (), S_root_.end (), 0.0);
  fill (S_ice_.begin (), S_ice_.end (), 0.0);
  // We don't clear S_p_ and S_drain_, because they are needed in solute.
}


void
SoilWater::root_uptake (const vector<double>& v)
{
  daisy_assert (S_sum_.size () == v.size ());
  daisy_assert (S_root_.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_sum_[i] += v[i];
      S_root_[i] += v[i];
    }
}

void 
SoilWater::freeze (const Soil&, const vector<double>& v)
{
  daisy_assert (v.size () == S_ice_.size ());
  daisy_assert (S_sum_.size () == v.size ());
  for (unsigned int i = 0; i < v.size (); i++)
    {
      S_sum_[i] += v[i];
      S_ice_[i] -= v[i] * rho_water / rho_ice;
    }
}

double 
SoilWater::content (const Geometry& geometry, double from, double to) const
{ return geometry.total (Theta_, from, to); }

double
SoilWater::pF (int i) const
{
  if (h (i) < 0.0)
    return log10 (-h (i));
  else
    return 0.0;
}

unsigned int 
SoilWater::first_groundwater_node () const
{ 
  for (unsigned int i = h_.size (); i > 0u; i--)
    if (h_[i-1] < 0.0)
      return i;
  return 0u;
}

double 
SoilWater::Theta (const Soil& soil, int i, double h) const
{ return soil.Theta (i, h, h_ice_[i]); }

void
SoilWater::macro_tick (const Soil& soil, Surface& surface, Treelog& out)
{
  // Calculate preferential flow first.
  fill (S_p_.begin (), S_p_.end (), 0.0);
  fill (q_p_.begin (), q_p_.end (), 0.0);
  macro.tick (soil, 0, soil.size () - 1, surface, h_ice_, h_, Theta_,
	      S_sum_, S_p_, q_p_, out);
}

void
SoilWater::tick (const Soil& soil, const SoilHeat& soil_heat, 
		 Surface& surface, Groundwater& groundwater,
		 Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater");

  // Ice first.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      X_ice_[i] -= S_ice_[i];
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);

      // Move extra ice to buffer.
      const double available_space
	= Theta_sat - Theta_[i] - X_ice_[i] + S_sum_[i] * dt;
      if (available_space < 0.0)
	{
	  X_ice_[i] += available_space;
	  X_ice_buffer[i] -= available_space;
	}
      else if (X_ice_buffer[i] > 0.0)
	{
	  if (X_ice_buffer[i] < available_space)
	    { 
	      X_ice_[i] += X_ice_buffer[i];
	      X_ice_buffer[i] = 0.0;
	    }
	  else
	    {
	      X_ice_[i] += available_space;
	      X_ice_buffer[i] -= available_space;
	    }
	}

      if (X_ice_[i] < 0.0)
	{
	  if (X_ice_[i] < -1e-13)
	    {
	      TmpStream tmp;
	      tmp () << "BUG: X_ice[" << i << "] = " << X_ice_[i]
		     << " (S_sum[i] = " << S_sum_[i] << ")";
	      msg.error (tmp.str ());
	    }
          X_ice_buffer[i] += X_ice_[i];
	  X_ice_[i] = 0.0;
	}

      // Update ice presure.
      h_ice_[i] = soil.h (i, Theta_sat - X_ice_[i]);
    }

  // External source.
  for (unsigned int i = 0; i < soil.size (); i++)
    S_sum_[i] += S_permanent_[i];

  // Remember old values.
  Theta_old_ = Theta_;
  h_old = h_;

  // Limit for groundwater table.
  int last  = soil.size () - 1;
  if (!groundwater.flux_bottom ())
    {
      daisy_assert (soil.size () > 1);
      if (groundwater.table () <= soil.zplus (soil.size () - 2))
	throw ("Groundwater table in or below lowest node.");
      last = soil.interval_plus (groundwater.table ());
      if (last >=  soil.size () - 1)
	daisy_assert ("Groundwater too low.");
      // Presure at the last node is equal to the water above it.
      for (unsigned int i = last + 1; i < soil.size (); i++)
	{
	  h_old[i] = groundwater.table () - soil.z (i);
	  h_[i] = groundwater.table () - soil.z (i);
	}
    }

  // Limit for ridging.
  const int first = surface.soil_top () ? surface.last_node () : 0;
  daisy_assert (first >= 0);
  bool ok = true;

  // Calculate matrix flow next.
  try
    {
      if (bottom)
	{
	  // We have two UZ models.
	  ok = top->tick (msg, soil, soil_heat,
			  first, surface,
			  bottom_start - 1, *bottom,
			  S_sum_, h_old, Theta_old_, h_ice_,
			  h_, Theta_, q_);
	  if (ok)
	    ok = bottom->tick (msg, soil, soil_heat,
			       bottom_start, *top,
			       last, groundwater,
			       S_sum_, h_old, Theta_old_, h_ice_,
			       h_, Theta_, q_);
	}
      else
	{
	  // We have only one UZ model.
	  ok = top->tick (msg, soil, soil_heat,
			  first, surface,
			  last, groundwater,
			  S_sum_, h_old, Theta_old_, h_ice_,
			  h_, Theta_, q_);
	}
    }
  catch (const char* error)
    {
      msg.warning (string ("UZ problem: ") + error);
      ok = false;
    }
  if (!ok)
    {
      msg.message ("Using reserve uz model.");
      reserve->tick (msg, soil, soil_heat,
                     first, surface,
                     last, groundwater,
                     S_sum_, h_old, Theta_old_, h_ice_,
                     h_, Theta_, q_);
    }

  for (unsigned int i = last + 2; i <= soil.size (); i++)
    {
      q_[i] = q_[i-1];
      q_p_[i] = q_p_[i-1];
    }

  // Update Theta below groundwater table.
  if (!groundwater.flux_bottom ())
    {
      for(unsigned int i = last + 1; i < soil.size (); i++)
	Theta_[i] = soil.Theta (i, h_[i], h_ice_[i]);
    }

  // Update surface and groundwater reservoirs.
  const bool top_accepted = surface.accept_top (msg, q_[0] * dt);
  daisy_assert (top_accepted);
  const bool bottom_accepted 
    = groundwater.accept_bottom ((q_[last + 1] + q_p_[last + 1]) * dt);
  daisy_assert (bottom_accepted);

  // Update flux in surface and groundwater.
  surface.update_water (soil, S_sum_, h_, Theta_, q_, q_p_);
  groundwater.update_water (soil, soil_heat,
			    S_sum_, S_drain_, h_, h_ice_, Theta_, q_, q_p_,
			    msg);
}

void 
SoilWater::set_external_source (const Geometry& geometry, 
				double amount, double from, double to)
{
  fill (S_permanent_.begin (), S_permanent_.end (), 0.0);
  geometry.add (S_permanent_, from, to, -amount);
}

void
SoilWater::mix (const Soil& soil, double from, double to)
{
  soil.mix (Theta_, from, to);
  for (unsigned int i = 0; i < soil.size(); i++)
    h_[i] = soil.h (i, Theta_[i]);
}

void
SoilWater::swap (Treelog& msg,
		 const Soil& soil, double from, double middle, double to)
{
  soil.swap (Theta_, from, middle, to);

  for (unsigned int i = 0; i < soil.size(); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      if (Theta_[i] > Theta_sat)
	{
	  TmpStream tmp;
	  tmp () << "BUG: Theta[ " << i << "] (" << Theta_[i]
		 << ") > Theta_sat (" << Theta_sat << ")";
	  msg.error (tmp.str ());
	  Theta_[i] = Theta_sat;
	}
      h_[i] = soil.h (i, Theta_[i]);
    }
}
  
void
SoilWater::set_Theta (const Soil& soil, 
		      unsigned int from, unsigned int to, double Theta)
{
  for (unsigned int i = from; i <= to; i++)
    {
      Theta_[i] = Theta;
      h_[i] = soil.h (i, Theta);
    }
}


bool 
SoilWater::check (unsigned n, Treelog& err) const
{
  bool ok = true;

  if (Theta_.size () != n)
    {
      TmpStream tmp;
      tmp () << "You have " << n 
	     << " intervals but " << Theta_.size () << " Theta values";
      err.entry (tmp.str ());
      ok = false;
    }
  if (h_.size () != n)
    {
      TmpStream tmp;
      tmp () << "You have " << n 
	     << " intervals but " << h_.size () << " h values";
      err.entry (tmp.str ());
      ok = false;
    }
  if (X_ice_.size () != n)
    {
      TmpStream tmp;
      tmp () << "You have " << n 
	     << " intervals but " << X_ice_.size () << " X_ice values";
      err.entry (tmp.str ());
      ok = false;
    }
  if (X_ice_buffer.size () != n)
    {
      TmpStream tmp;
      tmp () << "You have " << n 
	     << " intervals but " << X_ice_buffer.size () 
	     << " X_ice_buffer values";
      err.entry (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
SoilWater::output (Log& log) const
{
  log.output ("S_sum", S_sum_);
  log.output ("S_root", S_root_);
  log.output ("S_drain", S_drain_);
  log.output ("S_p", S_p_);
  log.output ("S_permanent", S_permanent_);
  log.output ("Theta", Theta_);
  log.output ("h", h_);
  log.output ("S_ice", S_ice_);
  log.output ("X_ice", X_ice_);
  log.output ("X_ice_buffer", X_ice_buffer);
  log.output ("h_ice", h_ice_);
  log.output ("q", q_);
  log.output ("q_p", q_p_);
  output_derived (*top, "UZtop", log);
  if (bottom)
    output_derived (*bottom, "UZbottom", log);
}

double
SoilWater::MaxExfiltration (const Soil& soil, double T) const
{
  return - ((soil.K (0, h_[0], h_ice_[0], T) / soil.Cw2 (0, h_[0])) 
	    * ((Theta_[0] - soil.Theta_res (0)) / soil.z(0)));
}

void 
SoilWater::put_h (const Soil& soil, const vector<double>& v) // [cm]
{
  const int size = soil.size ();
  daisy_assert (v.size () == size);
  daisy_assert (h_.size () == size);
  daisy_assert (Theta_.size () == size);

  h_ = v;

  for (unsigned int i = 0; i < size; i++)
    Theta_[i] = soil.Theta (i, h_[i], h_ice_[i]);
}

void
SoilWater::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilWater");
  alist.add ("description", "Water content of soil.");

  syntax.add ("UZtop", Librarian<UZmodel>::library (),
	      "Main water transport model in unsaturated zone.");
  AttributeList richard;
  richard.add ("type", "richards");
  richard.add ("max_time_step_reductions", 4);
  richard.add ("time_step_reduction", 4);
  richard.add ("max_iterations", 25);
  richard.add ("max_absolute_difference", 0.02);
  richard.add ("max_relative_difference", 0.001);
  alist.add ("UZtop", richard);

  syntax.add ("UZbottom", Librarian<UZmodel>::library (),
	      Syntax::OptionalState, Syntax::Singleton, "\
Water transport model for the bottom of the unsaturated zone.\n\
If this is given, 'UZtop' will be used down to 'UZborder', and 'UZbottom'\n\
will be used from there to the bottom.");
  syntax.add ("UZborder", Syntax::Integer, Syntax::OptionalConst,
	      "Top node to use 'UZbottom' in.");
  syntax.add ("UZreserve", Librarian<UZmodel>::library (),
	      "Reserve transport model if UZtop fails.");
  // Use lr as UZreserve by default.
  AttributeList lr;
  lr.add ("type", "lr");
  lr.add ("h_fc", -100.0);
  lr.add ("z_top", -10.0);
  alist.add ("UZreserve", lr);
  syntax.add ("S_sum", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Total water sink (due to root uptake and macropores).");
  syntax.add ("S_root", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to root uptake.");
  syntax.add ("S_drain", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to soil drainage.");
  syntax.add ("S_p", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink (due to macropores).");
  syntax.add ("S_permanent", "cm^3/cm^3/h", Syntax::State, Syntax::Sequence,
	      "Permanent water sink, e.g. subsoil irrigation.");
  vector<double> empty;
  alist.add ("S_permanent", empty);
  Geometry::add_layer (syntax, "Theta", "cm^3/cm^3", "Soil water content.");
  Geometry::add_layer (syntax, "h", "cm", "Soil water pressure.");
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

  syntax.add ("macro", Librarian<Macro>::library (),
	      "Preferential flow model.");
  AttributeList macro;
  macro.add ("type", "none");
  alist.add ("macro", macro);
}

SoilWater::SoilWater (const AttributeList& al)
  : S_permanent_ (al.number_sequence ("S_permanent")),
    top (&Librarian<UZmodel>::create (al.alist ("UZtop"))),
    bottom (  al.check ("UZbottom") 
	    ? &Librarian<UZmodel>::create (al.alist ("UZbottom"))
	    : 0),
    bottom_start (  al.check ("UZborder") 
		  ? al.integer ("UZborder")
		  : -1),
    reserve (&Librarian<UZmodel>::create (al.alist ("UZreserve"))),
    macro (Librarian<Macro>::create (al.alist ("macro")))
{ }

void
SoilWater::initialize (const AttributeList& al,
		       const Soil& soil, const Groundwater& groundwater, 
		       Treelog& out)
{
  const unsigned int size = soil.size ();

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
      X_ice_buffer = al.number_sequence ("X_ice_buffer");
      if (X_ice_buffer.size () == 0)
	X_ice_buffer.push_back (0.0);
      while (X_ice_buffer.size () < size)
	X_ice_buffer.push_back (X_ice_buffer[X_ice_buffer.size () - 1]);
    }
  else 
    X_ice_buffer.insert (X_ice_buffer.begin (), size, 0.0);

  h_ice_.insert (h_ice_.begin (), size, 0.0);
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      daisy_assert (Theta_sat >= X_ice_[i]);
      h_ice_[i] = soil.h (i, Theta_sat - X_ice_[i]);
    }

  soil.initialize_layer (Theta_, al, "Theta", out);
  soil.initialize_layer (h_, al, "h", out);

  if (Theta_.size () > 0)
    {
      while (Theta_.size () < size)
	Theta_.push_back (Theta_[Theta_.size () - 1]);
      if (h_.size () == 0)
	for (unsigned int i = 0; i < size; i++)
	  h_.push_back (soil.h (i, Theta_[i]));
    }
  if (h_.size () > 0)
    {
      while (h_.size () < size)
	h_.push_back (h_[h_.size () - 1]);
      if (Theta_.size () == 0)
	for (unsigned int i = 0; i < size; i++)
	  Theta_.push_back (soil.Theta (i, h_[i], h_ice_[i]));
    }

  S_sum_.insert (S_sum_.begin (), size, 0.0);
  S_root_.insert (S_root_.begin (), size, 0.0);
  S_drain_.insert (S_drain_.begin (), size, 0.0);
  S_p_.insert (S_p_.begin (), size, 0.0);
  if (S_permanent_.size () < size)
    S_permanent_.insert (S_permanent_.end (), size - S_permanent_.size (),
			 0.0);
  q_.insert (q_.begin (), size + 1, 0.0);
  q_p_.insert (q_p_.begin (), size + 1, 0.0);
  S_ice_.insert (S_ice_.begin (), size, 0.0);

  daisy_assert (h_.size () == Theta_.size ());
  if (h_.size () == 0)
    {
      if (groundwater.flux_bottom ())
	{
	  const double h = -100.0; // pF 2.0;
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h_.push_back (h);
	      Theta_.push_back (soil.Theta (i, h, h_ice_[i]));
	    }
	}
      else
	{
	  const double table = groundwater.table ();
	  
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h_.push_back (max (-100.0, table - soil.z (i)));
	      Theta_.push_back (soil.Theta (i, h_[i], h_ice_[i]));
	    }
	}
    }
  daisy_assert (h_.size () == soil.size ());

  // We just assume no changes.
  Theta_old_ = Theta_;
  h_old = h_;

  // Let 'macro' choose the default method to average K values in 'uz'.
  const bool has_macropores = (al.alist ("macro").name ("type") != "none");
  top->has_macropores (has_macropores);
  if (bottom)
    bottom->has_macropores (has_macropores);
}

SoilWater::~SoilWater ()
{
  daisy_assert (top);
  delete top;
  if (bottom)
    delete bottom;
  daisy_assert (reserve);
  delete reserve;
  delete &macro;
}

static Submodel::Register 
soil_water_submodel ("SoilWater", SoilWater::load_syntax);
