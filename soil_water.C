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
#include "macro.h"
#include "syntax.h"
#include "mathlib.h"
#include "tmpstream.h"
#include "submodel.h"

using namespace std;

static const double rho_water = 1.0; // [g/cm^3]
static const double rho_ice = 0.917; // [g/cm^3]

struct SoilWater::Implementation
{
  // Content.
  vector<double> S_sum;
  vector<double> S_root;
  vector<double> S_drain;
  vector<double> S_p;
  vector<double> S_permanent;
  vector<double> S_incorp;
  vector<double> Theta_old;
  vector<double> h_old;
  vector<double> Theta;
  vector<double> h;
  vector<double> S_ice;
  vector<double> X_ice;
  vector<double> X_ice_buffer;
  vector<double> h_ice;
  vector<double> q;
  vector<double> q_p;
  UZmodel *const top;
  UZmodel *const bottom;
  const int bottom_start;
  UZmodel *const reserve;
  Macro* macro;

  unsigned int first_groundwater_node () const;

  // Sink.
public:
  void clear (const Geometry&);
  void root_uptake (const vector<double>&);
  void drain (const vector<double>&);
  void freeze (const Soil&, const vector<double>&);

  // Simulation.
public:
  void macro_tick (const Soil&, Surface&, Treelog&);
  void tick (const Soil&, const SoilHeat&, Surface&, Groundwater&, Treelog&);
  void set_external_source (const Geometry&, 
			    double amount, double from, double to);
  void incorporate (const Geometry&, double amount, double from, double to);
  void mix (const Soil&, double from, double to);
  void swap (Treelog&, const Soil&, double from, double middle, double to);
  void set_Theta (const Soil& soil, 
		  unsigned int from, unsigned int to, double Theta);
  bool check (unsigned n, Treelog& err) const;
  void output (Log&) const;

  // Communication with surface.
  double MaxExfiltration (const Soil&, double T) const;

  // Communication with external model.
  void put_h (const Soil& soil, const vector<double>& v); // [cm]

  // Creation.
  static void load_syntax (Syntax&, AttributeList&);
  Implementation (const AttributeList&);
  void initialize (const AttributeList&, 
		   const Soil& soil, const Groundwater& groundwater,
		   Treelog&);
  ~Implementation ();
};

void
SoilWater::Implementation::clear (const Geometry&)
{
  fill (S_sum.begin (), S_sum.end (), 0.0);
  fill (S_drain.begin (), S_drain.end (), 0.0);
  fill (S_root.begin (), S_root.end (), 0.0);
  fill (S_ice.begin (), S_ice.end (), 0.0);
  fill (S_incorp.begin (), S_incorp.end (), 0.0);
  // We don't clear S_p and S_drain, because they are needed in solute.
}


void
SoilWater::Implementation::root_uptake (const vector<double>& v)
{
  daisy_assert (S_sum.size () == v.size ());
  daisy_assert (S_root.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_sum[i] += v[i];
      S_root[i] += v[i];
    }
}

void
SoilWater::Implementation::drain (const vector<double>& v)
{
  daisy_assert (S_sum.size () == v.size ());
  daisy_assert (S_root.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_sum[i] += v[i];
      S_drain[i] += v[i];
    }
}

void 
SoilWater::Implementation::freeze (const Soil&, const vector<double>& v)
{
  daisy_assert (v.size () == S_ice.size ());
  daisy_assert (S_sum.size () == v.size ());
  for (unsigned int i = 0; i < v.size (); i++)
    {
      S_sum[i] += v[i];
      S_ice[i] -= v[i] * rho_water / rho_ice;
    }
}

unsigned int 
SoilWater::Implementation::first_groundwater_node () const
{ 
  for (unsigned int i = h.size (); i > 0u; i--)
    if (h[i-1] < 0.0)
      return i;
  return 0u;
}

void
SoilWater::Implementation::macro_tick (const Soil& soil, Surface& surface, 
				       Treelog& out)
{
  if (!macro)			// No macropores.
    return;

  // Calculate preferential flow first.
  fill (S_p.begin (), S_p.end (), 0.0);
  fill (q_p.begin (), q_p.end (), 0.0);
  macro->tick (soil, 0, soil.size () - 1, surface, h_ice, h, Theta,
	       S_sum, S_p, q_p, out);
}

void
SoilWater::Implementation::tick (const Soil& soil, const SoilHeat& soil_heat, 
				 Surface& surface, Groundwater& groundwater,
				 Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater");

  // Ice first.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      X_ice[i] -= S_ice[i];
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);

      // Move extra ice to buffer.
      const double available_space
	= Theta_sat - Theta[i] - X_ice[i] + S_sum[i] * dt;
      if (available_space < 0.0)
	{
	  X_ice[i] += available_space;
	  X_ice_buffer[i] -= available_space;
	}
      else if (X_ice_buffer[i] > 0.0)
	{
	  if (X_ice_buffer[i] < available_space)
	    { 
	      X_ice[i] += X_ice_buffer[i];
	      X_ice_buffer[i] = 0.0;
	    }
	  else
	    {
	      X_ice[i] += available_space;
	      X_ice_buffer[i] -= available_space;
	    }
	}

      if (X_ice[i] < 0.0)
	{
	  if (X_ice[i] < -1e-13)
	    {
	      TmpStream tmp;
	      tmp () << "BUG: X_ice[" << i << "] = " << X_ice[i]
		     << " (S_sum[i] = " << S_sum[i] << ")";
	      msg.error (tmp.str ());
	    }
          X_ice_buffer[i] += X_ice[i];
	  X_ice[i] = 0.0;
	}

      // Update ice pressure.
      h_ice[i] = soil.h (i, Theta_sat - X_ice[i]);
    }

  // External source.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      S_incorp[i] += S_permanent[i];
      S_sum[i] += S_incorp[i];
    }

  // Remember old values.
  Theta_old = Theta;
  h_old = h;

  // Limit for groundwater table.
  int last  = soil.size () - 1;
  if (groundwater.bottom_type () == UZbottom::pressure)
    {
      daisy_assert (soil.size () > 1);
      if (groundwater.table () <= soil.zplus (soil.size () - 2))
	throw ("Groundwater table in or below lowest node.");
      last = soil.interval_plus (groundwater.table ());
      if (last >=  soil.size () - 1)
	daisy_assert ("Groundwater too low.");
      // Pressure at the last node is equal to the water above it.
      for (unsigned int i = last + 1; i < soil.size (); i++)
	{
	  h_old[i] = groundwater.table () - soil.z (i);
	  h[i] = groundwater.table () - soil.z (i);
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
			  S_sum, h_old, Theta_old, h_ice,
			  h, Theta, q);
	  if (ok)
	    ok = bottom->tick (msg, soil, soil_heat,
			       bottom_start, *top,
			       last, groundwater,
			       S_sum, h_old, Theta_old, h_ice,
			       h, Theta, q);
	}
      else
	{
	  // We have only one UZ model.
	  ok = top->tick (msg, soil, soil_heat,
			  first, surface,
			  last, groundwater,
			  S_sum, h_old, Theta_old, h_ice,
			  h, Theta, q);
	}
    }
  catch (const char* error)
    {
      msg.warning (string ("UZ problem: ") + error);
      ok = false;
    }
  catch (const string& error)
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
                     S_sum, h_old, Theta_old, h_ice,
                     h, Theta, q);
    }

  for (unsigned int i = last + 2; i <= soil.size (); i++)
    {
      q[i] = q[i-1];
      q_p[i] = q_p[i-1];
    }

  // Update Theta below groundwater table.
  if (groundwater.bottom_type () == UZbottom::pressure)
    {
      for(unsigned int i = last + 1; i < soil.size (); i++)
	Theta[i] = soil.Theta (i, h[i], h_ice[i]);
    }

  // Update surface and groundwater reservoirs.
  const bool top_accepted = surface.accept_top (msg, q[0] * dt);
  daisy_assert (top_accepted);
  const bool bottom_accepted 
    = groundwater.accept_bottom ((q[last + 1] + q_p[last + 1]) * dt);
  daisy_assert (bottom_accepted);
}

void 
SoilWater::Implementation::set_external_source (const Geometry& geometry, 
						double amount, 
						double from, double to)
{
  fill (S_permanent.begin (), S_permanent.end (), 0.0);
  geometry.add (S_permanent, from, to, -amount);
}

void 
SoilWater::Implementation::incorporate (const Geometry& geometry, 
                                        double amount, double from, double to)

{
  geometry.add (S_incorp, from, to, -amount);
}

void
SoilWater::Implementation::mix (const Soil& soil, double from, double to)
{
  soil.mix (Theta, from, to);
  for (unsigned int i = 0; i < soil.size(); i++)
    h[i] = soil.h (i, Theta[i]);
}

void
SoilWater::Implementation::swap (Treelog& msg, const Soil& soil,
				 double from, double middle, double to)
{
  soil.swap (Theta, from, middle, to);

  for (unsigned int i = 0; i < soil.size(); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      if (Theta[i] > Theta_sat)
	{
	  TmpStream tmp;
	  tmp () << "BUG: Theta[ " << i << "] (" << Theta[i]
		 << ") > Theta_sat (" << Theta_sat << ")";
	  msg.error (tmp.str ());
	  Theta[i] = Theta_sat;
	}
      h[i] = soil.h (i, Theta[i]);
    }
}
  
void
SoilWater::Implementation::set_Theta (const Soil& soil, 
				      unsigned int from, unsigned int to, 
				      double value)
{
  for (unsigned int i = from; i <= to; i++)
    {
      Theta[i] = value;
      h[i] = soil.h (i, value);
    }
}


bool 
SoilWater::Implementation::check (unsigned n, Treelog& err) const
{
  bool ok = true;

  if (Theta.size () != n)
    {
      TmpStream tmp;
      tmp () << "You have " << n 
	     << " intervals but " << Theta.size () << " Theta values";
      err.entry (tmp.str ());
      ok = false;
    }
  if (h.size () != n)
    {
      TmpStream tmp;
      tmp () << "You have " << n 
	     << " intervals but " << h.size () << " h values";
      err.entry (tmp.str ());
      ok = false;
    }
  if (X_ice.size () != n)
    {
      TmpStream tmp;
      tmp () << "You have " << n 
	     << " intervals but " << X_ice.size () << " X_ice values";
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
SoilWater::Implementation::output (Log& log) const
{
  output_variable (S_sum, log);
  output_variable (S_root, log);
  output_variable (S_drain, log);
  output_variable (S_p, log);
  output_variable (S_permanent, log);
  output_variable (S_incorp, log);
  output_variable (Theta, log);
  output_variable (h, log);
  output_variable (S_ice, log);
  output_variable (X_ice, log);
  output_variable (X_ice_buffer, log);
  output_variable (h_ice, log);
  output_variable (q, log);
  output_variable (q_p, log);
  output_derived (*top, "UZtop", log);
  if (bottom)
    output_derived (*bottom, "UZbottom", log);
}

double
SoilWater::Implementation::MaxExfiltration (const Soil& soil, double T) const
{
  return - ((soil.K (0, h[0], h_ice[0], T) / soil.Cw2 (0, h[0])) 
	    * ((Theta[0] - soil.Theta_res (0)) / soil.z(0)));
}

void 
SoilWater::Implementation::put_h (const Soil& soil,
				  const vector<double>& v) // [cm]
{
  const int size = soil.size ();
  daisy_assert (v.size () == size);
  daisy_assert (h.size () == size);
  daisy_assert (Theta.size () == size);

  h = v;

  for (unsigned int i = 0; i < size; i++)
    Theta[i] = soil.Theta (i, h[i], h_ice[i]);
}

SoilWater::Implementation::Implementation (const AttributeList& al)
  : S_permanent (al.number_sequence ("S_permanent")),
    top (&Librarian<UZmodel>::create (al.alist ("UZtop"))),
    bottom (  al.check ("UZbottom") 
	    ? &Librarian<UZmodel>::create (al.alist ("UZbottom"))
	    : 0),
    bottom_start (  al.check ("UZborder") 
		  ? al.integer ("UZborder")
		  : -1),
    reserve (&Librarian<UZmodel>::create (al.alist ("UZreserve"))),
    macro (NULL)
{ }

void
SoilWater::Implementation::initialize (const AttributeList& al,
				       const Soil& soil,
				       const Groundwater& groundwater, 
				       Treelog& out)
{
  Treelog::Open nest (out, "SoilWater");

  const unsigned int size = soil.size ();

  if (al.check ("X_ice"))
    {
      X_ice = al.number_sequence ("X_ice");
      if (X_ice.size () == 0)
	X_ice.push_back (0.0);
      while (X_ice.size () < size)
	X_ice.push_back (X_ice[X_ice.size () - 1]);
    }
  else 
    X_ice.insert (X_ice.begin (), size, 0.0);

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

  h_ice.insert (h_ice.begin (), size, 0.0);
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      daisy_assert (Theta_sat >= X_ice[i]);
      h_ice[i] = soil.h (i, Theta_sat - X_ice[i]);
    }

  soil.initialize_layer (Theta, al, "Theta", out);
  soil.initialize_layer (h, al, "h", out);

  for (int i = 0; i < Theta.size () && i < h.size (); i++)
    {
      const double Theta_h = soil.Theta (i, h[i], h_ice[i]);
      if (!approximate (Theta[i], Theta_h))
	{
	  TmpStream tmp;
	  tmp () << "Theta[" << i << "] (" << Theta[i] << ") != Theta (" 
		 << h[i] << ") (" << Theta_h << ")";
	  out.error (tmp.str ());
	}
      Theta[i] = Theta_h;
    }
  if (Theta.size () > 0)
    {
      while (Theta.size () < size)
	Theta.push_back (Theta[Theta.size () - 1]);
      if (h.size () == 0)
	for (unsigned int i = 0; i < size; i++)
	  h.push_back (soil.h (i, Theta[i]));
    }
  if (h.size () > 0)
    {
      while (h.size () < size)
	h.push_back (h[h.size () - 1]);
      if (Theta.size () == 0)
	for (unsigned int i = 0; i < size; i++)
	  Theta.push_back (soil.Theta (i, h[i], h_ice[i]));
    }

  S_sum.insert (S_sum.begin (), size, 0.0);
  S_root.insert (S_root.begin (), size, 0.0);
  S_drain.insert (S_drain.begin (), size, 0.0);
  S_p.insert (S_p.begin (), size, 0.0);
  S_incorp.insert (S_incorp.begin (), size, 0.0);
  if (S_permanent.size () < size)
    S_permanent.insert (S_permanent.end (), size - S_permanent.size (), 0.0);

  q.insert (q.begin (), size + 1, 0.0);
  q_p.insert (q_p.begin (), size + 1, 0.0);
  S_ice.insert (S_ice.begin (), size, 0.0);

  daisy_assert (h.size () == Theta.size ());
  if (h.size () == 0)
    {
      if (groundwater.table () > 0.0)
	{
	  const double h_pF2 = -100.0; // pF 2.0;
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h.push_back (h_pF2);
	      Theta.push_back (soil.Theta (i, h_pF2, h_ice[i]));
	    }
	}
      else
	{
	  const double table = groundwater.table ();
	  
	  for (unsigned int i = 0; i < soil.size (); i++)
	    {
	      h.push_back (max (-100.0, table - soil.z (i)));
	      Theta.push_back (soil.Theta (i, h[i], h_ice[i]));
	    }
	}
    }
  daisy_assert (h.size () == soil.size ());

  // We just assume no changes.
  Theta_old = Theta;
  h_old = h;

  if (al.check ("macro"))
    macro = &Librarian<Macro>::create (al.alist ("macro"));
  else if (soil.humus (0) + soil.clay (0) > 0.05)
    // More than 5% clay (and humus) in first horizon.
    {
      // Find first non-clay layer.
      int lay = 1;
      while (lay < soil.size () && soil.humus (lay) + soil.clay (lay) > 0.05)
	lay++;

      // Don't go below 1.5 m.
      double height = max (soil.zplus (lay-1), -150.0);

      // Don't go below drain pipes.
      if (groundwater.is_pipe ())
	height = max (height, groundwater.pipe_height ());

      // Add them.
      macro = &Macro::create (height);

      out.debug ("Adding macropores");
    }

  // Let 'macro' choose the default method to average K values in 'uz'.
  const bool has_macropores = (macro && !macro->none ());
  top->has_macropores (has_macropores);
  if (bottom)
    bottom->has_macropores (has_macropores);
}

SoilWater::Implementation::~Implementation ()
{
  daisy_assert (top);
  delete top;
  if (bottom)
    delete bottom;
  daisy_assert (reserve);
  delete reserve;
  if (macro)
    delete macro;
}

void
SoilWater::clear (const Geometry& geometry)
{ impl.clear (geometry); }

void
SoilWater::root_uptake (const vector<double>& v)
{ impl.root_uptake (v); }

void
SoilWater::drain (const vector<double>& v)
{ impl.drain (v); }

void 
SoilWater::freeze (const Soil& soil, const vector<double>& v)
{ impl.freeze (soil, v); }

double
SoilWater::h (int i) const
{ return impl.h[i]; }

double
SoilWater::pF (int i) const
{
  if (h (i) < 0.0)
    return log10 (-h (i));
  else
    return 0.0;
}

double
SoilWater::Theta (int i) const
{ return impl.Theta[i]; }

double
SoilWater::Theta_left (int i) const
{ return impl.Theta[i] - impl.S_sum[i]; }

double
SoilWater::Theta_old (int i) const
{ return impl.Theta_old[i]; }

double 
SoilWater::content (const Geometry& geometry, double from, double to) const
{ return geometry.total (impl.Theta, from, to); }

double
SoilWater::q (int i) const
{ 
  return impl.q[i]; 
}

double
SoilWater::q_p (int i) const
{ return impl.q_p[i]; }

double
SoilWater::S_sum (int i) const
{ return impl.S_sum[i]; }

double
SoilWater::S_root (int i) const
{ return impl.S_root[i]; }

double
SoilWater::S_drain (int i) const
{ return impl.S_drain[i]; }

double
SoilWater::S_ice (int i) const
{ return impl.S_ice[i]; }

double
SoilWater::S_p (int i) const
{ return impl.S_p[i]; }

double
SoilWater::h_ice (int i) const
{ return impl.h_ice[i]; }

double
SoilWater::X_ice (int i) const
{ return impl.X_ice[i]; }

double
SoilWater::X_ice_total (int i) const
{ return impl.X_ice[i] + impl.X_ice_buffer[i]; }


unsigned int 
SoilWater::first_groundwater_node () const
{ return impl.first_groundwater_node (); }

double 
SoilWater::Theta (const Soil& soil, int i, double h) const
{ return soil.Theta (i, h, h_ice (i)); }

void
SoilWater::macro_tick (const Soil& soil, Surface& surface, Treelog& out)
{ impl.macro_tick (soil, surface, out); }

void
SoilWater::tick (const Soil& soil, const SoilHeat& soil_heat, 
		 Surface& surface, Groundwater& groundwater,
		 Treelog& msg)
{ impl.tick (soil, soil_heat, surface, groundwater, msg); }

void 
SoilWater::set_external_source (const Geometry& geometry, 
				double amount, double from, double to)
{ impl.set_external_source (geometry, amount, from, to); }

void 
SoilWater::incorporate (const Geometry& geometry, 
                        double amount, double from, double to)
{ impl.incorporate (geometry, amount, from, to); }

void
SoilWater::mix (const Soil& soil, double from, double to)
{ impl.mix (soil, from, to); }

void
SoilWater::swap (Treelog& msg,
		 const Soil& soil, double from, double middle, double to)
{ impl.swap (msg, soil, from, middle, to); }
  
void
SoilWater::set_Theta (const Soil& soil, 
		      unsigned int from, unsigned int to, double Theta)
{ impl.set_Theta (soil, from, to, Theta); }

bool 
SoilWater::check (unsigned n, Treelog& err) const
{ return impl.check (n, err); }

void 
SoilWater::output (Log& log) const
{ impl.output (log); }

double
SoilWater::MaxExfiltration (const Soil& soil, double T) const
{ return impl.MaxExfiltration (soil, T); }

void 
SoilWater::put_h (const Soil& soil, const vector<double>& v) // [cm]
{ impl.put_h (soil, v); }

void 
SoilWater::get_sink (vector<double>& v) const // [cm^3/cm^3/h]
{ v = impl.S_sum; }

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
  syntax.add ("S_incorp", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Incorporated water sink, typically from subsoil irrigation.");
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
	      Syntax::OptionalState, Syntax::Singleton,
	      "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon if above 5%.");
}

SoilWater::SoilWater (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

void
SoilWater::initialize (const AttributeList& al,
		       const Soil& soil, const Groundwater& groundwater, 
		       Treelog& out)
{ impl.initialize (al, soil, groundwater, out); }

SoilWater::~SoilWater ()
{ delete &impl; }

static Submodel::Register 
soil_water_submodel ("SoilWater", SoilWater::load_syntax);
