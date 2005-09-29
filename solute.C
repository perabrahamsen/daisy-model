// solute.C
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


#include "solute.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "soil.h"
#include "soil_water.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

double
Solute::total (const Geometry& geometry, double from, double to) const
{ return geometry.total (M_, from, to); }

void
Solute::clear ()
{
  fill (S.begin (), S.end (), 0.0);
  fill (S_external.begin (), S_external.end (), 0.0);
  fill (S_root.begin (), S_root.end (), 0.0);
  fill (tillage.begin (), tillage.end (), 0.0);
}

void
Solute::add_to_source (const vector<double>& v)
{
  daisy_assert (S.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S[i] += v[i];
      daisy_assert (isfinite (S[i]));
      daisy_assert (M_left (i) >= 0.0);
    }
}

void
Solute::add_to_sink (const vector<double>& v)
{
  daisy_assert (S.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S[i] -= v[i];
      daisy_assert (isfinite (S[i]));
      daisy_assert (M_left (i) >= 0.0);
    }
}

void
Solute::add_to_root_sink (const vector<double>& v)
{
  daisy_assert (S_root.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    S_root[i] -= v[i];
  add_to_sink (v);
}

void 
Solute::tick (const Soil& soil, 
	      const SoilWater& soil_water, 
	      double J_in, Treelog& msg)
{
  for (unsigned i = 0; i < soil.size (); i++)
    daisy_assert (M_left (i) >= 0.0);

  // Initialize.
  fill (S_p.begin (), S_p.end (), 0.0);
  fill (J_p.begin (), J_p.end (), 0.0);

  // Permanent source.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      S_external[i] += S_permanent[i];
      S[i] += S_external[i];
      daisy_assert (M_left (i) >= 0.0);
    }

  // Upper border.
  if (soil_water.q_p (0) < 0.0)
    {
      if (soil_water.q (0) >= 0.0)
	{
	  if (soil_water.q (0) > 1.0e-10)
	    {
	      std::ostringstream tmp;
	      tmp << "BUG: q_p[0] = " << soil_water.q_p (0) 
		     << " and q[0] = " << soil_water.q (0);
	      msg.error (tmp.str ());
	    }
	  J_p[0] = J_in;
	  J[0] = J_in;
	}
      else
	{
	  const double macro_fraction
	    = soil_water.q_p (0) / (soil_water.q_p (0) + soil_water.q (0));
	  J_p[0] = J_in * macro_fraction;
	  J[0] = J_in - J_p[0];
	}
    }
  else
    J[0] = J_in;

  // Drainage.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      S_drain[i] = -soil_water.S_drain (i) * dt * C (i);
      S[i] += S_drain[i];
      daisy_assert (M_left (i) >= 0.0);
    }

  // Flow.
  const double old_content = soil.total (M_);
  mactrans->tick (soil, soil_water, M_, C_, S, S_p, J_p, msg);

  try
    {
      for (unsigned i = 0; i < soil.size (); i++)
	daisy_assert (M_left (i) >= 0.0);
      transport->tick (msg, soil, soil_water, *adsorption, 
                       diffusion_coefficient (), 
                       M_, C_, S, J);
    }
  catch (const char* error)
    {
      msg.warning (string ("Transport problem: ") + error +
		   ", trying reserve.");
      try
	{
	  for (unsigned i = 0; i < soil.size (); i++)
	    daisy_assert (M_left (i) >= 0.0);
	  reserve->tick (msg, soil, soil_water, *adsorption, 
                         diffusion_coefficient (), M_, C_, S, J);
	}
      catch (const char* error)
	{
	   msg.warning (string ("Reserve transport problem: ") + error
			+ ", trying last resort.");
	  for (unsigned i = 0; i < soil.size (); i++)
	    daisy_assert (M_left (i) >= 0.0);
	  last_resort->tick (msg, soil, soil_water, *adsorption, 
                             diffusion_coefficient (), M_, C_, S, J);
	}
    }
  const double new_content = soil.total (M_);
  const double delta_content = new_content - old_content;
  const double source = soil.total (S);
  const double in = -J[0];	// No preferential transport, it is 
  const double out = -J[soil.size ()]; // included in S.
  const double expected = source + in - out;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ":" << submodel
	     << ": mass balance new - old != source + in - out\n"
	     << new_content << " - " << old_content << " != " 
	     << source << " + " << in << " - " << out << " (error "
	     << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
}

bool 
Solute::check (unsigned, Treelog&) const
{
  bool ok = true;
  return ok;
}

void
Solute::output (Log& log) const
{
  output_derived (transport, "transport", log);
  output_derived (adsorption, "adsorption", log);
  output_value (C_, "C", log);
  output_value (M_, "M", log);
  output_variable (S, log);
  output_variable (S_p, log);
  output_variable (S_drain, log);
  output_variable (S_external, log);
  output_variable (S_permanent, log);
  output_variable (S_root, log);
  output_variable (J, log);
  output_variable (J_p, log);
  output_variable (tillage, log);
}

static bool check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  daisy_assert (al.check ("transport"));
  daisy_assert (al.check ("adsorption"));

  if (al.alist ("adsorption").name ("type") == "full"
      && al.alist ("transport").name ("type") != "none")
    {
      err.entry ("You can't have any transport with full adsorption");
      ok = false;
    }
  return ok;
}

void 
Solute::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  syntax.add ("transport", Librarian<Transport>::library (), 
	      "Solute transport model in matrix.");
  AttributeList cd;
  cd.add ("type", "cd");
  cd.add ("max_time_step_reductions", 20);
  alist.add ("transport", cd);
  syntax.add ("reserve", Librarian<Transport>::library (),
	      "Reserve transport model if the primary model fails.");
  AttributeList convection;
  convection.add ("type", "convection");
  convection.add ("max_time_step_reductions", 10);
  alist.add ("reserve", convection);
  syntax.add ("last_resort", Librarian<Transport>::library (),
	      "Last resort transport model if the reserve model fails.");
  AttributeList none;
  none.add ("type", "none");
  alist.add ("last_resort", none);

  syntax.add ("mactrans", Librarian<Mactrans>::library (), 
	      "Solute transport model in macropores.");
  AttributeList mactrans;
  mactrans.add ("type", "default");
  alist.add ("mactrans", mactrans);
  syntax.add ("adsorption", Librarian<Adsorption>::library (), 
	      "Soil adsorption properties.");
  Geometry::add_layer (syntax, Syntax::OptionalState, "C", Syntax::Fraction (),
                       "Concentration in water.\n\
This number does not include adsorped matter.");
  Geometry::add_layer (syntax, Syntax::OptionalState, "M", "g/cm^3", 
                       "Total mass per volume water, soil and air.\n\
This number include both adsorped and dissolved matter.");
  Geometry::add_layer (syntax, Syntax::OptionalConst,
                       "Ms", Syntax::Fraction (), "Mass in dry soil.\n\
This include all matter in both soil and water, relative to the\n\
dry matter weight.\n\
Only for initialization of the 'M' parameter.");
  syntax.add ("S", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term.");
  syntax.add ("S_p", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (macropore transport only).");
  syntax.add ("S_drain", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (soil drainage only).");
  syntax.add ("S_external", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "External source, such as incorporated fertilizer.");
  syntax.add ("S_permanent", "g/cm^3/h", Syntax::State, Syntax::Sequence,
	      "Permanent external source, e.g. subsoil irrigation.");
  vector<double> empty;
  alist.add ("S_permanent", empty);
  syntax.add ("S_root", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (root uptake only, always negative).");
  syntax.add ("J", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in matrix (positive up).");
  syntax.add ("J_p", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in macropores (positive up).");
  syntax.add ("tillage", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Changes during tillage.");
}

Solute::Solute (const AttributeList& al)
  : submodel (al.name ("submodel")),
    S_permanent (al.number_sequence ("S_permanent")),
    transport (Librarian<Transport>::create (al.alist ("transport"))),
    reserve (Librarian<Transport>::create (al.alist ("reserve"))),
    last_resort (Librarian<Transport>::create (al.alist ("last_resort"))),
    mactrans  (Librarian<Mactrans>::create (al.alist ("mactrans"))),
    adsorption (Librarian<Adsorption>::create (al.alist ("adsorption")))
{ }

Solute::~Solute ()
{ 
}

void 
Solute::incorporate (const Geometry& geometry, 
                     double amount, double from, double to)
{ 
  daisy_assert (amount >= 0.0);
  daisy_assert (from <= 0.0);
  daisy_assert (to <= from);
  geometry.add (S_external, from, to, amount);
}

void 
Solute::set_external_source (const Geometry& geometry, 
			     double amount, double from, double to)
{
  fill (S_permanent.begin (), S_permanent.end (), 0.0);
  geometry.add (S_permanent, from, to, amount);
}

void 
Solute::mix (const Soil& soil, const SoilWater& soil_water, 
	     double from, double to)
{ 
  soil.mix (M_, from, to, tillage);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::swap (const Soil& soil, const SoilWater& soil_water,
	      double from, double middle, double to)
{ 
  soil.swap (M_, from, middle, to, tillage);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::put_M (const Soil& soil, const SoilWater& soil_water,
	       const vector<double>& v)
{
  const unsigned int size = soil.size ();
  daisy_assert (M_.size () == size);
  daisy_assert (C_.size () == size);
  daisy_assert (v.size () == size);

  M_ = v;

  for (unsigned int i = 0; i < size; i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::default_initialize (const Soil& soil, const SoilWater&)
{ 
  M_.insert (M_.begin (), soil.size (), 0.0);
}

void
Solute::initialize (const AttributeList& al, 
		    const Soil& soil, const SoilWater& soil_water, 
		    Treelog& out)
{
  vector<double> Ms;
  soil.initialize_layer (C_, al, "C", out);
  soil.initialize_layer (M_, al, "M", out);
  soil.initialize_layer (Ms, al, "Ms", out);

  if (C_.size () > 0)
    {
      // Fill it up.
      while (C_.size () < soil.size ())
	C_.push_back (C_[C_.size () - 1]);
      if (C_.size () > soil.size ())
	throw ("To many members of C sequence");
    }
  if (M_.size () > 0)
    {
      // Fill it up.
      while (M_.size () < soil.size ())
	M_.push_back (M_[M_.size () - 1]);
      if (M_.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (Ms.size () > 0)
    {
      // Fill it up.
      while (Ms.size () < soil.size ())
	Ms.push_back ( Ms[Ms.size () - 1]);
      if (Ms.size () > soil.size ())
	throw ("To many members of Ms sequence");
    }
  if (M_.size () == 0 && C_.size () == 0)
    {
      if (Ms.size () != 0)
	{
	  daisy_assert (Ms.size () == soil.size ());

	  for (unsigned int i = M_.size (); i < Ms.size (); i++)
	    M_.push_back (Ms[i] * soil.dry_bulk_density (i));
	}
      if (M_.size () == 0 && C_.size () == 0)
	{
	  default_initialize (soil, soil_water);
	}
    }
  for (unsigned int i = C_.size (); i < M_.size (); i++)
    C_.push_back (M_to_C (soil, soil_water.Theta (i), i, M_[i]));
  for (unsigned int i = M_.size (); i < C_.size (); i++)
    M_.push_back (C_to_M (soil, soil_water.Theta (i), i, C_[i]));

  daisy_assert (C_.size () == M_.size ());
  daisy_assert (C_.size () == soil.size ());

  for (unsigned int i = 0; i < soil.size (); i++)
    {
      if (M_[i] == 0.0)
	{
	  if (C_[i] != 0.0)
	    throw ("C & M mismatch in solute");
	}
      else
	{
	  
	  if (!approximate (C_[i],
			    M_to_C (soil, soil_water.Theta (i), i, M_[i])))
	    throw ("Solute C does not match M");
	}
    }

  S.insert (S.begin (), soil.size (), 0.0);
  S_p.insert (S_p.begin (), soil.size (), 0.0);
  S_drain.insert (S_drain.begin (), soil.size (), 0.0);
  S_external.insert (S_external.begin (), soil.size (), 0.0);
  if (S_permanent.size () < soil.size ())
    S_permanent.insert (S_permanent.end (), 
			soil.size () - S_permanent.size (),
			0.0);
  S_root.insert (S_root.begin (), soil.size (), 0.0);
  J.insert (J_p.begin (), soil.size () + 1, 0.0);
  J_p.insert (J_p.begin (), soil.size () + 1, 0.0);
  tillage.insert (tillage.begin (), soil.size (), 0.0);
}
