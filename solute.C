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
#include "tmpstream.h"

double
Solute::total (const Geometry& geometry, double from, double to) const
{ return geometry.total (M_, from, to); }

void
Solute::clear ()
{
  fill (S.begin (), S.end (), 0.0);
  fill (S_external.begin (), S_external.end (), 0.0);
}

void
Solute::add_to_source (const vector<double>& v)
{
  daisy_assert (S.size () >= v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S[i] += v[i];
      daisy_assert (finite (S[i]));
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
      daisy_assert (finite (S[i]));
      daisy_assert (M_left (i) >= 0.0);
    }
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
      S[i] += S_permanent[i];
      daisy_assert (M_left (i) >= 0.0);
      S_external[i] += S_permanent[i];
    }

  // Upper border.
  if (soil_water.q_p (0) < 0.0)
    {
      if (soil_water.q (0) >= 0.0)
	{
	  if (soil_water.q (0) > 1.0e-10)
	    {
	      TmpStream tmp;
	      tmp () << "BUG: q_p[0] = " << soil_water.q_p (0) 
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
  mactrans.tick (soil, soil_water, M_, C_, S, S_p, J_p, msg);

  try
    {
      for (unsigned i = 0; i < soil.size (); i++)
	daisy_assert (M_left (i) >= 0.0);
      transport.tick (msg, soil, soil_water, *this, M_, C_, S, J);
    }
  catch (const char* error)
    {
      msg.warning (string ("Transport problem: ") + error +
		   ", trying reserve.");
      try
	{
	  for (unsigned i = 0; i < soil.size (); i++)
	    daisy_assert (M_left (i) >= 0.0);
	  reserve.tick (msg, soil, soil_water, *this, M_, C_, S, J);
	}
      catch (const char* error)
	{
	   msg.warning (string ("Reserve transport problem: ") + error
			+ ", trying last resort.");
	  for (unsigned i = 0; i < soil.size (); i++)
	    daisy_assert (M_left (i) >= 0.0);
	  last_resort.tick (msg, soil, soil_water, *this, M_, C_, S, J);
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
      TmpStream tmp;
      tmp () << __FILE__ << ":" << __LINE__ << ":" << submodel
	     << ": mass balance new - old != source + in - out\n"
	     << new_content << " - " << old_content << " != " 
	     << source << " + " << in << " - " << out << " (error "
	     << delta_content - expected << ")";
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
  log.output ("C", C_);
  log.output ("M", M_);
  log.output ("S", S);
  log.output ("S_p", S_p);
  log.output ("S_drain", S_drain);
  log.output ("S_external", S_external);
  log.output ("S_permanent", S_permanent);
  log.output ("J", J);
  log.output ("J_p", J_p);
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
  Geometry::add_layer (syntax, "C", "g/cm^3", "Concentration in water.");
  Geometry::add_layer (syntax, "M", "g/cm^3", "Mass in water and soil.");
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
  syntax.add ("J", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in matrix (positive up).");
  syntax.add ("J_p", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in macropores (positive up).");
  Geometry::add_layer (syntax, "soil_ppm", "ppm", "Concentration in water.\n\
Only for initialization of the 'M' parameter.");
  Geometry::add_layer (syntax, "solute_ppm", "ppm", "Mass in water and soil.\n\
Only used for initialization of the 'C' parameter.");
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
  delete &transport; 
  delete &reserve; 
  delete &last_resort;
  delete &mactrans;
  delete &adsorption; 
}

void 
Solute::add_external (const Soil& soil, const SoilWater& soil_water, 
		      double amount, double from, double to)
{ 
  vector<double> added (M_.size (), 0.0);
  soil.add (added, from, to, amount);
  for (unsigned int i = 0; i < C_.size (); i++)
    {
      M_[i] += added[i];
      S_external[i] += added[i];
      C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
    }
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
  soil.mix (M_, from, to);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::swap (const Soil& soil, const SoilWater& soil_water,
	      double from, double middle, double to)
{ 
  soil.swap (M_, from, middle, to);
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
  vector<double> soil_ppm;
  vector<double> solute_ppm;
  soil.initialize_layer (C_, al, "C", out);
  soil.initialize_layer (M_, al, "M", out);
  soil.initialize_layer (soil_ppm, al, "soil_ppm", out);
  soil.initialize_layer (solute_ppm, al, "solute_ppm", out);

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
  if (soil_ppm.size () > 0)
    {
      // Fill it up.
      while (soil_ppm.size () < soil.size ())
	soil_ppm.push_back ( soil_ppm[soil_ppm.size () - 1]);
      if (soil_ppm.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (solute_ppm.size () > 0)
    {
      // Fill it up.
      while (solute_ppm.size () < soil.size ())
	solute_ppm.push_back ( solute_ppm[solute_ppm.size () - 1]);
      if (solute_ppm.size () > soil.size ())
	throw ("To many members of M sequence");
    }
  if (M_.size () == 0 && C_.size () == 0)
    {
      if (soil_ppm.size () != 0)
	{
	  daisy_assert (soil_ppm.size () == soil.size ());

	  for (unsigned int i = M_.size (); i < soil_ppm.size (); i++)
	    // ppm -> g / cm^3.
	    M_.push_back (1.0e-6 * soil_ppm[i] * soil.dry_bulk_density (i));
	}
      if (solute_ppm.size () != 0)
	{
	  daisy_assert (solute_ppm.size () == solute_ppm.size ());

	  for (unsigned int i = M_.size (); i < solute_ppm.size (); i++)
	    // ppm -> g / cm^3.
	    C_.push_back (1.0e-6 * solute_ppm[i]);
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
  J.insert (J_p.begin (), soil.size () + 1, 0.0);
  J_p.insert (J_p.begin (), soil.size () + 1, 0.0);
}
