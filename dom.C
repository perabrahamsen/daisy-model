// dom.C --- A single dissolved organic matter pool.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#include "dom.h"
#include "submodel.h"
#include "alist.h"
#include "syntax.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "check.h"
#include "mathlib.h"
#include "tmpstream.h"

class DOM::Element
{
  // Content.
public:
  vector<double> M;		// Concentration in soil [g / cm³]
  vector<double> C;		// Concentration in soil solution [g / cm³]
  vector<double> S;		// Combined source term.
  vector<double> S_p;		// Source term for macropores only.
  vector<double> S_drain;	// Source term for soil drainage only.
  vector<double> J;		// Solute transport log in matrix.
  vector<double> J_p;		// Solute transport log in macropores.

  // Simulation.
public:
  void output (Log&) const;
  void mix (const Soil&, const SoilWater&, Adsorption&,
	    double from, double to);
  void swap (const Soil&, const SoilWater&, Adsorption&, 
	     double from, double middle, double to);
  void transport (const Soil& soil, 
		  const SoilWater& soil_water, 
		  Transport& trans,
		  Transport& reserve,
		  Transport& last_resort,
		  Mactrans& mactrans,		
		  Adsorption& adsorption,
		  double diffusion_coefficient,
		  Treelog& msg);
  
  // Create and Destroy.
public:
  static Submodel::Register dom_element_submodel;
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const Soil&, const SoilWater&, Adsorption&, Treelog&);
  Element (const AttributeList& al);
  ~Element ();
};

void 
DOM::Element::output (Log& log) const
{
  output_variable (M, log);
  output_variable (C, log);
  output_variable (S, log);
  output_variable (S_p, log);
  output_variable (S_drain, log);
  output_variable (J, log);
  output_variable (J_p, log);
}

void 
DOM::Element::mix (const Soil& soil, const SoilWater& soil_water, 
		   Adsorption& adsorption,
		   double from, double to)
{
  soil.mix (M, from, to);
  for (unsigned int i = 0; i < C.size (); i++)
    C[i] = adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]);
}

void 
DOM::Element::swap (const Soil& soil, const SoilWater& soil_water,
		    Adsorption& adsorption,
		    double from, double middle, double to)
{
  soil.swap (M, from, middle, to);
  for (unsigned int i = 0; i < C.size (); i++)
    C[i] = adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]);
}

void 
DOM::Element::transport (const Soil& soil, 
			 const SoilWater& soil_water, 
			 Transport& trans,
			 Transport& reserve,
			 Transport& last_resort,
			 Mactrans& mactrans,		
			 Adsorption& adsorption,
			 double diffusion_coefficient,
			 Treelog& msg)
{
  // Initialize.
  fill (S_p.begin (), S_p.end (), 0.0);
  fill (J_p.begin (), J_p.end (), 0.0);

  // Drainage.
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      S_drain[i] = -soil_water.S_drain (i) * dt * C[i];
      S[i] += S_drain[i];
    }

  // Flow.
  const double old_content = soil.total (M);
  mactrans.tick (soil, soil_water, M, C, S, S_p, J_p, msg);

  try
    {
      trans.tick (msg, soil, soil_water, adsorption, 
		  diffusion_coefficient, 
		  M, C, S, J);
    }
  catch (const char* error)
    {
      msg.warning (string ("Transport problem: ") + error +
		   ", trying reserve.");
      try
	{
	  reserve.tick (msg, soil, soil_water, adsorption, 
			diffusion_coefficient, M, C, S, J);
	}
      catch (const char* error)
	{
	  msg.warning (string ("Reserve transport problem: ") + error
		       + ", trying last resort.");
	  last_resort.tick (msg, soil, soil_water, adsorption, 
			    diffusion_coefficient, M, C, S, J);
	}
    }
  const double new_content = soil.total (M);
  const double delta_content = new_content - old_content;
  const double source = soil.total (S);
  const double in = -J[0];	// No preferential transport, it is 
  const double out = -J[soil.size ()]; // included in S.
  const double expected = source + in - out;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10)
    {
      TmpStream tmp;
      tmp () << __FILE__ << ":" << __LINE__ << ": DOM"
	     << ": mass balance new - old != source + in - out\n"
	     << new_content << " - " << old_content << " != " 
	     << source << " + " << in << " - " << out << " (error "
	     << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
}

void 
DOM::Element::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Submodel.
  alist.add ("submodel", "DOM-Element");
  alist.add ("description", "\
A single element in a Dissolved Organic Matter pool.");

  // Content.
  syntax.add ("M", "g/cm^3", Syntax::State, Syntax::Sequence,
	      "Mass in water and soil.");
  syntax.add ("C", "g/cm^3", Syntax::LogOnly, Syntax::Sequence,
	      "Concentration in water.");
  syntax.add ("S", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Combined source term.");
  syntax.add ("S_p", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (macropore transport only).");
  syntax.add ("S_drain", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Source term (soil drainage only).");
  syntax.add ("J", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in matrix (positive up).");
  syntax.add ("J_p", "g/cm^2/h", Syntax::LogOnly, Syntax::Sequence,
	      "Transportation in macropores (positive up).");
}

void 
DOM::Element::initialize (const Soil& soil, const SoilWater& soil_water,
			  Adsorption& adsorption, Treelog& msg)
{
  if (soil.size () >= M.size ())
    M.insert (M.end (), soil.size () - M.size (), 0.0);
  else
    msg.warning ("Too many elements of M in DOM pool");

  for (unsigned int i = C.size (); i < M.size (); i++)
    C.push_back (adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]));

  S.insert (S.begin (), soil.size (), 0.0);
  S_p.insert (S_p.begin (), soil.size (), 0.0);
  S_drain.insert (S_drain.begin (), soil.size (), 0.0);
  J.insert (J_p.begin (), soil.size () + 1, 0.0);
  J_p.insert (J_p.begin (), soil.size () + 1, 0.0);
}

DOM::Element::Element (const AttributeList& al)
  : M (al.number_sequence ("M"))
{ }

DOM::Element::~Element ()
{ }

Submodel::Register 
DOM::Element::dom_element_submodel ("DOM-Element", DOM::Element::load_syntax);

void 
DOM::output (Log& log) const
{
  output_submodule (C, "C", log);
  output_submodule (N, "N", log);
  output_derived (trans, "transport", log);
  output_derived (adsorption, "adsorption", log);
}

void 
DOM::mix (const Soil& soil, const SoilWater& soil_water,
	  double from, double to)
{
  C.mix (soil, soil_water, adsorption, from, to);
  N.mix (soil, soil_water, adsorption, from, to);
}

void 
DOM::swap (const Soil& soil, const SoilWater& soil_water,
	   double from, double middle, double to)
{
  C.swap (soil, soil_water, adsorption, from, middle, to);
  N.swap (soil, soil_water, adsorption, from, middle, to);
}

void
DOM::add_to_source (unsigned int at, double to_C, double to_N)
{
  C.S[at] += to_C;
  N.S[at] += to_N;
}

double 
DOM::soil_C (const Geometry& geometry) const
{ return geometry.total (C.M); }

double 
DOM::soil_N (const Geometry& geometry) const
{ return geometry.total (N.M); }

double
DOM::C_source (const Geometry& geometry) const
{ return geometry.total (C.S); }

double
DOM::N_source (const Geometry& geometry) const
{ return geometry.total (N.S); }

double 
DOM::C_at (unsigned int at) const
{ return C.M[at]; }
  
double 
DOM::N_at (unsigned int at) const
{ return N.M[at]; }

void
DOM::clear ()
{
  // Clear sources.
  fill (C.S.begin (), C.S.end (), 0.0);
  fill (N.S.begin (), N.S.end (), 0.0);
}

void 
DOM::turnover (unsigned int end, const double* turnover_factor, 
	       const double* N_soil, double* N_used,
	       double* CO2, const vector<SMB*>& smb)
{
  // Find size.
  const unsigned int size = min (C.M.size (), end);
  daisy_assert (N.M.size () >= size);
  const unsigned int smb_size = smb.size ();
  daisy_assert (fractions.size () == smb_size);
  // Distribute to all biological pools.
  for (unsigned int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (size, turnover_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j]);
    }
}

void
DOM::tock (unsigned int end, 
	   const double* factor, double fraction, double efficiency,
	   const double* N_soil, double* N_used, double* CO2, OM& om)
{
  const unsigned int size = min (C.M.size (), end);
  daisy_assert (N.M.size () >= size);

  for (unsigned int i = 0; i < size; i++)
    {
      double rate = min (factor[i] * fraction, 0.1);
      daisy_assert (C.M[i] >= 0.0);
      daisy_assert (isfinite (rate));
      daisy_assert (rate >=0);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (N.M[i] >= 0.0);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (om.C[i] >= 0.0);
      double C_use;
      double N_produce;
      double N_consume;
      
      OM::turnover (C.M[i], N.M[i], om.goal_C_per_N (i), N_soil[i] - N_used[i],
		    min (factor[i] * fraction, 0.1), efficiency,
		    C_use, N_produce, N_consume);
      add_to_source (i, -C_use, -N_produce);

      // Update C.
      daisy_assert (om.C[i] >= 0.0);
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency;
      daisy_assert (om.C[i] >= 0.0);
      daisy_assert (C.M[i] >= 0.0);

      // Update N.
      N_used[i] += (N_consume - N_produce);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N.M[i] >= 0.0);
      om.N[i] += N_consume;
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N.M[i] >= 0.0);
    }
}

void 
DOM::transport (const Soil& soil, const SoilWater& soil_water, Treelog& msg)
{
  C.transport (soil, soil_water, 
	       trans, reserve, last_resort, mactrans, 
	       adsorption, diffusion_coefficient,
	       msg);
  N.transport (soil, soil_water, 
	       trans, reserve, last_resort, mactrans, 
	       adsorption, diffusion_coefficient,
	       msg);
}

void 
DOM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Submodel.
  alist.add ("submodel", "DOM");
  alist.add ("description", "\
A single Dissolved Organic Matter pool.");

  // Content.
  syntax.add_submodule ("C", alist, Syntax::State,
			"Carbon content of DOM pool.",
			DOM::Element::load_syntax);
  syntax.add_submodule ("N", alist, Syntax::State,
			"Nitrogen content of DOM pool.",
			DOM::Element::load_syntax);

  // Transport
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
  syntax.add ("diffusion_coefficient", "cm^2/s", Check::positive (),
	      Syntax::Const, "Diffusion coefficient.");

  // Turnover.
  syntax.add ("heat_factor", "dg C", Syntax::None (), Syntax::OptionalConst,
	      "Heat factor.  If empty, use default from 'OrganicMatter'.");
  syntax.add ("turnover_rate", "h^-1", Check::fraction (), 
	      Syntax::OptionalConst,
	      "Fraction converted to other pools each hour.\n\
You must specify either this or 'turnover_halftime'.");
  syntax.add ("turnover_halftime", "h", Check::positive (), 
	      Syntax::OptionalConst,
	      "Time until half had been converted to other pools.\n\
You must specify either this or 'turnover_rate'.");
  syntax.add_fraction ("efficiency", Syntax::Const, Syntax::Sequence, "\
the efficiency this pool can be digested by each of the SMB pools.");
  syntax.add_fraction ("fractions", Syntax::Const, Syntax::Sequence, "\
Fraction of this pool that ends up in each SMB pools");
}

void
DOM::initialize (const Soil& soil, const SoilWater& soil_water, Treelog& msg)
{ 
  C.initialize (soil, soil_water, adsorption, msg);
  N.initialize (soil, soil_water, adsorption, msg);
}

DOM::DOM (const AttributeList& al)
  : C (*new Element (al.alist ("C"))),
    N (*new Element (al.alist ("N"))),
    trans (Librarian<Transport>::create (al.alist ("transport"))),
    reserve (Librarian<Transport>::create (al.alist ("reserve"))),
    last_resort (Librarian<Transport>::create (al.alist ("last_resort"))),
    mactrans  (Librarian<Mactrans>::create (al.alist ("mactrans"))),
    adsorption (Librarian<Adsorption>::create (al.alist ("adsorption"))),
    diffusion_coefficient (al.number ("diffusion_coefficient")),
    turnover_rate (al.check ("turnover_rate")
		   ? al.number ("turnover_rate")
		   : halftime_to_rate (al.number ("turnover_halftime"))),
    efficiency (al.number_sequence ("efficiency")),
    fractions (al.number_sequence ("fractions"))
{
  if (al.check ("heat_factor"))
    heat_factor = al.plf ("heat_factor");
}

DOM::~DOM ()
{ 
  delete &C;
  delete &N;
  delete &trans; 
  delete &reserve; 
  delete &last_resort;
  delete &mactrans;
  delete &adsorption; 
}

static Submodel::Register dom_submodel ("DOM", DOM::load_syntax);
