// om.C -- A single unspecified organic matter pool.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
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


#include "om.h"
#include "som.h"
#include "smb.h"
#include "dom.h"
#include "syntax.h"
#include "alist.h"
#include "check.h"
#include "geometry.h"
#include "log.h"
#include "mathlib.h"
#include "tmpstream.h"
#include "assertion.h"
#include <numeric>

void
OM::output (Log& log) const
{
  log.output ("initial_C_per_N", initial_C_per_N); // For checkpoint
  log.output ("C", C);
  log.output ("N", N);
  if (log.check_member ("C_per_N"))
    {
      vector<double> C_per_N;
      unsigned int size = N.size ();
      daisy_assert (C.size () >= size);
      for (int i = 0; i < size; i++)
	{
	  if (N[i] == 0.0)
	    C_per_N.push_back (Unspecified);
	  else
	    C_per_N.push_back (C[i] / N[i]);
	}
      log.output ("C_per_N", C_per_N);
    }
}

void 
OM::mix (const Geometry& geometry, double from, double to)
{
  // Ignore tiny pools.
  if (soil_C (geometry) < 1e-20)
    return;

  // Mix.
  geometry.mix (C, from, to);
  geometry.mix (N, from, to);
}

void
OM::swap (const Geometry& geometry, double from, double middle, double to)
{
  // Ignore tiny pools.
  if (soil_C (geometry) < 1e-20)
    return;

  // Swap.
  geometry.swap (C, from, middle, to);
  geometry.swap (N, from, middle, to);
}

double 
OM::soil_C (const Geometry& geometry) const
{ return geometry.total (C); }

double 
OM::soil_N (const Geometry& geometry) const
{ return geometry.total (N); }

double 
OM::goal_C_per_N (unsigned int at) const // Desired C/N ratio.
{
  daisy_assert (C_per_N_goal.size () > at);
  return C_per_N_goal[at];
}

void
OM::tock (unsigned int end, const double* factor,
	  double fraction, double efficiency,
	  const double* N_soil, double* N_used, double* CO2, OM& om)
{
  const unsigned int size = min (C.size (), end);
  daisy_assert (N.size () >= size);
  // Maintenance.
  for (unsigned int i = 0; i < size; i++)
    {
      const double N_avail = N_soil[i] - N_used[i];
      double rate = min (factor[i] * fraction, 0.1);
      daisy_assert (C[i] >= 0.0);
      daisy_assert (finite (rate));
      daisy_assert (rate >=0);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (N[i] >= 0.0);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (om.C[i] >= 0.0);
      const double om_C_per_N_goal = om.goal_C_per_N (i);
      double N_produce = N[i] * rate;
      double N_consume = C[i] * rate * efficiency / om_C_per_N_goal;
      daisy_assert (finite (N_produce));
      daisy_assert (finite (N_consume));
      if (N_consume - N_produce > N_avail)
	{
	  if (N_avail < 1e-12) // Less than 1 [ug/l] to use...
	    continue;

	  // Lower rate to force 
	  //   N_consume - N_produce == N_soil - N_used 
	  // This is what calc tell me:
	  rate = (N_soil[i] - N_used[i]) 
	    / (efficiency * C[i] / om_C_per_N_goal - N[i]);
	  daisy_assert (finite (rate));
	  if (rate < 0)
	    rate = 0;

	  // Aside: We could also have solved the equation by decreasing the 
	  // efficiency.
	  //   efficiency = ((N_soil - N_used) + rate * N[i])
	  //     * om_C_per_N_goal / rate * C[i];
	  // But we don't

	  // Update the N values.
	  N_produce = N[i] * rate;
	  N_consume = C[i] * rate * efficiency / om_C_per_N_goal;
	  daisy_assert (finite (N_produce));
	  daisy_assert (finite (N_consume));
	  // Check that we calculated the right rate.
	  daisy_assert (approximate (N_consume - N_produce, N_avail));

	  // Upddate N.
	  N_used[i] = N_soil[i];
	}
      else
	// Upddate N.
	N_used[i] += (N_consume - N_produce);
	
      // Update C.
      daisy_assert (om.C[i] >= 0.0);
      const double C_use = C[i] * rate;
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency;
      C[i] -= C_use;
      daisy_assert (om.C[i] >= 0.0);
      daisy_assert (C[i] >= 0.0);

      // Update N.
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
      om.N[i] += N_consume;
      N[i] -= N_produce;
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
      
      // Check for NaN.
      daisy_assert (finite (N_used[i]));
      daisy_assert (finite (rate));
      daisy_assert (finite (efficiency));
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
    }
}

void
OM::tick (unsigned int end, const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<SMB*>& smb, const vector<SOM*>&som,
	  const vector<DOM*>& dom)
{
  const unsigned int size = min (C.size (), end);
  daisy_assert (N.size () >= size);

  const unsigned int smb_size = smb.size ();
  const unsigned int som_size = som.size ();
  const unsigned int dom_size = dom.size ();
  daisy_assert (fractions.size () == smb_size + som_size + dom_size);
  // Distribute to all biological pools.
  for (unsigned int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j]);
    }
  // Distribute to all soil pools.
  for (unsigned int j = 0; j < som_size; j++)
    {
      const double fraction = fractions[smb_size + j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, 1.0,
	      N_soil, N_used, CO2, *som[j]);
    }
  // Distribute to all dissolved pools.
#ifdef FIXED_DOM_C_per_N
  for (unsigned int j = 0; j < dom_size; j++)
    {
      const double fraction = fractions[smb_size + som_size + j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, 1.0,
	      N_soil, N_used, CO2, *dom[j]);
    }
#else // Variable DOM C/N
  for (unsigned int j = 0; j < dom_size; j++)
    {
      const double factor = turnover_rate * fractions[smb_size + som_size + j];
      if (factor > 1e-50)
	{
	  for (unsigned int i = 0; i < size; i++)
	    {
	      const double rate = min (factor * abiotic_factor[i], 0.1);
	      const double C_use = C[i] * rate;
	      const double N_use = N[i] * rate;
	      dom[j]->N[i] += N_use;
	      dom[j]->C[i] += C_use;
	      C[i] -= C_use;
	      N[i] -= N_use;
	      daisy_assert (C[i] >= 0.0);
	      daisy_assert (dom[j]->C[i] >= 0.0);
	      daisy_assert (dom[j]->N[i] >= 0.0);
	    }
	}
    }
#endif // Variable DOM C/N
  for (unsigned int i = 0; i < size; i++)
    {
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (C[i] >= 0.0);
    }
}

void 
OM::grow (unsigned int size)
{
  // Make sure C is large enough.
  const int extra_C = size - C.size ();
  if (extra_C > 0)
    C.insert (C.end (), extra_C, 0.0);

  // Make sure N is large enough.
  const int extra_N = size - N.size ();
  if (extra_N > 0)
    N.insert (N.end (), extra_N, 0.0);
}

const double OM::Unspecified = -1042.42e42;

static bool check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  const vector<double>& fractions = al.number_sequence ("fractions");
  if (!approximate (accumulate (fractions.begin (), fractions.end (), 0.0),
		    1.0))
    {
      err.entry ("Sum of 'fractions' must be 1.0");
      ok = false;
    }
  const double initial_fraction = al.number ("initial_fraction");
  if (initial_fraction != OM::Unspecified
      && initial_fraction < 0.0 || initial_fraction > 1.0)
    {
      err.entry ("Initial fraction should be unspecified, or between 0 and 1");
      ok = false;
    }
  if (!al.check ("turnover_rate") && !al.check ("turnover_halftime"))
    {
      err.entry ("\
You must specify 'turnover_rate' or 'turnover_halftime'");
      ok = false;
	
    }
  if (al.check ("turnover_rate") && al.check ("turnover_halftime"))
    {
      err.entry ("\
You may not specify both 'turnover_rate' and 'turnover_halftime'");
      ok = false;
    }

  if (al.check ("C") && al.check ("C_per_N"))
    {
      const vector<double>& C = al.number_sequence ("C");
      const vector<double>& C_per_N = al.number_sequence ("C_per_N");
      
      if (C_per_N.size () > C.size () && C_per_N.size () > 1)
	{
	  err.entry ("\
You cannot specify 'C_per_N' for intervals where 'C' is unspecified.");
	  ok = false;
	}
      else if (al.check ("N") && C.size () > 0)
	{
	  bool bogus = false;
	  const vector<double>& N = al.number_sequence ("N");

	  const int check_size = min (N.size (), C_per_N.size ());
	  for (unsigned int i = 0; i < check_size; i++)
	    {
	      if (C_per_N[i] < 0.0)
		/* do nothing */;
	      else if (N[i] == 0)
		bogus = true;
	      else if (!approximate (C[i] / N[i], C_per_N[i]))
		bogus = true;
	    }
	  if (bogus)
	    {
	      err.entry ("'C' / 'N' is inconsitent with 'C_per_N'");
	      ok = false;
	    }
	}
    }

  return ok;
}

void
OM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
  alist.add ("submodel", "OM");
  syntax.add ("C", "g C/cm^3", Check::non_negative (),
	      Syntax::OptionalState, Syntax::Sequence,
	      "Carbon in each soil interval.");
  syntax.add ("C_per_N", "(g C/cm^3)/(g N/cm^3)", Check::none (), 
	      Syntax::OptionalState, Syntax::Sequence, 
	      "The carbon/nitrogen ratio.");
  syntax.add ("N", "g N/cm^3", Check::non_negative (),
	      Syntax::OptionalState, Syntax::Sequence,
	      "Nitrogen in each soil interval.");
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
How this pool is divided into other pools.\n\
The first numbers corresponds to each of the SMB pools, the remaining\n\
numbers corresponds to the SOM pools.  The length of the sequence should\n\
thus be the number of SMB pools plus the number of SOM pools.");
  syntax.add ("initial_C_per_N", "g C/g N", Syntax::OptionalState, "\
The initial C/N ratio when this pool is created.\n\
Negative numbers mean unspecified.");
  syntax.add ("initial_fraction", Syntax::None (), Syntax::Const, "\
The initial fraction of the total available carbon\n\
allocated to this pool for AOM.  One pool should be left unspecified\
\n(which corresponds to the default value, a large negative number).");
  alist.add ("initial_fraction", Unspecified);
  syntax.add ("heat_factor", "dg C", Syntax::None (), Syntax::OptionalConst,
	      "Heat factor.  If empty, use default from 'OrganicMatter'.");
  syntax.add ("water_factor", "cm", Syntax::None (), Syntax::OptionalConst, "\
Water potential factor.  If empty, use default from 'OrganicMatter'.");
}

double
OM::get_initial_C_per_N (const AttributeList& al)
{
  if (al.check ("initial_C_per_N"))
    if (al.number ("initial_C_per_N") < 0.0)
      return OM::Unspecified;
    else
      return al.number ("initial_C_per_N");
  if (al.check ("C_per_N"))
    {
      const vector<double>& C_per_N = al.number_sequence ("C_per_N");
      if (C_per_N.size () > 0U)
	return C_per_N[0];
    }
  if (al.check ("C") && al.check ("N"))
    {
      const vector<double>& C = al.number_sequence ("C");
      const vector<double>& N = al.number_sequence ("N");
      
      if (C.size () > 0 && C[0] > 0 && N.size () > 0 && N[0] > 0)
	return C[0] / N[0];
    }
  return OM::Unspecified;
}

OM::OM (const AttributeList& al)
  : initial_fraction (al.number ("initial_fraction")),
    initial_C_per_N (get_initial_C_per_N (al)),
    turnover_rate (al.check ("turnover_rate")
		   ? al.number ("turnover_rate")
		   : halftime_to_rate (al.number ("turnover_halftime"))),
    efficiency (al.number_sequence ("efficiency")),
    fractions (al.number_sequence ("fractions"))
{ 
  if (al.check ("heat_factor"))
    heat_factor = al.plf ("heat_factor");
  if (al.check ("water_factor"))
    water_factor = al.plf ("water_factor");
  if (al.check ("C"))
    C = al.number_sequence ("C");
  if (al.check ("N"))
    N = al.number_sequence ("N");

  if (al.check ("C_per_N"))
    {
      const vector<double>& C_per_N = al.number_sequence ("C_per_N");
      if (C.size () > 0)
	{
	  daisy_assert (C_per_N.size () >= C.size ());
	  for (unsigned int i = N.size (); i < C_per_N.size (); i++)
	    {
	      if (C_per_N[i] > 0)
		N.push_back (C[i] / C_per_N[i]);
	      else
		N.push_back (0.0);
	    }
	}
    }
}

OM::~OM ()
{ }

