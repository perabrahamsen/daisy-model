// om.C
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


#include "om.h"
#include "syntax.h"
#include "alist.h"
#include "check.h"
#include "geometry.h"
#include "log.h"
#include "mathlib.h"
#include "tmpstream.h"
#include "submodel.h"
#include "assertion.h"
#include <numeric>

void
OM::output (Log& log) const
{
  log.output ("initial_C_per_N", initial_C_per_N); // For checkpoint
  log.output ("top_C", top_C);
  log.output ("top_N", top_N);
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
OM::mix (const Geometry& geometry, double from, double to, double penetration)
{
  daisy_assert (penetration >= 0.0);
  daisy_assert (penetration <= 1.0);
  daisy_assert (top_C >= 0.0);
  daisy_assert (top_N >= 0.0);

  // Ignore tiny pools.
  if (total_C (geometry) < 1e-20)
    return;

  // Mix C.
  geometry.add (C, from, to, top_C * penetration);
  geometry.mix (C, from, to);
  top_C *= (1.0 - penetration);
  daisy_assert (top_C >= 0.0);

  // Mix N.
  geometry.add (N, from, to, top_N * penetration);
  geometry.mix (N, from, to);
  top_N *= (1.0 - penetration);
  daisy_assert (top_N >= 0.0);
}

void
OM::swap (const Geometry& geometry, double from, double middle, double to)
{
  daisy_assert (top_C >= 0.0);
  daisy_assert (top_N >= 0.0);

  // Ignore tiny pools.
  if (total_C (geometry) < 1e-20)
    return;

  // Swap.
  geometry.swap (C, from, middle, to);
  geometry.swap (N, from, middle, to);
}

double 
OM::total_C (const Geometry& geometry) const
{
  return geometry.total (C) + top_C;
}

double 
OM::total_N (const Geometry& geometry) const
{
  return geometry.total (N) + top_N;
}

double 
OM::C_at (unsigned int at) const
{
  if (at >= C.size ())
    return 0.0;
  return C[at];
}

double 
OM::N_at (unsigned int at) const
{
  if (at >= N.size ())
    return 0.0;
  return N[at];
}

double 
OM::goal_C_per_N (unsigned int at) const // Desired C/N ratio.
{
  daisy_assert (C_per_N_goal.size () > at);
  return C_per_N_goal[at];
}

void
OM::pour (vector<double>& cc, vector<double>& nn)
{
  const unsigned int size = C.size ();
  daisy_assert (N.size () >= size);
  daisy_assert (cc.size () >= size);
  daisy_assert (nn.size () >= size);
  for (unsigned int i = 0; i < size; i++)
    {
      cc[i] += C[i];
      C[i] = 0.0;
      nn[i] += N[i];
      N[i] = 0.0;
    }
}

void 
OM::add (double C, double N)
{
  daisy_assert (C >= 0.0);
  daisy_assert (N >= 0.0);
  top_C += C;
  top_N += N;
}

void 
OM::add (unsigned int at, double to_C, double to_N)
{
  grow (at+1);
  C[at] += to_C;
  N[at] += to_N;
  daisy_assert (finite (C[at]));
  daisy_assert (finite (N[at]));
}

void 
OM::add (const Geometry& geometry, // Add dead roots.
	 double to_C, double to_N, 
	 const vector<double>& density)
{
  const double old_C = total_C (geometry);
  const double old_N = total_N (geometry);
  grow (density.size ());

  // Distribute it according to the root density.
  const double total = geometry.total (density);
  for (unsigned int i = 0; i < density.size (); i++)
    {
      // We should *not* multiply with dz here.  Reason: We want to
      // divide C on the total depth.  
      const double factor = density[i] /* * geometry.dz (i) */ / total;
      daisy_assert (factor >= 0.0);
      N[i] += to_N * factor;
      C[i] += to_C * factor;
      daisy_assert (finite (C[i]));
      daisy_assert (C[i] >= 0.0);
      daisy_assert (finite (N[i]));
      daisy_assert (N[i] >= 0.0);
    }

  // Check that we computed the correct value.
  const double new_C = total_C (geometry);
  const double new_N = total_N (geometry);
  daisy_assert (to_C * 1e9 < old_C
	  ? approximate (old_C + to_C, new_C)
	  : (approximate (new_C - old_C, to_C)));
  daisy_assert (to_N * 1e9 < old_N
	  ? approximate (old_N + to_N, new_N)
	  : (approximate (new_N - old_N, to_N)));
}

inline void
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
	  double* CO2, const vector<OM*>& smb, const vector<OM*>&som,
	  const vector<OM*>& dom)
{
  const unsigned int size = min (C.size (), end);
  daisy_assert (N.size () >= size);

  if (maintenance != 0.0)
    {
      // SMB maintenance.
      for (unsigned int i = 0; i < size; i++)
	{
	  daisy_assert (C[i] >= 0.0);
	  daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
	  // Maintenance.
	  const double C_use = C[i] * maintenance * abiotic_factor[i];
	  const double N_use = N[i] * maintenance * abiotic_factor[i];
	  CO2[i] += C_use;
	  C[i] -= C_use;
	  N[i] -= N_use;
	  N_used[i] -= N_use;
	  daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
	}
    }

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
  for (unsigned int j = 0; j < dom_size; j++)
    {
      const double fraction = fractions[smb_size + som_size + j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, 1.0,
	      N_soil, N_used, CO2, *dom[j]);
    }
  for (unsigned int i = 0; i < size; i++)
    {
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (C[i] >= 0.0);
    }
}

void 
OM::tick (unsigned int end, const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<OM*>& smb, double* som_C, double* som_N,
	  const vector<OM*>& dom)
{
  const unsigned int size = min (C.size (), end);
  daisy_assert (N.size () >= size);
  daisy_assert (maintenance == 0.0);
  
  const unsigned int smb_size = smb.size ();
  const unsigned int dom_size = dom.size ();
  daisy_assert (fractions.size () >= smb_size + 1);

  // Distribute to all biological pools.
  for (unsigned int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j]);
    }

  // Distribute to soil buffer.
  const double factor = turnover_rate * fractions[smb_size];
  for (unsigned int i = 0; i < size; i++)
    {
      const double rate = min (factor * abiotic_factor[i], 0.1);
      const double C_use = C[i] * rate;
      const double N_use = N[i] * rate;
      som_N[i] += N_use;
      som_C[i] += C_use;
      C[i] -= C_use;
      N[i] -= N_use;
      daisy_assert (C[i] >= 0.0);
      daisy_assert (som_C[i] >= 0.0);
      daisy_assert (som_N[i] >= 0.0);
    }

  if (fractions.size () == smb_size + 1)
    return;
  daisy_assert (fractions.size () == smb_size + 1 + dom_size);

  // Distribute to all dissolved pools.
  for (unsigned int j = 0; j < dom_size; j++)
    {
      const double fraction = fractions[smb_size + 1 + j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *dom[j]);
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
  alist.add ("description", "\
Organic matter.  This is a common abstraction for the SMB (Soil\n\
MicroBiomass), SOM (Soil Organic Matter) DOM (Dissolved Organic Matter)\n\
and AOM (Added Organic Matter) pools.  That is, all the organic matter\n\
in the soil.  Some attributes, such as 'maintenance', are only meaningful\n\
for certain kinds of organic matter, in this case the SMB pools.");
  syntax.add ("top_C", "g C/cm^2", Check::non_negative (), Syntax::State,
	      "Carbon on top of soil.");
  alist.add ("top_C", 0.0);
  syntax.add ("top_N", "g N/cm^2", Check::non_negative (), Syntax::State,
	      "Nitrogen on top of soil.");
  alist.add ("top_N", 0.0);
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
  syntax.add ("maintenance", "h^-1", Check::fraction (), Syntax::Const, "\
The fraction used for staying alive each hour.");
  alist.add ("maintenance", 0.0);
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
    top_C (al.number ("top_C")),
    top_N (al.number ("top_N")),
    turnover_rate (al.check ("turnover_rate")
		   ? al.number ("turnover_rate")
		   : halftime_to_rate (al.number ("turnover_halftime"))),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
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

static Submodel::Register om_submodel ("OM", OM::load_syntax);
