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
#include <numeric>

void
OM::output (Log& log) const
{
  log.output ("initial_C_per_N", initial_C_per_N); // For checkpoint
  log.output ("top_C", top_C);
  log.output ("top_N", top_N);
  log.output ("C", C);
  if (log.check ("N"))
    {
      vector<double> N;
      unsigned int size = C.size ();
      assert (C_per_N.size () >= size);
      for (int i = 0; i < size; i++)
	{
	  if (C[i] != 0.0)
	    {
	      assert (C_per_N[i] > 0);
	      N.push_back (C[i] / C_per_N[i]);
	    }
	}
      log.output ("N", N);
    }
  // These are sometimes const and should be read from the AM library.
  log.output ("C_per_N", C_per_N);
#if 0
  log.output ("turnover_rate", turnover_rate);
  log.output ("efficiency", efficiency);
  log.output ("maintenance", maintenance);
  log.output ("fractions", fractions);
#endif
}

vector<double>
OM::get_N () const
{
  assert (C_per_N.size () >= C.size ());
  vector<double> N;
  
  for (unsigned i = 0; i < C.size (); i++)
  {
    assert (C_per_N[i] >= 0.0);
    N.push_back (C[i] / C_per_N[i]);
  }
  return N;
}

void
OM::set_N (vector<double>& N) 
{
  assert (N.size () == C.size ());

  // Calculate C/N.
  C_per_N.erase (C_per_N.begin (), C_per_N.end ());

  for (unsigned i = 0; i < C.size (); i++)
    {
      if (C[i] == 0.0)
	{
	  assert (N[i] == 0.0);
	  C_per_N.push_back (1.0); // Arbitrary.
	}
      else
	{
	  assert (C[i] > 0.0);
	  assert (N[i] > 0.0);
	  C_per_N.push_back (C[i] / N[i]);
	}
    }
}

void 
OM::mix (const Geometry& geometry, double from, double to, double penetration)
{
  assert (penetration >= 0.0);
  assert (penetration <= 1.0);
  assert (top_C >= 0.0);
  assert (top_N >= 0.0);

  // Ignore tiny pools.
  if (total_C (geometry) < 1e-20)
    return;

  // Calcaluate N.
  vector<double> N = get_N ();

  // Mix C.
  geometry.add (C, from, to, top_C * penetration);
  geometry.mix (C, from, to);
  top_C *= (1.0 - penetration);
  assert (top_C >= 0.0);

  // Mix N.
  geometry.add (N, from, to, top_N * penetration);
  geometry.mix (N, from, to);
  top_N *= (1.0 - penetration);
  assert (top_N >= 0.0);

  // Calculate C/N.
  set_N (N);
}

void
OM::swap (const Geometry& geometry, double from, double middle, double to)
{
  assert (top_C >= 0.0);
  assert (top_N >= 0.0);

  // Ignore tiny pools.
  if (total_C (geometry) < 1e-20)
    return;

  // Calcaluate N.
  vector<double> N = get_N ();

  // Swap.
  geometry.swap (C, from, middle, to);
  geometry.swap (N, from, middle, to);

  // Calculate C/N.
  set_N (N);
}

double 
OM::total_C (const Geometry& geometry) const
{
  return geometry.total (C) + top_C;
}

double 
OM::total_N (const Geometry& geometry) const
{
  double total = 0.0;
  const unsigned int size = C.size ();
  assert (C_per_N.size () >= size);

  for (unsigned int i = 0; i < size; i++)
    {
      assert (C_per_N[i] > 0.0);
      total += (C[i] / C_per_N[i]) * geometry.dz (i);
    }
  return total + top_N;
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
  if (at >= C.size ())
    return 0.0;
  assert (at < C_per_N.size ());
  assert ( C_per_N[at] > 0.0);
  return C[at] / C_per_N[at];
}

void
OM::pour (vector<double>& cc, vector<double>& nn)
{
  const unsigned int size = C.size ();
  assert (C_per_N.size () >= size);
  assert (cc.size () >= size);
  assert (nn.size () >= size);
  for (unsigned int i = 0; i < size; i++)
    {
      cc[i] += C[i];
      assert (C_per_N[i] > 0.0);
      nn[i] += C[i] / C_per_N[i];
      C[i] = 0.0;
    }
}

void 
OM::add (double C, double N)
{
  assert (C >= 0.0);
  assert (N >= 0.0);
  top_C += C;
  top_N += N;
}

void 
OM::add (unsigned int at, double to_C)
{
  grow (at+1);
  C[at] += to_C;
}

void 
OM::add (unsigned int at, double to_C, double to_N)
{
  grow (at+1);

  const double new_N = C[at] / C_per_N[at] + to_N;
  C[at] += to_C;
  if (C[at] > 0.0)
    C_per_N[at] = C[at] / new_N;
  else
    assert (new_N == 0.0);
  assert (finite (C[at]));
  assert (C[at] >= 0.0);
  assert (finite (C_per_N[at]));
  assert (C_per_N[at] >= 0.0);
}

void 
OM::add (const Geometry& geometry, // Add dead roots.
	 double to_C, /* Fixed C/N */
	 const vector<double>& density)
{
  const double old_C = total_C (geometry);
  grow (density.size ());

  // Distribute it according to the root density.
  const double total = geometry.total (density);
  for (unsigned int i = 0; i < density.size (); i++)
    {
      assert (approximate (C_per_N[i], initial_C_per_N));
      // We should *not* multiply with dz here.  Reason: We want to
      // divide C on the total depth.  
      C[i] += to_C * density[i] /* * geometry.dz (i) */ / total;
      assert (density[i] >= 0.0);
      assert (finite (C[i]));
      assert (C[i] >= 0.0);
    }

  // Check that we computed the correct value.
  const double new_C = total_C (geometry);
  assert (to_C * 1e9 < old_C
	  ? approximate (old_C + to_C, new_C)
	  : (approximate (new_C - old_C, to_C)));
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
      assert (factor >= 0.0);
      const double new_N = C[i] / C_per_N[i] + to_N * factor;
      C[i] += to_C * factor;
      if (C[i] > 0.0)
	C_per_N[i] = C[i] / new_N;
      else
	assert (new_N == 0.0);
      assert (finite (C[i]));
      assert (C[i] >= 0.0);
      assert (finite (C_per_N[i]));
      assert (C_per_N[i] >= 0.0);
    }

  // Check that we computed the correct value.
  const double new_C = total_C (geometry);
  const double new_N = total_N (geometry);
  assert (to_C * 1e9 < old_C
	  ? approximate (old_C + to_C, new_C)
	  : (approximate (new_C - old_C, to_C)));
  assert (to_N * 1e9 < old_N
	  ? approximate (old_N + to_N, new_N)
	  : (approximate (new_N - old_N, to_N)));
}

inline void
OM::tock (unsigned int end, const double* factor,
	  double fraction, double efficiency,
	  const double* N_soil, double* N_used, double* CO2, OM& om)
{
  const unsigned int size = min (C.size (), end);
  assert (C_per_N.size () >= size);

  // Maintenance.
  for (unsigned int i = 0; i < size; i++)
    {
      const double N_avail = N_soil[i] - N_used[i];
      double rate = min (factor[i] * fraction, 0.1);
      assert (C[i] >= 0.0);
      assert (finite (rate));
      assert (rate >=0);
      assert (N_soil[i] * 1.001 >= N_used[i]);
      assert (C_per_N[i] > 0.0);
      assert (om.C_per_N[i] > 0.0);
      double N_produce = C[i] * rate / C_per_N[i];
      double N_consume = C[i] * rate * efficiency / om.C_per_N[i];
      assert (finite (N_produce));
      assert (finite (N_consume));
      if (N_consume - N_produce > N_avail)
	{
	  if (N_avail < 1e-12) // Less than 1 [ug/l] to use...
	    continue;

	  // Lower rate to force 
	  //   N_consume - N_produce == N_soil - N_used 
	  // This is what calc tell me:
	  rate = (N_soil[i] - N_used[i]) 
	    / (efficiency * C[i] / om.C_per_N[i] - C[i] / C_per_N[i]);
	  assert (finite (rate));
	  if (rate < 0)
	    rate = 0;

	  // Aside: We could also have solved the equation by decresing the 
	  // efficiency.
	  //   efficiency = ((N_soil - N_used) + rate * C[i] / C_per_N[i])
	  //     * om.C_per_N / rate * C[i];
	  // But we don't

	  // Update the N values.
	  N_produce = C[i] * rate / C_per_N[i];
	  N_consume = C[i] * rate * efficiency / om.C_per_N[i];
	  assert (finite (N_produce));
	  assert (finite (N_consume));
	  // Check that we calculated the right rate.
	  assert (approximate (N_consume - N_produce, N_avail));

	  // Upddate N.
	  N_used[i] = N_soil[i];
	}
      else
	// Upddate N.
	N_used[i] += (N_consume - N_produce);
	
      // Update C.
      assert (om.C[i] >= 0.0);
      const double C_use = C[i] * rate;
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency;
      C[i] -= C_use;
      assert (om.C[i] >= 0.0);
      assert (C[i] >= 0.0);

      // Check for NaN.
      assert (finite (N_used[i]));
      assert (finite (rate));
      assert (finite (efficiency));
      assert (N_soil[i] * 1.001 >= N_used[i]);
      assert (C[i] >= 0.0);
    }
}

void
OM::tick (unsigned int end, const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<OM*>& smb, const vector<OM*>&som)
{
  const unsigned int size = min (C.size (), end);
  assert (C_per_N.size () >= size);

  if (maintenance != 0.0)
    {
      // SMB maintenance.
      for (unsigned int i = 0; i < size; i++)
	{
	  assert (C[i] >= 0.0);
	  assert (N_soil[i] * 1.001 >= N_used[i]);
	  // Maintenance.
	  const double C_use = C[i] * maintenance * abiotic_factor[i];
	  CO2[i] += C_use;
	  C[i] -= C_use;
	  N_used[i] -= C_use / C_per_N[i];
	  assert (N_soil[i] * 1.001 >= N_used[i]);
	}
    }

  assert (fractions.size () == smb.size () + som.size ());
  // Distribute to all biological pools.
  const unsigned int smb_size = smb.size ();
  for (unsigned int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j]);
    }
  // Distribute to all soil pools.
  const unsigned int som_size = som.size ();
  for (unsigned int j = 0; j < som_size; j++)
    {
      const double fraction = fractions[smb_size + j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, 1.0,
	      N_soil, N_used, CO2, *som[j]);
    }
  for (unsigned int i = 0; i < size; i++)
    {
      assert (N_soil[i] * 1.001 >= N_used[i]);
      assert (C[i] >= 0.0);
    }
}

void 
OM::tick (unsigned int end, const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<OM*>& smb, double* som_C, double* som_N)
{
  const unsigned int size = min (C.size (), end);
  assert (C_per_N.size () >= size);

  assert (maintenance == 0.0);
#if 0
  // Maintenance.
  for (unsigned int i = 0; i < size; i++)
    {
      assert (C[i] >= 0.0);

      if (C[i] > 0.0)
	{
	  CO2[i] += C[i] * maintenance;
	  C[i] *= (1.0 - maintenance);
	}
    }
  assert (fractions.size () == smb.size () + 1);
#endif
  
  // Distribute to all biological pools.
  const unsigned int smb_size = smb.size ();
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
      const double N_use = C_use / C_per_N[i];
      som_N[i] += N_use;
      som_C[i] += C_use;
      C[i] -= C_use;
#if 0      
      if (C[i] < 1e-9)
	{
	  assert (C[i] > -1e9);
	  som_C[i] += C[i];
	  som_N[i] += C[i] / C_per_N[i];
	  C[i] = 0.0;
	}
#endif
      assert (C[i] >= 0.0);
      assert (som_C[i] >= 0.0);
      assert (som_N[i] >= 0.0);
    }
}

void 
OM::grow (unsigned int size)
{
  // Make sure C/N is large enough.
  int extra_C_per_N = size - C_per_N.size ();
  if (extra_C_per_N > 0)
    {
      if (initial_C_per_N == Unspecified)
	C_per_N.insert (C_per_N.end (), extra_C_per_N, 1.0);
      else
	C_per_N.insert (C_per_N.end (), extra_C_per_N, initial_C_per_N);
    }

  // Make sure C is large enough.
  const int extra_C = size - C.size ();
  if (extra_C > 0)
    C.insert (C.end (), extra_C, 0.0);
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


  return ok;
}

void
OM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
  alist.add ("submodel", "OM");
  alist.add ("description", "\
Organic matter.  This is a common abstraction for the SMB (Soil\n\
MicroBiomass), SOM (Soil Organic Matter) and AOM (Added Organic Matter)\n\
pools.  That is, all the organic matter in the soil.  Some attributes,\n\
such as 'maintenance', are only meaningful for certain kinds of organic\n\
matter, in this case the SMB pools.");
  syntax.add ("top_C", "g C/cm^2", Check::non_negative (), Syntax::State,
	      "Carbon on top of soil.");
  alist.add ("top_C", 0.0);
  syntax.add ("top_N", "g N/cm^2", Check::non_negative (), Syntax::State,
	      "Nitrogen on top of soil.");
  alist.add ("top_N", 0.0);
  syntax.add ("C", "g C/cm^3", Check::non_negative (),
	      Syntax::OptionalState, Syntax::Sequence,
	      "Carbon in each soil interval.");
  syntax.add ("C_per_N", "(g C/cm^3)/(g N/cm^3)", Check::positive (), 
	      Syntax::OptionalState, Syntax::Sequence, 
	      "The carbon/nitrogen ratio.");
  syntax.add ("N", "g N/cm^3", Syntax::LogOnly, Syntax::Sequence,
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
  PLF empty;
  syntax.add ("heat_factor", "dg C", Syntax::None (), Syntax::Const,
	      "Heat factor.  If empty, use default from 'OrganicMatter'.");
  alist.add ("heat_factor", empty);
  syntax.add ("water_factor", "cm", Syntax::None (), Syntax::Const, "\
Water potential factor.  If empty, use default from 'OrganicMatter'.");
  alist.add ("heat_factor", empty);
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
  return OM::Unspecified;
}

OM::OM (const AttributeList& al)
  : initial_fraction (al.number ("initial_fraction")),
    initial_C_per_N (get_initial_C_per_N (al)),
    heat_factor (al.plf ("heat_factor")),
    water_factor (al.plf ("water_factor")),
    top_C (al.number ("top_C")),
    top_N (al.number ("top_N")),
    turnover_rate (al.check ("turnover_rate")
		   ? al.number ("turnover_rate")
		   : halftime_to_rate (al.number ("turnover_halftime"))),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{ 
  if (al.check ("C_per_N"))
    C_per_N = al.number_sequence ("C_per_N");
  if (al.check ("C"))
    C = al.number_sequence ("C");
}

static Submodel::Register om_submodel ("OM", OM::load_syntax);
