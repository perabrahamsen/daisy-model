// om.C

#include "om.h"
#include "syntax.h"
#include "alist.h"
#include "geometry.h"
#include "log.h"
#include "mathlib.h"
#include "submodel.h"
#include <numeric>

static double
get_initial_C_per_N (const AttributeList& al)
{
  if (al.check ("initial_C_per_N"))
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
    top_C (al.number ("top_C")),
    top_N (al.number ("top_N")),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{ 
  if (al.check ("C_per_N"))
    C_per_N = al.number_sequence ("C_per_N");
}

void
OM::output (Log& log) const
{
  if (initial_C_per_N != Unspecified)
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

  if (initial_C_per_N != Unspecified)
    {
      // Fixed C/N.
     for (unsigned i = 0; i < C.size (); i++)
	{
	  if (N[i] == 0.0)
	    assert (C[i] == 0.0);
	  else if (!approximate (initial_C_per_N, C[i] / N[i]))
	    CERR << "Bug: init C/N (" << initial_C_per_N 
		 << ") != " << C[i] << "/" << N[i] << " (" << C[i]/N[i] 
		 << ") for layer " << i << ".\n";
	  
	  if (i < C_per_N.size ())
	    assert (approximate (C_per_N[i], initial_C_per_N));
	  else
	    C_per_N.push_back (initial_C_per_N);
	}
    }
  else
    {
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

#if 0
void 
OM::distribute (const Geometry& geometry, const vector<double>& content)
{
  // Ignore empty pools.
  if (top_C == 0.0)
    return;

  // Prepare.
  assert (C.size () == content.size ());
  const double total = accumulate (content.begin (), content.end (), 0.0);

  // Calcaluate N.
  vector<double> N = get_N ();

  // Distribute C.
  for (unsigned int i = 0; i < content.size (); i++)
    C[i] += top_C * content[i] / total / geometry.dz (i);
  top_C = 0;

  // Distribute N.
  for (unsigned int i = 0; i < content.size (); i++)
    N[i] += top_N * content[i] / total / geometry.dz (i);
  top_N = 0;

  // Calculate C/N.
  set_N (N);
}
#endif 

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
OM::add (const Geometry& geometry, // Add dead roots.
	 double to_C, /* Fixed C/N */
	 const vector<double>& density)
{
  const double old_C = total_C (geometry);

  // Make sure C/N is large enough.
  int extra_C_per_N = density.size () - C_per_N.size ();
  if (extra_C_per_N > 0)
    {
      assert (initial_C_per_N != Unspecified);
      while (extra_C_per_N > 0)
	{
	  C_per_N.push_back (initial_C_per_N);
	  extra_C_per_N--;
	}
      assert (initial_C_per_N > 0.0);
    }
  
  // Make sure C is large enough.
  const int extra_C = density.size () - C.size ();
  if (extra_C > 0)
    C.insert (C.end (), extra_C, 0.0);
  
  assert (approximate (old_C, total_C (geometry)));

  // Distribute it according to the root density.
  const double total = geometry.total (density);
  for (unsigned int i = 0; i < density.size (); i++)
    {
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

  // Make sure C/N is large enough.
  const int extra_C_per_N = density.size () - C_per_N.size ();
  if (extra_C_per_N > 0)
    // It doesn't matter what number we use, add C will be 0.
    C_per_N.insert (C_per_N.end (), extra_C_per_N, 1.0);
  
  // Make sure C is large enough.
  const int extra_C = density.size () - C.size ();
  if (extra_C > 0)
    {
      assert (extra_C_per_N <= extra_C);
      C.insert (C.end (), extra_C, 0.0);
    }

  assert (approximate (old_C, total_C (geometry)));

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
      double rate = min (factor[i] * fraction, 0.1);
      assert (C[i] >= 0.0);
      assert (finite (rate));
      assert (rate >=0);
      // assert (N_soil * 1.001 >= N_used);
      assert (C_per_N[i] > 0.0);
      assert (om.C_per_N[i] > 0.0);
      double N_produce = C[i] * rate / C_per_N[i];
      double N_consume = C[i] * rate * efficiency / om.C_per_N[i];
      assert (finite (N_produce));
      assert (finite (N_consume));
      if (N_consume - N_produce > N_soil - N_used
	  // && N_soil * 1.001 >= N_used
	  && ((N_consume - N_produce) - (N_soil[i] - N_used[i])
	      > 1.0e-10) // Lose 1 g / ha / year
	  && rate > 0.0)
	{
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
	  assert ((rate == 0)
		  ? true 
		  : ((N_soil[i] == N_used[i])
		     ? (fabs (N_consume - N_produce) < 1e-10)
		     : (fabs (1.0 - (N_consume - N_produce) 
			      / (N_soil[i] - N_used[i]))
			< 0.01)));
	}
      // Update.
      assert (om.C[i] >= 0.0);
      const double C_use = C[i] * rate;
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency;
      C[i] -= C_use;
      assert (om.C[i] >= 0.0);
      assert (C[i] >= 0.0);
      N_used[i] += (N_consume - N_produce);

      // Check for NaN.
      assert (finite (N_used[i]));
      assert (finite (rate));
      assert (finite (efficiency));
      // assert (N_soil * 1.001 >= N_used);
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

  for (unsigned int i = 0; i < size; i++)
    {
      assert (C[i] >= 0.0);
      // assert (N_soil * 1.001 >= N_used);
      // Maintenance.
      const double C_use = C[i] * maintenance * abiotic_factor[i];
      CO2[i] += C_use;
      C[i] -= C_use;
      N_used[i] -= C_use / C_per_N[i];
      // assert (N_soil * 1.001 >= N_used);
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
  // assert (N_soil * 1.001 >= N_used);
  for (unsigned int i = 0; i < size; i++)
    assert (C[i] >= 0.0);
}

void 
OM::tick (unsigned int end, const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<OM*>& smb, double* som_C, double* som_N)
{
  const unsigned int size = min (C.size (), end);
  assert (C_per_N.size () >= size);
  
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
      som_N[i] += C[i] * rate / C_per_N[i];
      som_C[i] += C[i] * rate;
      C[i] *= (1.0 - rate);
      // assert (N_soil * 1.001 >= N_used);
      
      if (C[i] < 1e-9)
	{
	  assert (C[i] > -1e9);
	  som_C[i] += C[i];
	  C[i] = 0.0;
	}
      assert (C[i] >= 0.0);
      assert (som_C[i] >= 0.0);
      assert (som_N[i] >= 0.0);
    }
}

const double OM::Unspecified = -1042.42e42;

static bool check_alist (const AttributeList& al)
{
  bool ok = true;

  non_negative (al.number ("top_C"), "top_C", ok);
  non_negative (al.number ("top_N"), "top_N", ok);
  non_negative (al.number ("turnover_rate"), "turnover_rate", ok);
  if (al.check ("C"))
    {
      const vector<double>& C = al.number_sequence ("C");
      for (unsigned int i = 0; i < C.size (); i++)
	non_negative (C[i], "C", ok, i);
    }
  if (al.check ("C_per_N"))
    {
      const vector<double>& C_per_N = al.number_sequence ("C_per_N");
      for (unsigned int i = 0; i < C_per_N.size (); i++)
	if (C_per_N[i] <= 0.0)
	  {
	    CERR << "C_per_N[" << i << "] is not positive\n";
	    ok = false;
	  }
    }
  const vector<double>& efficiency = al.number_sequence ("efficiency");
  for (unsigned int i = 0; i < efficiency.size (); i++)
    is_fraction (efficiency[i], "efficiency", ok, i);
  non_negative (al.number ("maintenance"), "maintenance", ok);
  const vector<double>& fractions = al.number_sequence ("fractions");
  for (unsigned int i = 0; i < fractions.size (); i++)
    is_fraction (fractions[i], "fractions", ok, i);
  if (!approximate (accumulate (fractions.begin (), fractions.end (), 0.0),
		    1.0))
    {
      CERR << "Sum of `fractions' must be 1.0\n";
      ok = false;
    }
  if (al.check ("initial_C_per_N"))
    if (al.number ("initial_C_per_N") <= 0.0)
      {
	CERR << "`initial_C_per_N' must be positive\n";
	ok = false;
      }
  const double initial_fraction = al.number ("initial_fraction");
  if (initial_fraction != OM::Unspecified
      && initial_fraction < 0.0 || initial_fraction > 1.0)
    {
      CERR << "Initial fraction should be unspecified, or between 0 and 1\n";
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
such as `maintenance', are only meaningful for certain kinds of organic\n\
matter, in this case the SMB pools.");
  syntax.add ("top_C", "g C/cm^2", Syntax::State,
	      "Carbon on top of soil.");
  alist.add ("top_C", 0.0);
  syntax.add ("top_N", "g N/cm^2", Syntax::State,
	      "Nitrogen on top of soil.");
  alist.add ("top_N", 0.0);
  syntax.add ("C", "g C/cm^3", Syntax::OptionalState, Syntax::Sequence,
	      "Carbon in each soil interval.");
  syntax.add ("C_per_N", "(g C/cm^3)/(g N/cm^3)",
	      Syntax::OptionalState, Syntax::Sequence, 
	      "The carbon/nitrogen ratio.");
  syntax.add ("N", "g N/cm^3", Syntax::LogOnly, Syntax::Sequence,
	      "Nitrogen in each soil interval.");
  syntax.add ("turnover_rate", "h^-1", Syntax::Const,
	      "Fraction converted to other pools each hour.");
  syntax.add ("efficiency", Syntax::Fraction (),
	      Syntax::Const, Syntax::Sequence,
	      "\
the efficiency this pool can be digested by each of the SMB pools.");
  syntax.add ("maintenance", "h^-1", Syntax::Const, "\
The fraction used for staying alive each hour.");
  alist.add ("maintenance", 0.0);
  syntax.add ("fractions", Syntax::Fraction (),
	      Syntax::Const, Syntax::Sequence, "\
How this pool is divided into other pools.\n\
The first numbers corresponds to each of the SMB pools, the remaining\n\
numbers corresponds to the SOM pools.  The length of the sequence should\n\
thus be the number of SMB pools plus the number of SOM pools.");
  syntax.add ("initial_C_per_N", "g C/g N", Syntax::OptionalState, "\
The initial C/N ratio when this pool is created.");
  syntax.add ("initial_fraction", Syntax::None (), Syntax::Const, "\
The initial fraction of the total available carbon\n\
allocated to this pool for AOM.  One pool should be left unspecified\n\
\(which corresponds to the default value, a large negative number).");
  alist.add ("initial_fraction", Unspecified);
}

static Submodel::Register om_submodel ("OM", OM::load_syntax);
