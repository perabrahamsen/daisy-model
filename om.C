// om.C

#include "om.h"
#include "syntax.h"
#include "alist.h"
#include "geometry.h"
#include "log.h"
#include "mathlib.h"
#include <numeric>

OM::OM (const AttributeList& al)
  : initial_fraction (al.number ("initial_fraction")),
    top_C (al.number ("top_C")),
    top_N (al.number ("top_N")),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{
  assert (!al.check ("C"));

  if (al.check ("C_per_N"))
    {
      C_per_N = al.number_sequence ("C_per_N");
      if (C_per_N.size () > 0U)
	initial_C_per_N =  C_per_N[0];
    }
  else
    initial_C_per_N = Unspecified;
}

OM::OM (const AttributeList& al, const Geometry& geometry)
  : initial_fraction (al.number ("initial_fraction")),
    top_C (al.number ("top_C")),
    top_N (al.number ("top_N")),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{
  if (al.check ("C"))
    C = al.number_sequence ("C");
  if (al.check ("C_per_N"))
    C_per_N = al.number_sequence ("C_per_N");

  // Create initial C.
  while (C.size () < geometry.size ())
    C.push_back (0.0);

  // Create initial C/N.
  if (C_per_N.size () > 0U)
    {
      initial_C_per_N = C_per_N[0];
      while (C_per_N.size () < C.size ())
	C_per_N.push_back (C_per_N[C_per_N.size () - 1]);
    }
  else
    initial_C_per_N = Unspecified;
}

OM::OM (const AttributeList& al, const Geometry& geometry, 
	const double carbon, const double N)
  : initial_fraction (al.number ("initial_fraction")),
    initial_C_per_N (Unspecified),
    top_C (carbon),
    top_N (N),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{
  assert (carbon >= 0.0);
  assert (N >= 0.0);
  
  double CpN;

  if (N == 0)
    {
      assert (carbon == 0);
      CpN = 0;
    }
  else
    CpN = carbon / N;

  // Create initial C.
  assert (!al.check ("C"));
  while (C.size () < geometry.size ())
    C.push_back (0.0);

  // Initialize C/N.
  assert (!al.check ("C_per_N"));
  while (C_per_N.size () < C.size ())
    C_per_N.push_back (CpN);
}

void
OM::output (Log& log, Filter& filter) const
{
#if 0
  log.output ("initial_fraction", filter, initial_fraction);
  log.output ("initial_C_per_N", filter, initial_C_per_N);
#endif
  log.output ("top_C", filter, top_C);
  log.output ("top_N", filter, top_N);
  log.output ("C", filter, C);
  // These are sometimes const and should be read from the AM library.
  log.output ("C_per_N", filter, C_per_N);
#if 0
  log.output ("turnover_rate", filter, turnover_rate);
  log.output ("efficiency", filter, efficiency);
  log.output ("maintenance", filter, maintenance);
  log.output ("fractions", filter, fractions);
#endif
}

void 
OM::mix (const Geometry& geometry, double from, double to, double penetration)
{ 
  geometry.add (C, from, to, top_C * penetration);
  geometry.mix (C, from, to);
  top_C *= (1.0 - penetration);
}

void 
OM::distribute (const Geometry& geometry, const vector<double>& content)
{
  const double total = accumulate (content.begin (), content.end (), 0.0);

  assert (C.size () == content.size ());

  for (unsigned int i = 0; i < content.size (); i++)
    C[i] += top_C * content[i] / total / geometry.dz (i);
  top_C = 0;
}

void
OM::swap (const Geometry& geometry, double from, double middle, double to)
{
  geometry.swap (C, from, middle, to);
}

double 
OM::total_C (const Geometry& geometry) const
{
  return geometry.total (C);
}

double 
OM::total_N (const Geometry& geometry) const
{
  double total = 0.0;
  const unsigned int size = min (C_per_N.size (), C.size ());
  for (unsigned int i = 0; i < size; i++)
    {
      assert (C_per_N[i] > 0.0);
      total += (C[i] / C_per_N[i]) * geometry.dz (i);
    }
  return total;
}

void
OM::pour (vector<double>& cc, vector<double>& nn)
{
  const unsigned int size = min (C.size (), C_per_N.size ());
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
  top_C += C;
  top_N += N;
}

void 
OM::add (const Geometry& geometry, // Add dead roots.
	 double to_C, /* Fixed C/N */
	 const vector<double>& density)
{
  // Make sure C/N is large enough.
  const int extra_C_per_N = density.size () - C_per_N.size ();
  if (extra_C_per_N > 0)
    {
      assert (initial_C_per_N != Unspecified);
      C_per_N.insert (C_per_N.begin (), extra_C_per_N, initial_C_per_N);
      assert (initial_C_per_N > 0.0);
    }
  
  // Make sure C is large enough.
  const int extra_C = density.size () - C_per_N.size ();
  if (extra_C > 0)
    C.insert (C.begin (), extra_C, 0.0);
  
  // Distribute it according to the root density.
  const double total = geometry.total (density);
  for (unsigned int i = 0; i < density.size (); i++)
    {
      C[i] += to_C * density[i] * geometry.dz (i) / total;
      assert (finite (C[i]));
      assert (C[i] >= 0.0);
    }
}

void 
OM::add (const Geometry& geometry, // Add dead roots.
	 double to_C, double to_N, 
	 const vector<double>& density)
{
  // Make sure C/N is large enough.
  const int extra_C_per_N = density.size () - C_per_N.size ();
  if (extra_C_per_N > 0)
    // It doesn't matter what number we use, ad C will be 0.
    C_per_N.insert (C_per_N.begin (), extra_C_per_N, 1.0);
  
  // Make sure C is large enough.
  const int extra_C = density.size () - C_per_N.size ();
  if (extra_C > 0)
    {
      assert (extra_C_per_N <= extra_C);
      C.insert (C.begin (), extra_C, 0.0);
    }

  // Distribute it according to the root density.
  const double total = geometry.total (density);
  for (unsigned int i = 0; i < density.size (); i++)
    {
      const double factor = density[i] * geometry.dz (i) / total;
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
}

inline void
OM::tock (const double* factor, double fraction, double efficiency,
	  const double* N_soil, double* N_used, double* CO2, OM& om)
{
  const unsigned int size = min (C_per_N.size (), C.size ());

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
OM::tick (const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<OM*>& smb, const vector<OM*>&som)
{
  const int size = C.size ();
  for (int i = 0; i < size; i++)
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
  // Distribute to all biological dk:puljer.
  const int smb_size = smb.size ();
  for (int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (abiotic_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j]);
    }
  // Distribute to all soil dk:puljer.
  const int som_size = som.size ();
  for (int j = 0; j < som_size; j++)
    {
      const double fraction = fractions[smb_size + j];
      if (fraction > 1e-50)
	tock (abiotic_factor, turnover_rate * fraction, 1.0,
	      N_soil, N_used, CO2, *som[j]);
    }
  // assert (N_soil * 1.001 >= N_used);
  for (int i = 0; i < size; i++)
    assert (C[i] >= 0.0);
}

void 
OM::tick (const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<OM*>& smb, double* som_C, double* som_N)
{
  const int size = min (C.size (), C_per_N.size ());

  // Maintenance.
  for (int i = 0; i < size; i++)
    {
      assert (C[i] >= 0.0);

      if (C[i] > 0.0)
	{
	  CO2[i] += C[i] * maintenance;
	  C[i] *= (1.0 - maintenance);
	}
    }
  assert (fractions.size () == smb.size () + 1);
  
  // Distribute to all biological dk:puljer.
  const int smb_size = smb.size ();
  for (int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (abiotic_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j]);
    }

  // Distribute to soil buffer.
  const double factor = turnover_rate * fractions[smb_size];
  for (int i = 0; i < size; i++)
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

OM& 
OM::create (const AttributeList& al, const Geometry& geometry)
{ return *new OM (al, geometry); }

const double OM::Unspecified = -1042.42e42;

void
OM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("top_C", Syntax::Number, Syntax::State);
  alist.add ("top_C", 0.0);
  syntax.add ("top_N", Syntax::Number, Syntax::State);
  alist.add ("top_N", 0.0);
  syntax.add ("C", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("C_per_N", Syntax::Number, Syntax::Optional,
	       Syntax::Sequence);
  syntax.add ("turnover_rate", Syntax::Number, Syntax::Const);
  syntax.add ("efficiency", Syntax::Number, Syntax::Const,
	       Syntax::Sequence);
  syntax.add ("maintenance", Syntax::Number, Syntax::Const);
  alist.add ("maintenance", 0.0);
  syntax.add ("fractions", Syntax::Number, Syntax::Const, 
	       Syntax::Sequence);
  syntax.add ("initial_fraction", Syntax::Number, Syntax::Const);
  alist.add ("initial_fraction", Unspecified);
}
