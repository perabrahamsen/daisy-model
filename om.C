// om.C

#include "om.h"
#include "syntax.h"
#include "alist.h"
#include "soil.h"
#include "log.h"
#include "mathlib.h"
#include <algo.h>
#include <ieeefp.h>

OM::OM (const AttributeList& al)
  : top_C (al.number ("top_C")),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{
  assert (!al.check ("C"));

  if (al.check ("C_per_N"))
    C_per_N = al.number_sequence ("C_per_N");
}

OM::OM (const AttributeList& al, const Soil& soil)
  : top_C (al.number ("top_C")),
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
  while (C.size () < soil.size () +0U)
    C.push_back (0.0);

  if (C_per_N.size () > 0U)
    while (C_per_N.size () < C.size ())
      C_per_N.push_back (C_per_N[C_per_N.size () - 1]);
}

OM::OM (const AttributeList& al, const Soil& soil, 
	const double carbon, const double N)
  : top_C (carbon),
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
  while (C.size () < soil.size () +0U)
    C.push_back (0.0);

  // Initialize C/N.
  assert (!al.check ("C_per_N"));
  while (C_per_N.size () < C.size ())
    C_per_N.push_back (CpN);
}

void
OM::output (Log& log, const Filter& filter) const
{
  log.output ("top_C", filter, top_C);
  log.output ("C", filter, C);
  // These are const and should be read from the AM library.
  log.output ("C_per_N", filter, C_per_N);
#if 0
  log.output ("turnover_rate", filter, turnover_rate);
  log.output ("efficiency", filter, efficiency);
  log.output ("maintenance", filter, maintenance);
  log.output ("fractions", filter, fractions);
#endif
}

void 
OM::mix (const Soil& soil, double from, double to, double penetration)
{ 
  soil.add (C, from, to, top_C * penetration);
  soil.mix (C, from, to);
  top_C *= (1.0 - penetration);
}

void 
OM::distribute (const Soil& soil, const vector<double>& content)
{
  const double total = accumulate (content.begin (), content.end (), 0.0);

  assert (C.size () == content.size ());

  for (unsigned int i = 0; i < content.size (); i++)
    C[i] += top_C * content[i] / total / soil.dz (i);
  top_C = 0;
}

void
OM::swap (const Soil& soil, double from, double middle, double to)
{
  soil.swap (C, from, middle, to);
}

bool 
OM::empty () const
{
  int size = C.size ();
  for (int i = 0; i < size; i++)
    if (C[i] > 0.0)
      return false;
  return true;
}

#if 0
void
OM::tick (int i, double abiotic_factor, double N_soil, double& N_used,
	  double& CO2, const vector<OM*>& smb, const vector<OM*>&som)
{
  assert (C[i] >= 0.0);
  // assert (N_soil * 1.001 >= N_used);
  // Maintenance.
  const double C_use = C[i] * maintenance * abiotic_factor;
  CO2 += C_use;
  C[i] -= C_use;
  N_used -= C_use / C_per_N[i];
  // assert (N_soil * 1.001 >= N_used);

  assert (fractions.size () == smb.size () + som.size ());
  // Distribute to all biological dk:puljer.
  const int smb_size = smb.size ();
  for (int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (i, turnover_rate * abiotic_factor * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j]);
    }
#if 1 /* BUG: WHY WHY WHY IS THIS UNCOMMENTED? */
  // Distribute to all soil dk:puljer.
  const int som_size = som.size ();
  for (int j = 0; j < som_size; j++)
    {
      const double fraction = fractions[smb_size + j];
      if (fraction > 1e-50)
	tock (i, turnover_rate * abiotic_factor * fraction, 1.0,
	      N_soil, N_used, CO2, *som[j]);
    }
  // assert (N_soil * 1.001 >= N_used);
  assert (C[i] >= 0.0);
#endif
}

void 
OM::tick (int i, double abiotic_factor, double N_soil, double& N_used,
	  double& CO2, const vector<OM*>& smb, double& som_C, double& som_N)
{
  assert (C[i] >= 0.0);

  if (C[i] > 0.0)
    {
      // Maintenance.
      CO2 += C[i] * maintenance;
      C[i] *= (1.0 - maintenance);
  
      assert (fractions.size () == smb.size () + 1);

      // Distribute to all biological dk:puljer.
      const int smb_size = smb.size ();
      for (int j = 0; j < smb_size; j++)
	{
	  const double fraction = fractions[j];
	  if (fraction > 1e-50)
	    tock (i, turnover_rate * abiotic_factor * fraction, efficiency[j],
		  N_soil, N_used, CO2, *smb[j]);
	}
      // Distribute to soil buffer.
      const double rate = turnover_rate * abiotic_factor * fractions[smb_size];
      som_N += C[i] * rate / C_per_N[i];
      som_C += C[i] * rate;
      C[i] *= (1.0 - rate);
      // assert (N_soil * 1.001 >= N_used);

      if (C[i] < 1e-9)
	{
	  assert (C[i] > -1e9);
	  som_C += C[i];
	  C[i] = 0.0;
	}
    }
}

void
OM::tock (int i, double rate, double efficiency,
	  double N_soil, double& N_used, double& CO2, OM& om)
{
  assert (C[i] >= 0.0);
  assert (finite (rate));
  assert (rate >=0);
  assert (C_per_N.size () > 0U);
  // assert (N_soil * 1.001 >= N_used);
  assert (C_per_N[i] > 0.0);
  assert (om.C_per_N[i] > 0.0);

  double N_produce = C[i] * rate / C_per_N[i];
  double N_consume = C[i] * rate * efficiency / om.C_per_N[i];
  assert (finite (N_produce));
  assert (finite (N_consume));

  if (N_consume - N_produce > N_soil - N_used
      // && N_soil * 1.001 >= N_used
      && (N_consume - N_produce) - (N_soil - N_used) > 1.0e-10 // Lose 1 g / ha / year
      && rate > 0.0)
    {
      // Lower rate to force 
      //   N_consume - N_produce == N_soil - N_used 
      // This is what calc tell me:
      rate = (N_soil - N_used) 
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
	      : ((N_soil == N_used)
		 ? (abs (N_consume - N_produce) < 1e-10)
		 : (abs (1.0 - (N_consume - N_produce) / (N_soil - N_used))
		    < 0.01)));
    }
  // Update.
  const double C_use = C[i] * rate;
  CO2 += C_use * (1.0 - efficiency);
  om.C[i] += C_use * efficiency;
  C[i] -= C_use;
  assert (om.C[i] >= 0.0);
  assert (C[i] >= 0.0);

  N_used += (N_consume - N_produce);

  // Check for NaN.
  assert (finite (N_used));
  assert (finite (rate));
  assert (finite (efficiency));
  // assert (N_soil * 1.001 >= N_used);
  assert (C[i] >= 0.0);
}
#else
inline void
OM::tock (const double* factor, double fraction, double efficiency,
	  const double* N_soil, double* N_used, double* CO2, OM& om)
{
  const int size = C.size ();

  // Maintenance.
  for (int i = 0; i < size; i++)
    {
      double rate = factor[i] * fraction;
#if 0
      assert (C[i] >= 0.0);
      assert (finite (rate));
      assert (rate >=0);
      assert (C_per_N.size () > 0U);
      // assert (N_soil * 1.001 >= N_used);
      assert (C_per_N[i] > 0.0);
      assert (om.C_per_N[i] > 0.0);
#endif
      double N_produce = C[i] * rate / C_per_N[i];
      double N_consume = C[i] * rate * efficiency / om.C_per_N[i];
#if 0
      assert (finite (N_produce));
      assert (finite (N_consume));
#endif
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
#if 0
	  assert (finite (rate));
#endif
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
#if 0
	  assert (finite (N_produce));
	  assert (finite (N_consume));
	  // Check that we calculated the right rate.
	  assert ((rate == 0)
		  ? true 
		  : ((N_soil[i] == N_used[i])
		     ? (abs (N_consume - N_produce) < 1e-10)
		     : (abs (1.0 - (N_consume - N_produce) 
			     / (N_soil[i] - N_used[i]))
			< 0.01)));
#endif
	}
      // Update.
      const double C_use = C[i] * rate;
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency;
      C[i] -= C_use;
#if 0
      assert (om.C[i] >= 0.0);
      assert (C[i] >= 0.0);
#endif
      N_used[i] += (N_consume - N_produce);

      // Check for NaN.
#if 0
      assert (finite (N_used[i]));
      assert (finite (rate));
      assert (finite (efficiency));
      // assert (N_soil * 1.001 >= N_used);
      assert (C[i] >= 0.0);
#endif
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
  const int size = C.size ();

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
      const double rate = factor * abiotic_factor[i];
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
    }
}
#endif

void
OM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("top_C", Syntax::Number, Syntax::State);
  alist.add ("top_C", 0.0);
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
  syntax.add ("initial_fraction", Syntax::Number, Syntax::Optional);
}
