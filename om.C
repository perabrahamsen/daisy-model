// om.C

#include "om.h"
#include "syntax.h"
#include "alist.h"
#include "soil.h"
#include "log.h"
#include "mathlib.h"
#include <algo.h>

OM::OM (const AttributeList& al)
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
}

OM::OM (const AttributeList& al, const double carbon, const double N)
  : top_C (carbon),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number_sequence ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{
  if (al.check ("C"))
    C = al.number_sequence ("C");
  if (al.check ("C_per_N"))
    {
      C_per_N = al.number_sequence ("C_per_N");
      assert (0);
    }
  else
    C_per_N.push_back (carbon / N);
}

void 
OM::initialize (const Soil& soil)
{
  // Create initial C.
  while (C.size () < soil.size () +0U)
    C.push_back (0.0);

  assert (C_per_N.size () > 0U);
  
  while (C_per_N.size () < C.size ())
    C_per_N.push_back (C_per_N[C_per_N.size () - 1]);

  assert (C_per_N[C_per_N.size () - 1] > 0);
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
OM::distribute (const vector<double>& density)
{
  const double total = accumulate (density.begin (), density.end (), 0.0);

  assert (C.size () == density.size ());

  for (unsigned int i = 0; i < density.size (); i++)
    C[i] += top_C * density[i] / total;
  top_C = 0;
}

void
OM::swap (const Soil& soil, double from, double middle, double to)
{
  soil.swap (C, from, middle, to);
}

void
OM::tick (int i, double turnover_factor, double N_soil, double& N_used,
	  double& CO2, const vector<OM*>& smb, const vector<OM*>&som)
{
  // Maintenance.
  CO2 += C[i] * maintenance;
  C[i] *= (1.0 - maintenance);

  assert (fractions.size () == smb.size () + som.size ());
  // Distribute to all biological dk:puljer.
  const int smb_size = smb.size ();
  for (int j = 0; j < smb_size; j++)
    tock (i, turnover_rate * turnover_factor * fractions[j], efficiency[j],
	  N_soil, N_used, CO2, *smb[j]);
  // Distribute to all soil dk:puljer.
  const int som_size = som.size ();
  for (int j = 0; j < som_size; j++)
    tock (i, turnover_rate * turnover_factor * fractions[smb_size + j], 1.0,
	  N_soil, N_used, CO2, *som[j]);
}

void 
OM::tick (int i, double turnover_factor, double N_soil, double& N_used,
	  double& CO2, const vector<OM*>& smb, double& som_C, double& som_N)
{
  // Maintenance.
  CO2 += C[i] * maintenance;
  C[i] *= (1.0 - maintenance);

  assert (fractions.size () == smb.size () + 1);

  // Distribute to all biological dk:puljer.
  const int smb_size = smb.size ();
  for (int j = 0; j < smb_size; j++)
    tock (i, turnover_rate * turnover_factor * fractions[j], efficiency[j],
	  N_soil, N_used, CO2, *smb[j]);
  
  // Distribute to soil buffer.
  const double rate = turnover_rate * turnover_factor * fractions[smb_size];
  som_N += C[i] * rate / C_per_N[i];
  som_C += C[i] * rate;
  C[i] *= (1.0 - rate);
}

void
OM::tock (int i, double rate, double efficiency,
	  double N_soil, double& N_used, double& CO2, OM& om)
{
  double N_produce = C[i] * rate / C_per_N[i];
  double N_consume = C[i] * rate * efficiency / om.C_per_N[i];
      
  if (N_consume - N_produce > N_soil - N_used)
    {
      // Lower rate to force 
      //   N_consume - N_produce == N_soil - N_used 
      // This is what calc tell me:
      rate = (N_soil - N_used) 
	/ (efficiency * C[i] / om.C_per_N[i] - C[i] / C_per_N[i]);
      // 
      // Aside: We could also have solved the equation by decresing the 
      // efficiency.
      //   efficiency = ((N_soil - N_used) + rate * C[i] / C_per_N[i])
      //     * om.C_per_N / rate * C[i];
      // But we don't

      // Update the N values.
      N_produce = C[i] * rate / C_per_N[i];
      N_consume = C[i] * rate * efficiency / om.C_per_N[i];
      // Check that we calculated the right rate.
      assert ((N_soil == N_used)
	      ? (abs (N_consume - N_produce) < 1e-10)
	      : (abs (1.0 - (N_consume - N_produce) / (N_soil - N_used))
		 < 0.01));
    }
  // Update.
  CO2 += C[i] * rate * (1.0 - efficiency);
  N_used += (N_consume - N_produce);
  om.C[i] += C[i] * rate * efficiency;
  C[i] *= (1.0 - rate * efficiency);

  // Check for NaN.
  assert (N_used >= 0.0 || N_used <= 0.0);
  assert (rate >= 0.0 || rate <= 1.0);
  assert (efficiency >= 0.0 || efficiency <= 1.0);
}

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
