// aom.C --- A single added organic matter pool.
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


#include "aom.h"
#include "submodel.h"
#include "alist.h"
#include "syntax.h"
#include "check.h"
#include "assertion.h"
#include "smb.h"
#include "dom.h"
#include "log.h"
#include "geometry.h"
#include "mathlib.h"

void
AOM::output (Log& log) const
{
  OM::output (log);
  log.output ("top_C", top_C);
  log.output ("top_N", top_N);
}

void 
AOM::penetrate (const Geometry& geometry, double from, double to,
		double penetration)
{
  daisy_assert (penetration >= 0.0);
  daisy_assert (penetration <= 1.0);

  // Ignore tiny pools.
  if (top_C < 1e-20 && top_N < 1e-21)
    return;

  // Penetrate.
  geometry.add (C, from, to, top_C * penetration);
  top_C *= (1.0 - penetration);
  geometry.add (N, from, to, top_N * penetration);
  top_N *= (1.0 - penetration);
}

double 
AOM::full_C (const Geometry& geometry) const
{ return soil_C (geometry) + top_C; }

double 
AOM::full_N (const Geometry& geometry) const
{ return soil_N (geometry) + top_N; }

double 
AOM::C_at (unsigned int at) const
{
  if (at >= C.size ())
    return 0.0;
  return C[at];
}

double 
AOM::N_at (unsigned int at) const
{
  if (at >= N.size ())
    return 0.0;
  return N[at];
}

void
AOM::pour (vector<double>& cc, vector<double>& nn)
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
AOM::add (unsigned int at, double to_C, double to_N)
{
  grow (at+1);
  C[at] += to_C;
  N[at] += to_N;
  daisy_assert (finite (C[at]));
  daisy_assert (finite (N[at]));
}

void 				// Add dead leafs.
AOM::add (double C, double N)
{
  daisy_assert (C >= 0.0);
  daisy_assert (N >= 0.0);
  top_C += C;
  top_N += N;
}

void 
AOM::add (const Geometry& geometry, // Add dead roots.
	 double to_C, double to_N, 
	 const vector<double>& density)
{
  const double old_C = soil_C (geometry);
  const double old_N = soil_N (geometry);
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
  const double new_C = soil_C (geometry);
  const double new_N = soil_N (geometry);
  daisy_assert (to_C * 1e9 < old_C
	  ? approximate (old_C + to_C, new_C)
	  : (approximate (new_C - old_C, to_C)));
  daisy_assert (to_N * 1e9 < old_N
	  ? approximate (old_N + to_N, new_N)
	  : (approximate (new_N - old_N, to_N)));
}

void 
AOM::tick (unsigned int end, const double* abiotic_factor, 
	  const double* N_soil, double* N_used,
	  double* CO2, const vector<SMB*>& smb, double* som_C, double* som_N,
	  const vector<DOM*>& dom)
{
  const unsigned int size = min (C.size (), end);
  daisy_assert (N.size () >= size);
  
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
#ifdef FIXED_DOM_C_per_N
  for (unsigned int j = 0; j < dom_size; j++)
    {
      const double fraction = fractions[smb_size + 1 + j];
      if (fraction > 1e-50)
	tock (size, abiotic_factor, turnover_rate * fraction, 1.0,
	      N_soil, N_used, CO2, *dom[j]);
    }
#else // Variable DOM C/N
  for (unsigned int j = 0; j < dom_size; j++)
    {
      const double factor = turnover_rate * fractions[smb_size + 1 + j];
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
}

void 
AOM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  OM::load_syntax (syntax, alist); 
  alist.add ("submodel", "AOM");
  alist.add ("description", "\
A single Added Organic Matter pool.");
  syntax.add ("top_C", "g C/cm^2", Check::non_negative (), Syntax::State,
	      "Carbon on top of soil.");
  alist.add ("top_C", 0.0);
  syntax.add ("top_N", "g N/cm^2", Check::non_negative (), Syntax::State,
	      "Nitrogen on top of soil.");
  alist.add ("top_N", 0.0);
}

AOM::AOM (const AttributeList& al)
  : OM (al),
    top_C (al.number ("top_C")),
    top_N (al.number ("top_N"))
{ }

static Submodel::Register aom_submodel ("AOM", AOM::load_syntax);
