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

#define BUILD_DLL


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

using namespace std;

void
AOM::output (Log& log) const
{
  OM::output (log);
  output_variable (top_C, log);
  output_variable (top_N, log);
}

void 
AOM::penetrate (const Geometry& geo, double from, double to,
		double penetration,
                double& tillage_N_top, double& tillage_C_top,
                vector<double>& tillage_N_soil, vector<double>& tillage_C_soil)
{
  daisy_assert (penetration >= 0.0);
  daisy_assert (penetration <= 1.0);

  // Ignore tiny pools.
  if (top_C < 1e-20 && top_N < 1e-21)
    return;
  
  const double C_pen = top_C * penetration;
  const double N_pen = top_N * penetration;

  static const double cm2_to_m2 = 100 * 100;

  // Penetrate.
  geo.add_surface (C, from, to, C_pen);
  geo.add_surface (tillage_C_soil, from, to, C_pen);
  daisy_non_negative (C);
  top_C -= C_pen;
  tillage_C_top -= C_pen * cm2_to_m2;
  daisy_assert (top_C >= 0.0);
  geo.add_surface (N, from, to, N_pen);
  geo.add_surface (tillage_N_soil, from, to, N_pen);
  daisy_non_negative (N);
  top_N -= N_pen;
  tillage_N_top -= N_pen * cm2_to_m2;
  daisy_assert (top_N >= 0.0);
}

double 
AOM::full_C (const Geometry& geo) const
{ return soil_C (geo) + top_C * geo.surface_area (); }

double 
AOM::full_N (const Geometry& geo) const
{ return soil_N (geo) + top_N * geo.surface_area (); }

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
  daisy_non_negative (cc);
  daisy_non_negative (nn);
}

void 
AOM::add (unsigned int at, double to_C, double to_N)
{
  daisy_assert (to_C >= 0.0);
  daisy_assert (to_N >= 0.0);
  C[at] += to_C;
  N[at] += to_N;
  daisy_assert (C[at] >= 0.0);
  daisy_assert (N[at] >= 0.0);
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
AOM::add (const Geometry& geo, // Add dead roots.
	 double to_C, double to_N, 
	 const vector<double>& density)
{
  const double old_C = soil_C (geo);
  const double old_N = soil_N (geo);

  // Distribute it according to the root density.
  const double total = geo.total_soil (density);
  for (size_t i = 0; i < density.size (); i++)
    {
      // We should *not* multiply with dz here.  Reason: We want to
      // divide C on the total depth.  
      const double factor = density[i] /* * geo.dz (i) */ / total;
      daisy_assert (factor >= 0.0);
      N[i] += to_N * factor;
      C[i] += to_C * factor;
      daisy_assert (C[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }

  // Check that we computed the correct value.
  const double new_C = soil_C (geo);
  const double new_N = soil_N (geo);
  daisy_assert (to_C * 1e9 < old_C
	  ? approximate (old_C + to_C, new_C)
	  : (approximate (new_C - old_C, to_C)));
  daisy_assert (to_N * 1e9 < old_N
	  ? approximate (old_N + to_N, new_N)
	  : (approximate (new_N - old_N, to_N)));
}

void 
AOM::tick (const std::vector<bool>& active, const double* abiotic_factor, 
           const double* N_soil, double* N_used,
           double* CO2, const vector<SMB*>& smb, double* som_C, double* som_N,
           const vector<DOM*>& dom, const double dt)
{
  const size_t cell_size = active.size ();
  daisy_assert (C.size () == cell_size);
  daisy_assert (N.size () == cell_size);
  
  const unsigned int smb_size = smb.size ();
  const unsigned int dom_size = dom.size ();
  daisy_assert (fractions.size () >= smb_size + 1);

  // Distribute to all biological pools.
  for (size_t j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	turnover_pool (active, abiotic_factor, fraction, efficiency[j],
		       N_soil, N_used, CO2, *smb[j], dt);
    }

  // Distribute to soil buffer.
  const double factor = turnover_rate * fractions[smb_size];
  if (factor > 1e-200)
    for (size_t i = 0; i < cell_size; i++)
      {
        if (!active[i])
          continue;
	const double rate = min (factor * abiotic_factor[i], 0.1);
	const double C_use = C[i] * rate;
	const double N_use = N[i] * rate;
	som_N[i] += N_use * dt;
	som_C[i] += C_use * dt;
	C[i] -= C_use * dt;
	N[i] -= N_use * dt;
	daisy_assert (C[i] >= 0.0);
	daisy_assert (N[i] >= 0.0);
	daisy_assert (som_C[i] >= 0.0);
	daisy_assert (som_N[i] >= 0.0);
      }

  if (fractions.size () == smb_size + 1)
    return;
  daisy_assert (fractions.size () == smb_size + 1 + dom_size);

  // Distribute to all dissolved pools.
  for (size_t j = 0; j < dom_size; j++)
    {
      const double fraction = fractions[smb_size + 1 + j];
      if (fraction > 1e-50)
	turnover_dom (active, abiotic_factor, fraction, *dom[j], dt);
    }
}

void 
AOM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  OM::load_syntax (syntax, alist, "\
The first numbers corresponds to each of the SMB pools, the next\n\
number to the SOM buffer, and any remaining numbers to each of\n\
the DOM pools.  The length of the sequence should thus be the number\n\
of SMB pools plus 1 plus optionally the number of DOM pools."); 
  alist.add ("submodel", "AOM");
  alist.add ("description", "\
A single Added Organic Matter pool.");
  syntax.add_fraction ("initial_fraction", Syntax::OptionalConst, "\
The initial fraction of the total available carbon\n\
allocated to this pool for AOM.  One pool should be left unspecified.");
  syntax.add ("top_C", "g C/cm^2", Check::non_negative (), Syntax::State,
	      "Carbon on top of soil.");
  alist.add ("top_C", 0.0);
  syntax.add ("top_N", "g N/cm^2", Check::non_negative (), Syntax::State,
	      "Nitrogen on top of soil.");
  alist.add ("top_N", 0.0);
}

AOM::AOM (const AttributeList& al)
  : OM (al),
    initial_fraction (al.number ("initial_fraction", Unspecified)),
    top_C (al.number ("top_C")),
    top_N (al.number ("top_N"))
{ }

static Submodel::Register aom_submodel ("AOM", AOM::load_syntax);
