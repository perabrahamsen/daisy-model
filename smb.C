// smb.C --- A single soil microbiological pool.
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


#include "smb.h"
#include "dom.h"
#include "submodel.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"
#include "check.h"
#include "mathlib.h"

using namespace std;

void
SMB::maintain (const std::vector<bool>& active, const double* abiotic_factor, 
	       double* N_used, double* CO2)
{
  const size_t node_size = active.size ();
  daisy_assert (C.size () == node_size);
  daisy_assert (N.size () == node_size);

  for (size_t i = 0; i < node_size; i++)
    {
      if (!active[i])
        continue;

      // Maintenance.
      const double C_use = C[i] * clay_maintenance[i] * abiotic_factor[i];
      const double N_use = N[i] * clay_maintenance[i] * abiotic_factor[i];
      CO2[i] += C_use;
      C[i] -= C_use;
      N[i] -= N_use;
      N_used[i] -= N_use;
      daisy_assert (C[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }
}

void
SMB::turnover_pool (const std::vector<bool>& active, const double* factor,
		    double fraction, double efficiency,
		    const double* N_soil, double* N_used, double* CO2, OM& om)
{
  const size_t node_size = active.size ();
  daisy_assert (C.size () == node_size);
  daisy_assert (N.size () == node_size);

  // Maintenance.
  for (size_t i = 0; i < node_size; i++)
    {
      if (!active[i])
        continue;
      const double rate 
        = min (factor[i] * clay_turnover[i] * fraction, 0.1);
      daisy_assert (C[i] >= 0.0);
      daisy_assert (isfinite (rate));
      daisy_assert (rate >=0);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (N[i] >= 0.0);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (om.C[i] >= 0.0);
      double C_use;
      double N_produce;
      double N_consume;
      
      turnover (C[i], N[i], om.goal_C_per_N (i), N_soil[i] - N_used[i],
		rate, efficiency, C_use, N_produce, N_consume);

      // Update C.
      daisy_assert (om.C[i] >= 0.0);
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency;
      C[i] -= C_use;
      daisy_assert (om.C[i] >= 0.0);
      daisy_assert (C[i] >= 0.0);

      // Update N.
      N_used[i] += (N_consume - N_produce);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
      om.N[i] += N_consume;
      N[i] -= N_produce;
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }
}

void
SMB::turnover_dom (const std::vector<bool>& active, const double* factor,
		  double fraction, DOM& dom)
{
  const size_t node_size = active.size ();

  for (unsigned int i = 0; i < node_size; i++)
    {
      const double rate = min (clay_turnover[i] * fraction * factor[i], 0.1);
      const double C_use = C[i] * rate;
      const double N_use = N[i] * rate;
      dom.add_to_source (i, C_use, N_use);
      C[i] -= C_use;
      N[i] -= N_use;
      daisy_assert (C[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }
}

void 
SMB::load_syntax (Syntax& syntax, AttributeList& alist)
{
  OM::load_syntax (syntax, alist, "\
The first numbers corresponds to each of the SMB pools, the next\n\
numbers corresponds to the SOM pools, and the last numbers to each of\n\
the DOM pools.  The length of the sequence should thus be the number\n\
of SMB pools plus the number of SOM pools plus the number of DOM pools."); 
  alist.add ("submodel", "SMB");
  alist.add ("description", "\
A single Soil MicroBiological pool.");
  syntax.add ("maintenance", "h^-1", Check::fraction (), Syntax::Const, "\
The fraction used for staying alive each hour.");
}

SMB::SMB (const AttributeList& al)
  : OM (al),
    maintenance (al.number ("maintenance"))
{ }

static Submodel::Register smb_submodel ("SMB", SMB::load_syntax);
