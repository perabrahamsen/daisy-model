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
#include "submodel.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"
#include "check.h"

void
SMB::maintain (unsigned int end, const double* abiotic_factor, 
	       double* N_used, double* CO2)
{
  const unsigned int size = min (C.size (), end);
  daisy_assert (N.size () >= size);

  for (unsigned int i = 0; i < size; i++)
    {
      // Maintenance.
      const double C_use = C[i] * maintenance * abiotic_factor[i];
      const double N_use = N[i] * maintenance * abiotic_factor[i];
      CO2[i] += C_use;
      C[i] -= C_use;
      N[i] -= N_use;
      N_used[i] -= N_use;
      daisy_assert (C[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }
}

void 
SMB::load_syntax (Syntax& syntax, AttributeList& alist)
{
  OM::load_syntax (syntax, alist); 
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
