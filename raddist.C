// raddist.C  -- Radiation distribution
// 
// Copyright 2006 Birgitte Gjettermann, Per Abrahamsen and KVL.
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


#include "raddist.h"
#include "block.h"
#include "mathlib.h"

template<>
Librarian<Raddist>::Content* Librarian<Raddist>::content = NULL;

const char *const Raddist::description = "\
The 'raddist' component calculates the radiation distribution in the canopy.";

void
Raddist::output (Log&) const
{ }

void
Raddist::radiation_distribution (const int No, const double LAI,
				 const double Ref,
				 const double Si,
				 const double Ext,
				 std::vector <double>& Rad)
{
  daisy_assert (std::isfinite (Si));
  if (Si <= 0.0)
    {
      for (int i = 0; i <= No; i++)
	Rad[i] = 0.0;
      return;
    }
  daisy_assert (Ref < 1.0);
  daisy_assert (Ext >= 0.0);
  // Fraction of Photosynthetically Active Radiation in Shortware
  // incoming radiation. 
  static const double PARinSi = 0.50;	

  const double PAR0 = (1 - Ref) * PARinSi * Si;
  intensity_distribution (No, LAI, PAR0, Ext, Rad);
}

void
Raddist::intensity_distribution (const int No, const double LAI,
				 const double Rad0,
				 const double Ext,
				 std::vector <double>& Rad)
{
  daisy_assert (Rad.size () == No + 1);
  const double dLAI = (LAI / No);
    
  for (int i = 0; i <= No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

void 
Raddist::load_syntax (Syntax&, AttributeList&)
{ }

Raddist::Raddist (Block& al)
  : name (al.identifier ("type")),
    alist (al.alist ())
{ }

Raddist::~Raddist ()
{ }

