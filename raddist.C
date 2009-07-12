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

#define BUILD_DLL

#include "raddist.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"

const char *const Raddist::component = "raddist";

symbol 
Raddist::library_id () const
{
  static const symbol id (component);
  return id;
}

const double
Raddist::PARinSi = 0.50; //(Ross, 1975)	

const double
Raddist::NIRinSi = 0.47; //(Ross, 1975)	

void
Raddist::radiation_distribution (const int No, const double LAI,
                                 const double Ref,
				 const double Si,
				 const double Ext,
				 std::vector <double>& Rad, const double RadinSi)
{
  daisy_assert (std::isfinite (Si));
  if (Si <= 0.0)  //Global radiation <= 0
    {
      for (int i = 0; i <= No; i++)
	Rad[i] = 0.0;
      return;
    }
  daisy_assert (Ext >= 0.0);
  daisy_assert (Ref >= 0.0);

  const double Rad0 = (1 - Ref) * RadinSi * Si;

  intensity_distribution (No, LAI, Rad0, Ext, Rad);
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

Raddist::Raddist (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Raddist::~Raddist ()
{ }

static struct RaddistInit : public DeclareComponent 
{
  RaddistInit ()
    : DeclareComponent (Raddist::component, "\
The 'raddist' component calculates the radiation distribution in the canopy.")
  { }
} Raddist_init;

// raddist.C ends here.
