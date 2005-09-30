// bioclimate.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "bioclimate.h"
#include "weather.h"
#include "mathlib.h"

using namespace std;

template<>
Librarian<Bioclimate>::Content* Librarian<Bioclimate>::content = NULL;

const char *const Bioclimate::description = "\
The 'bioclimate' component is responsible for distributing the water\n\
and energy provided by the weather component among the crops and soil\n\
for a given column.";

double 
Bioclimate::day_fraction () const
{
  if (daily_global_radiation () > 0.0)
    return hourly_global_radiation () / (24.0 * daily_global_radiation ());
  return 0.0;
}

void
Bioclimate::radiation_distribution (const int No, const double LAI,
                                    const double Ref,
                                    const double Si,
                                    const double Ext,
                                    vector <double>& Rad)
{
  // Fraction of Photosynthetically Active Radiation in Shortware
  // incoming radiation. 
  static const double PARinSi = 0.50;	

  const double PAR0 = (1 - Ref) * PARinSi * Si;
  intensity_distribution (No, LAI, PAR0, Ext, Rad);
}

void
Bioclimate::intensity_distribution (const int No, const double LAI,
                                    const double Rad0,
                                    const double Ext,
                                    vector <double>& Rad)
{
  daisy_assert (Rad.size () == No + 1);
  const double dLAI = (LAI / No);
    
  for (int i = 0; i <= No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

Bioclimate::Bioclimate (const AttributeList& al)
  : name (al.identifier ("type")),
    alist (al)
{ }

Bioclimate::~Bioclimate ()
{ }
