// texture.C --- Mineral particle size of soil.
// 
// Copyright 2003 Per Abrahamsen and KVL.
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

#include "texture.h"
#include "assertion.h"
#include "mathlib.h"
#include <numeric>

using namespace std;

// Weigth of mineral particles. [g/cm^3]
static const double rho_mineral = 2.65;	
static const double rho_humus = 1.3; 
static const double rho_CaCO3 = 1.612;

double 
Texture::fraction_of_minerals_smaller_than (double size /* [um] */) const
{ return accumulated (log (size)); }

double 
Texture::mineral () const      // Fraction of minerals in dry soil [].
{ return 1.0 - humus - CaCO3; }

double
Texture::rho_soil_particles () const
{ return mineral () * rho_mineral + humus * rho_humus + CaCO3 * rho_CaCO3; }

const PLF
Texture::build_accumulated (const vector<double>& lim,
                            const vector<double>& frac)
{
  PLF result;
  daisy_assert (lim.size () == frac.size ());
  double sum = 0.0;
  for (unsigned int i = 0; i < lim.size (); i++)
    {
      sum += frac[i];
      result.add (log(lim[i]), sum);
    }
  daisy_assert (approximate (sum, 1.0));
  return result;
}

Texture::Texture (const vector<double>& lim, const vector<double>& frac,
                  double hum, double chalk)
  : limit (lim),
    fraction (frac),
    accumulated (build_accumulated (lim, fraction)),
    humus (hum),
    CaCO3 (chalk)
{
  daisy_assert (limit.size () == fraction.size ());
  daisy_assert (approximate (accumulate (fraction.begin (), 
                                         fraction.end (), 0.0), 
                             1.0));
}

Texture::~Texture ()
{ }

