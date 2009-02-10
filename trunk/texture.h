// texture.h --- Mineral particle size of soil.
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


#ifndef TEXTURE_H
#define TEXTURE_H

#include <vector>
#include "plf.h"

class Texture 
{
  // Content.
  const std::vector<double> limit; // Limits between texture classes [um].
  const std::vector<double> fraction; // Mineral in each tc [], total 1.0.
  const PLF accumulated; // Accumulated mineral content [log(um)]->[].
public:
  double fraction_of_minerals_smaller_than (double size /* [um] */) const;
  double mineral () const;     // Fraction of minerals in dry soil [].
  const double humus;           // Fraction of humus in dry soil [].
  const double CaCO3;           // Fraction of chalk in dry soil.
  // Note: mineral + humus + CaCO3 = 1.0
  double rho_soil_particles () const;
  
  // Create and destroy.
private:
  static const PLF build_accumulated (const std::vector<double>& lim,
                                      const std::vector<double>& frac);
public:
  Texture (const std::vector<double>& lim, const std::vector<double>& frac,
           double hum, double chalk);
  ~Texture ();
};

#endif // TEXTURE_H
