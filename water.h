// water.h --- Water properties.
// 
// 2009 Copyright Per Abrahamsen and KU
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

#ifndef WATER_H
#define WATER_H

namespace Water
{
  const double density = 998;   // [kg/m^3] 
  const double surface_tension = 7.28e-2; // [N/m]
  double viscosity (double T /* [dg C] */); // [Pa s] = [N s m^-2]
}

#endif // WATER_H
