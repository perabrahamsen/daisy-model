// select_flux.h --- Shared interface for fluxes. 
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002, 2006 Per Abrahamsen and KVL.
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

#ifndef SELECT_FLUX_H
#define SELECT_FLUX_H

#include "select_value.h"

struct SelectFlux : public SelectValue
{
  // Content.
  double height;
  const Geometry* last_geo;
  const Soil* last_soil;
  std::vector<int> edges;
  std::vector<double> weight;

  // Output routines.
  void output_array (const std::vector<double>&, 
                     const Geometry*, const Soil*, Treelog&);
  SelectFlux (Block&, double height);
};

#endif // SELECT_FLUX_H
