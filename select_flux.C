// select_flux.C --- Select a state variable.
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002, 2006 KVL and Per Abrahamsen
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


#include "select_flux.h"
#include "geometry.h"
#include "treelog.h"
#include <cmath>
#include <sstream>

void
SelectFlux::output_array (const std::vector<double>& array, 
                          const Geometry* geo, const Soil* soil, Treelog&)
{ 
  if (soil != last_soil)
    last_soil = soil;

  if (geo != last_geo)
    {
      last_geo = geo;
      const size_t size = geo->edge_size ();
      edges.clear ();
      weight.clear ();
      double total_area = 0.0;
      for (size_t e = 0; e < size; e++)
        if (geo->edge_cross_z (e, height > 0 ? geo->bottom () : height))
          {
            edges.push_back (e);
            const double area = geo->edge_area (e);
            weight.push_back (area);
            total_area += area;
          }
      daisy_assert (std::isnormal (total_area) || weight.size () == 0);
      for (size_t i = 0; i < weight.size (); i++)
        weight[i] /= total_area;
      daisy_assert (edges.size () == weight.size ());
    }
  daisy_assert (edges.size () <= array.size ());

  double sum = 0.0;
  for (size_t i = 0; i < edges.size (); i++)
    sum += array[edges[i]] * weight[i];

  add_result (sum);
}

SelectFlux::SelectFlux (Block& al, const double h)
  : SelectValue (al),
    height (h),
    last_geo (NULL),
    last_soil (NULL)
{ }
