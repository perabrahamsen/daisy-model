// soil_water_rect.C -- Soil water movement in a a rectangular grid.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "soil_water_rect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "surface.h"
#include "timestep.h"
#include "submodel.h"

void
SoilWaterRect::tick (GeometryRect& geo, const Soil& soil, 
                     Surface& surface, Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  // Shared work.
  tick_base (cell_size, soil, msg);

  // Update water.
  for (size_t i = 0; i < cell_size; i++)
    {
      Theta_[i] -= S_sum_[i] * dt;
      h_[i] = soil.h (i, Theta_[i]);
    }

  const double q_up = surface.q ();
  const size_t edge_size = geo.edge_size ();
  const double surface_area = geo.surface_area ();

  for (size_t e = 0; e < edge_size; e++)
    if (geo.edge_to (e) == Geometry::cell_above)
      {
        if (q_up > 0)
          // We obey exfiltration.
          {
            const size_t n = geo.edge_from (e);
            q_[e] = q_up / surface_area;
            Theta_[n] -= q_[e] * geo.edge_area (e) * dt / geo.volume (n);
            surface.accept_top (q_up, geo, e, msg);
          }
        else
          q_[e] = 0.0;
      }
}

void 
SoilWaterRect::output (Log& log) const
{ output_base (log); }

void
SoilWaterRect::initialize (const AttributeList& al, const GeometryRect& geo,
                           const Soil& soil, const Groundwater& groundwater, 
                           Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWaterRect");
  initialize_base (al, geo, soil, groundwater, msg);
}

void
SoilWaterRect::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  load_base (syntax, alist);
  alist.add ("submodel", "SoilWaterrect");
  alist.add ("description", "Water content of soil in a rectangular grid.");
}

SoilWaterRect::SoilWaterRect (Block& al)
  : SoilWater (al)
{ }
  
SoilWaterRect::~SoilWaterRect ()
{ }

static Submodel::Register 
soil_water_rect_submodel ("SoilWaterRect", SoilWaterRect::load_syntax);
