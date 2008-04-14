// msoltranrect.C -- Solute transport in primary domain.
// 
// Copyright 2006, 2008 Per Abrahamsen and KVL.
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

#include "msoltranrect.h"
#include "chemical.h"
#include "doe.h"
#include "geometry.h"
#include "adsorption.h"
#include "block.h"
#include "librarian.h"
#include "soil_water.h"
#include "soil.h"

const char *const Msoltranrect::component = "msoltranrect";

symbol 
Msoltranrect::library_id () const
{
  static const symbol id (component);
  return id;
}

void 
Msoltranrect::element (const Geometry& geo, 
                       const Soil& soil, const SoilWater& soil_water,
                       DOE& element, const double diffusion_coefficient, 
                       const double dt, Treelog& msg)
{
  // Edges.
  const size_t edge_size = geo.edge_size ();
  std::vector<double> q (edge_size); // Water flux [cm].
  for (size_t e = 0; e < edge_size; e++)
    q[e] = soil_water.q_primary (e);

  // Cells.
  const size_t cell_size = geo.cell_size ();
  std::vector<double> Theta_old (cell_size); // Water content at start...
  std::vector<double> Theta_new (cell_size); // ...and end of timestep.
  for (size_t c = 0; c < cell_size; c++)
    {
      Theta_old[c] = soil_water.Theta_primary_old (c);
      Theta_new[c] = soil_water.Theta_primary (c);
    }

  // Upper border.
  std::map<size_t, double> J_forced;
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      J_forced[edge] = 0.0;
    }

  // Lower border.
  std::map<size_t, double> C_border;
  const std::vector<size_t>& edge_below = geo.cell_edges (Geometry::cell_below);
  const size_t edge_below_size = edge_below.size ();
  for (size_t i = 0; i < edge_below_size; i++)
    {
      const size_t edge = edge_below[i];
      C_border[edge] = 0.0;
    }

  element.tick (cell_size, soil_water, dt);
  static const symbol DOM_name ("DOM");
  flow (geo, soil, Theta_old, Theta_new, q, DOM_name, 
        element.S, J_forced, C_border, element.C, element.J_matrix, 
        diffusion_coefficient, dt, msg);
  for (size_t c = 0; c < cell_size; c++)
    element.M[c] = element.C[c] * soil_water.Theta (c);
}

bool 
Msoltranrect::check (const Geometry&, Treelog&)
{ return true; }

Msoltranrect::Msoltranrect (Block& al)
  : Model ()
{ }

Msoltranrect::~Msoltranrect ()
{ }

static Librarian Msoltranrect_init (Msoltranrect::component, "\
Solute transport in primary domain.");

// msoltranrect.C ends here
