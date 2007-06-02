// msoltranrect.C -- Matrix solute transport in rectangular grid.
// 
// Copyright 2006 Per Abrahamsen andKVL.
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


#include "msoltranrect.h"
#include "solute.h"
#include "element.h"
#include "geometry_rect.h"
#include "block.h"
#include "librarian.h"

const char *const Msoltranrect::component = "msoltranrect";

void
Msoltranrect::solute (const GeometryRect& geo,
                      const Soil& soil, const SoilWater& soil_water,
                      const double J_in, Solute& solute, const double dt,
                      Treelog& msg)
{ 
  Treelog::Open nest (msg, "Msoltranrect: " + name);
  const size_t edge_size = geo.edge_size ();
  const size_t cell_size = geo.cell_size ();

  solute.tick (cell_size, soil_water, dt);

  std::vector<double> M (cell_size);
  std::vector<double> C (cell_size);
  std::vector<double> S (cell_size);
  std::vector<double> J (edge_size);

  // Initialize edges.
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_to (e) == Geometry::cell_above)
        J[e] = J_in;
      else
        J[e] = 0.0;
    }

  // Initialize cells.
  for (size_t c = 0; c < cell_size; c++)
    {
      M[c] = solute.M (c);
      C[c] = solute.C (c);
      S[c] = solute.S (c);
    }

  // Flow.
  flow (geo, soil, soil_water, solute.submodel, 
        M, C, S, J, 
        *solute.adsorption, solute.diffusion_coefficient (), 
        dt, msg);

  // Update edges.
  for (size_t e = 0; e < edge_size; e++)
    solute.set_matrix_flux (e, J[e]);

  // Update cells.
  for (size_t c = 0; c < cell_size; c++)
    solute.set_content (c, M[c], C[c]);
}

void 
Msoltranrect::element (const GeometryRect& geo, 
                       const Soil& soil, const SoilWater& soil_water,
                       Element& element, Adsorption& adsorption,
                       const double diffusion_coefficient, 
                       const double dt, Treelog& msg)
{
  element.tick (geo.cell_size (), soil_water, dt);
  flow (geo, soil, soil_water, "DOM", 
        element.M, element.C, element.S, element.J, 
        adsorption, diffusion_coefficient, dt, msg);
}

Msoltranrect::Msoltranrect (Block& al)
  : name (al.identifier ("type"))
{ }

Msoltranrect::~Msoltranrect ()
{ }

static Librarian Msoltranrect_init (Msoltranrect::component, "\
Matrix solute transport in rectangular grid.");

// msoltranrect.C ends here
