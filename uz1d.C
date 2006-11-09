// uz1d.C --- One dimensional transport.
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

#include "uz1d.h"

void 
SMM1D::reset (std::vector<double>& h, std::vector<double>& Theta) const
{
  const size_t cell_size = cells.size ();
  daisy_assert (h.size () == cell_size);
  daisy_assert (Theta.size () == cell_size);
  for (size_t i = 0; i < cell_size; i++)
    {
      h[i] = h_old[i];
      Theta[i] = Theta_old_[i];
    }
}

void 
SMM1D::update (const std::vector<double>& h,
               const std::vector<double>& Theta,
               const std::vector<double>& q)
{
  const size_t cell_size = cells.size ();
  daisy_assert (h.size () == cell_size);
  daisy_assert (Theta.size () == cell_size);
  for (size_t i = 0; i < cell_size; i++)
    {
      const size_t cell = cells[i];
      soil_water.set_content (cell, h[i], Theta[i]);
    }
  const size_t edge_size = edges.size ();
  daisy_assert (q.size () == edge_size);
  for (size_t i = 0; i < edge_size; i++)
    {
      const int edge = edges[i];
      if (edge >= 0)
        {
          daisy_assert (edge < geo.edge_size ());
          daisy_assert (approximate (geo.z_safe (geo.edge_from (edge)), 
                                     geo.z_safe (geo.edge_to (edge))));
          soil_water.set_flux (edge, q[i]);
        }
    }
}


static std::vector<double>
calculate_edge_distance (const GeometryRect& geo, 
                         const std::vector<size_t>& cells)
{ 
  std::vector<double> result;
  const size_t size = cells.size ();
  const size_t cell_size = geo.cell_size ();
  double prev_x = 0.0;

  for (size_t i = 0; i < size; i++)
    {
      const size_t cell = cells[i];
      daisy_assert (cell < cell_size);
      const double next_x = geo.x (cell);
      const double dist = next_x - prev_x;
      daisy_assert (dist > 0.0);
      result.push_back (dist);
      prev_x = next_x;
    }
  const double last = geo.surface_area () / 1.0 /* [cm] */;
  daisy_assert (last > prev_x);
  result.push_back (last - prev_x);

  daisy_assert (result.size () == size + 1);
  return result;
}

static std::vector<double>
fetch_Theta (const SoilWater& soil_water, const std::vector<size_t>& cells)
{ 
  std::vector<double> result;
  const size_t size = cells.size ();

  for (size_t i = 0; i < size; i++)
    result.push_back (soil_water.Theta (cells[i]));

  daisy_assert (result.size () == size);
  return result;
}

static std::vector<double>
fetch_h (const SoilWater& soil_water, const std::vector<size_t>& cells)
{ 
  std::vector<double> result;
  const size_t size = cells.size ();

  for (size_t i = 0; i < size; i++)
    result.push_back (soil_water.h (cells[i]));

  daisy_assert (result.size () == size);
  return result;
}

SMM1D::SMM1D (const GeometryRect& geo_, const Soil& soil_,
              SoilWater& soil_water_, const SoilHeat& soil_heat_,
              const std::vector<size_t>& cells_, 
              const std::vector<int>& edges_)
  : geo (geo_),
    soil (soil_),
    soil_water (soil_water_),
    Theta_old_ (fetch_Theta (soil_water_, cells_)),
    h_old (fetch_h (soil_water_, cells_)),
    soil_heat (soil_heat_),
    cells (cells_),
    edges (edges_),
    edge_distance_ (calculate_edge_distance (geo_, cells_))
{ 
  daisy_assert (cells.size () + 1 == edges.size ());
  for (size_t i = 0; i < cells.size (); i++)
    {
      const size_t cell = cells[i];
      daisy_assert (cell < geo.cell_size ());
      const int prev = edges[i];
      if (prev < 0)
        daisy_assert (i == 0);
      else
        {
          daisy_assert (prev < geo.edge_size ());
          daisy_assert (geo.edge_to (prev) == cell);
        }
      const int next = edges[i+1];
      if (next < 0)
        daisy_assert (i == cells.size () - 1);
      else
        {
          daisy_assert (geo.edge_from (next) == cell);
          daisy_assert (next < geo.edge_size ());
        }
    }
}

template<>
Librarian<UZ1D>::Content* Librarian<UZ1D>::content = NULL;

const char *const UZ1D::description = "\
The 'uz1d' component handles the horizontal water movement in the\n\
unsaturated zone soil matrix.";

UZ1D::UZ1D (Block& al)
  : name (al.identifier ("type"))
{ }

UZ1D::~UZ1D ()
{ }

// uz1d.C ends here.
