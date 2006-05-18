// geometry_rect.C -- Horizontal and vertical grid lines.
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

#include "geometry_rect.h"
#include "check.h"
#include "vcheck.h"
#include "block.h"
#include "submodel.h"
#include <sstream>

size_t 
GeometryRect::cell_at (const double z, const double x, const double) const
{ 
  size_t cell = 0;
  while (zplus_[cell] > z)
    {
      cell++;
      daisy_assert (cell < cell_size ());
    }
  while (xplus_[cell] < x)
    { 
      cell += cell_columns_;
      daisy_assert (cell < cell_size ());
    }      
  return cell;
}

double 
GeometryRect::fraction_in_z_interval (const size_t i, 
                                      const double from, const double to) const
{ return fraction_within (zplus (i), zminus (i), to, from); }

bool 
GeometryRect::contain_z (const size_t i, const double z) const
{ 
  daisy_assert (zminus (i) > zplus (i));
  return  zminus (i) > z && z >= zplus (i); 
}

bool 
GeometryRect::check (Treelog&) const
{
  bool ok = true;
  return ok;
}

bool 
GeometryRect::check_border (const double border, Treelog& err) const
{
  bool ok = false;

  for (size_t i = 0; i < cell_rows_; i++)
    if (approximate (border, zplus (i)))
      ok = true;

  if (!ok)
    {
      std::ostringstream tmp;
      tmp << "No geometric border near " << border 
             << " [cm], log results may be inexact";
      err.warning (tmp.str ());
    }

  return ok;
}

void
GeometryRect::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("zplus", "cm", Check::negative (), 
	      Syntax::Const, Syntax::Sequence,
	      "Depth of each numeric layer (a negative number).\n\
The end points are listed descending from the surface to the bottom.");
  static VCheck::All zplus_check (VCheck::decreasing (), 
				  VCheck::min_size_1 ());
  syntax.add_check ("zplus", zplus_check);
  syntax.add ("xplus", "cm", Check::positive (), 
	      Syntax::Const, Syntax::Sequence,
	      "Horizontal end of each numeric layer (a positive number).\n\
The end points are listed ascending from left (0.0) to right.");
  static VCheck::All xplus_check (VCheck::increasing (), 
				  VCheck::min_size_1 ());
  syntax.add_check ("xplus", xplus_check);
}
  
GeometryRect::GeometryRect (Block& al)
  : Geometry (al), 
    cell_rows_ (al.number_sequence ("zplus").size ()),
    cell_columns_ (al.number_sequence ("xplus").size ())
{
  // Initialize base.
  size_ = cell_columns_ * cell_rows_;

  // Extract grid information from parameters.
  const std::vector<double> z_end (al.number_sequence ("zplus"));
  std::vector<double> z_center;
  std::vector<double> z_distance;
  initialize_intervals (z_end, z_center, z_distance);
  const std::vector<double> x_end (al.number_sequence ("xplus"));
  std::vector<double> x_center;
  std::vector<double> x_distance;
  initialize_intervals (x_end, x_center, x_distance);

  // Fill in cells by column, starting from the top left corner.
  size_t next_cell = 0;
  for (size_t column = 0; column < cell_columns_; column++)
    {
      // Top edge.
      size_t last_cell = cell_above;
      
      for (size_t row = 0; row < cell_rows_; row++)
        {
          // Cell
          zplus_.push_back (z_end[row]);
          z_.push_back (z_center[row]);
          dz_.push_back (z_distance[row]);
          xplus_.push_back (x_end[column]);
          x_.push_back (x_center[column]);
          dx_.push_back (x_distance[column]);
          // Vertical edge.
          edge_from_.push_back (next_cell);
          edge_to_.push_back (last_cell);
          // Horizontal edge.
          if (column > 0U)
            {
              daisy_assert (next_cell >= cell_rows_);
              edge_from_.push_back (next_cell - cell_rows_);
              edge_to_.push_back (next_cell);
            }
          // Next node.
          last_cell = next_cell;
          next_cell++;
        }
      // Bottom edge.
      edge_from_.push_back (cell_below);
      edge_to_.push_back (last_cell);
    }
  daisy_assert (next_cell == cell_size ());
}

GeometryRect::~GeometryRect ()
{ }

static Submodel::Register 
geometry_rect_submodel ("GeometryRect", GeometryRect::load_syntax);
