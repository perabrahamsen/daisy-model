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

#define BUILD_DLL

#include "geometry_rect.h"
#include "volume.h"
#include "check.h"
#include "vcheck.h"
#include "block_model.h"
#include "frame.h"
#include "librarian.h"
#include "treelog.h"
#include "assertion.h"
#include "mathlib.h"
#include <sstream>

size_t 
GeometryRect::cell_at (const double z, const double x, const double) const
{ 
  size_t cell = 0;
  while (xplus_[cell] < x)
    { 
      cell += cell_columns_;
      daisy_assert (cell < cell_size ());
    }      
  while (zplus_[cell] > z)
    {
      cell++;
      daisy_assert (cell < cell_size ());
    }
  return cell;
}

double 
GeometryRect::fraction_in_z_interval (const size_t i, 
                                      const double from, const double to) const
{ return fraction_within (zplus (i), zminus (i), to, from); }

double 
GeometryRect::fraction_in_volume (size_t n, const Volume& volume) const
{ return volume.box_fraction (zplus (n), zminus (n), 
                              xminus (n), xplus (n));
}

bool 
GeometryRect::contain_x (size_t i, double x) const
{ return  xminus (i) <= x && x <= xplus (i); }

bool 
GeometryRect::contain_y (size_t i, double y) const
{ return  front () <= y  && y <= back (); }

double 
GeometryRect::xplus (size_t n) const
{ 
  daisy_assert (n < xplus_.size ());
  return xplus_[n]; 
}

double 
GeometryRect::xminus (size_t n) const
{ return (n < cell_rows_) ? 0.0 : xplus (n-cell_rows_); }

void
GeometryRect::add_soil (std::vector<double>& v, 
                        const double top, const double bottom, 
                        const double left, const double right,
                        const double amount) const
// This function must be fast, as it is called once for each biopore
// row and class.
{
  // Pre-conditions.
  daisy_assert (bottom < top);
  daisy_assert (left < right);

  // Find cols.
  const size_t cols = cell_columns ();
  std::vector<double> col_factor (cols, 0.0);
  size_t col_first = cols;
  size_t col_last = 0;
  for (size_t col = 0; col < cols; col++)
    {
      const size_t cell = cell_index (0, col);
      const double cell_right = xplus (cell);
      const double cell_left = xminus (cell);

      if (col < col_first)      // There yet?
        {
          if (cell_right <= left) // No.
            continue;

          col_first = col;
        }
      const double cover_left = std::max (left, cell_left);
      const double cover_right = std::min (right, cell_right);
      const double cover = (cover_right - cover_left) 
        / (cell_right - cell_left);
      daisy_assert (cover >= 0.0);
      daisy_assert (cover <= 1.0);
      col_factor[col] = cover;

      col_last = col;
      if (cell_right >= right)  // Done?
	break;
    }
  // Find rows.
  const size_t rows = cell_rows ();
  std::vector<double> row_factor (rows, 0.0);
  size_t row_first = rows;
  size_t row_last = 0;
  for (size_t row = 0; row < rows; row++)
    {
      const size_t cell = cell_index (row, 0);
      const double cell_top = zminus (cell);
      const double cell_bottom = zplus (cell);

      if (row < row_first)      // There yet?
        {
          if (cell_bottom >= top) // No.
            continue;

          row_first = row;
        }

      const double cover_bottom = std::max (bottom, cell_bottom);
      const double cover_top = std::min (top, cell_top);
      const double cover = (cover_top - cover_bottom) 
        / (cell_top - cell_bottom);
      daisy_assert (cover >= 0.0);
      daisy_assert (cover <= 1.0);
      row_factor[row] = cover;

      row_last = row;
      if (cell_bottom <= bottom)  // Done?
	break;
    }

  // Add it.
  const double volume = (bottom - top) * (right - left) * (front () - back ());
  const double fill = amount / volume;

  for (size_t row = row_first; row <= row_last; row++)
    {
      const double row_add = fill * row_factor[row];
      for (size_t col = col_first; col <= col_last; col++)
        {
          const size_t i = cell_index (row, col);
          v[i] += row_add * col_factor[col];
        }
    }
}

void 
GeometryRect::fill_xplus (std::vector<double>& result) const
{ 
  const size_t row = 0;
  daisy_assert (cell_rows () > row);

  result.clear ();
  for (size_t col = 0; col < cell_columns (); col++)
    result.push_back (xplus (cell_index (row, col))); 
}

bool 
GeometryRect::check (Treelog&) const
{
  bool ok = true;
  return ok;
}

bool 
GeometryRect::check_x_border (const double value, Treelog& err) const
{
  bool ok = false;

  for (size_t i = 0; i < cell_size (); i++)
    if (approximate (value, xplus (i)))
      ok = true;

  if (!ok)
    {
      std::ostringstream tmp;
      tmp << "No geometric border near " << value 
          << " [cm] on x-axis, log results will be inexact";
      err.warning (tmp.str ());
    }
  return ok;
}

bool 
GeometryRect::check_y_border (const double, Treelog& err) const
{
  err.warning ("Logging on y-axis on a 2D geometry is meaningless");
  return false;
}

void
GeometryRect::load_syntax (Frame& frame)
{ 
  frame.declare ("zplus", "cm", Check::negative (), 
                 Attribute::Const, Attribute::Variable,
                 "Depth of each numeric layer (a negative number).\n\
The end points are listed descending from the surface to the bottom.");
  static VCheck::All zplus_check (VCheck::decreasing (), 
				  VCheck::min_size_1 ());
  frame.set_check ("zplus", zplus_check);
  frame.declare ("xplus", "cm", Check::positive (), 
                 Attribute::Const, Attribute::Variable,
                 "Horizontal end of each numeric layer (a positive number).\n\
The end points are listed ascending from left (0.0) to right.");
  static VCheck::All xplus_check (VCheck::increasing (), 
				  VCheck::min_size_1 ());
  frame.set_check ("xplus", xplus_check);
}
  
GeometryRect::GeometryRect (const Block& al)
  : GeometryVert (al), 
    cell_rows_ (al.number_sequence ("zplus").size ()),
    cell_columns_ (al.number_sequence ("xplus").size ())
{
  // Initialize base.
  size_ = cell_columns () * cell_rows ();

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
  for (size_t column = 0; column < cell_columns (); column++)
    {
      // Top edge.
      size_t last_cell = cell_above;
      double last_z = 0.0;

      for (size_t row = 0; row < cell_rows (); row++)
        {
	  const double next_z = z_center[row];

          // Cell
          zplus_.push_back (z_end[row]);
          z_.push_back (z_center[row]);
          dz_.push_back (z_distance[row]);
          xplus_.push_back (x_end[column]);
          x_.push_back (x_center[column]);
          dx_.push_back (x_distance[column]);
          std::vector<int> cc;
          cc.push_back (corner_index (row,     column    )); // SW
          cc.push_back (corner_index (row,     column + 1)); // SE
          cc.push_back (corner_index (row + 1, column + 1)); // NE
          cc.push_back (corner_index (row + 1, column    )); // NW
          cell_corners_.push_back (cc);

          // Vertical edge.
          edge_from_.push_back (next_cell);
          edge_to_.push_back (last_cell);
          edge_area_.push_back (x_distance[column]);
	  edge_length_.push_back (last_z - next_z);
	  daisy_assert (edge_length_[edge_length_.size () - 1U] > 0.0);
          edge_center_z_.push_back (row == 0 ? 0.0 : z_end[row - 1]);
          edge_center_x_.push_back (x_center[column]);
          std::vector<int> corners;
          corners.push_back (corner_index (row, column    )); // W
          corners.push_back (corner_index (row, column + 1)); // E
          edge_corners_.push_back (corners);
	  
          // Next cell.
          daisy_assert (next_cell == cell_index (row, column));
	  last_z = next_z;
          last_cell = next_cell;
          next_cell++;
        }

      // Bottom edge.
      edge_from_.push_back (cell_below);
      edge_to_.push_back (last_cell);
      edge_area_.push_back (x_distance[column]);
      const double next_z = z_end[cell_rows () - 1];
      edge_length_.push_back (last_z - next_z);
      daisy_assert (edge_length_[edge_length_.size () - 1U] > 0.0);
      edge_center_z_.push_back (z_end[cell_rows () - 1]);
      edge_center_x_.push_back (x_center[column]);
      std::vector<int> corners;
      corners.push_back (corner_index (cell_rows (), column    )); // W
      corners.push_back (corner_index (cell_rows (), column + 1)); // E
      edge_corners_.push_back (corners);
    }
  daisy_assert (next_cell == cell_size ());
  edge_sin_angle_.insert (edge_sin_angle_.end (), edge_from_.size (), 1.0);
  edge_cos_angle_.insert (edge_cos_angle_.end (), edge_from_.size (), 0.0);

  // Horizontal edges.
  for (size_t row = 0; row < cell_rows (); row++)
    {
      edge_center_x_.push_back (0.0);
      double last_x = 0.0;
      edge_from_.push_back (cell_left);
      for (size_t column = 0; column < cell_columns (); column++)
        {
	  const double next_x = x_center[column];
          edge_to_.push_back (cell_index (row, column));
          std::vector<int> corners;
          corners.push_back (corner_index (row    , column)); // W
          corners.push_back (corner_index (row + 1, column)); // W
          edge_corners_.push_back (corners);
          edge_center_x_.push_back (x_end[column]);
	  edge_length_.push_back (next_x - last_x);
	  daisy_assert (edge_length_[edge_length_.size () - 1U] > 0.0);
	  last_x = next_x;
          edge_from_.push_back (cell_index (row, column));
        }
      edge_to_.push_back (cell_right);
      std::vector<int> corners;
      corners.push_back (corner_index (row    , cell_columns ())); // W
      corners.push_back (corner_index (row + 1, cell_columns ())); // W
      edge_corners_.push_back (corners);
      const double next_x = x_end[cell_columns () - 1];
      edge_length_.push_back (next_x - last_x);
      daisy_assert (edge_length_[edge_length_.size () - 1U] > 0.0);
      edge_area_.insert (edge_area_.end (), edge_columns (), z_distance[row]);
      edge_center_z_.insert (edge_center_z_.end (), edge_columns (), 
                             z_center[row]);
    }
  edge_sin_angle_.insert (edge_sin_angle_.end (),
			  edge_from_.size () - edge_sin_angle_.size (), 0.0);
  edge_cos_angle_.insert (edge_cos_angle_.end (),
			  edge_from_.size () - edge_cos_angle_.size (), 1.0);

  // Common stuff.
  build_common ();
  daisy_assert (edge_area_.size () == edge_size ());
  daisy_assert (edge_length_.size () == edge_size ());
  daisy_assert (edge_area_per_length_.size () == edge_size ());

  // Corners.
  corner_x_.insert (corner_x_.end (), corner_rows (), 0.0);
  for (size_t column = 0; column < cell_columns (); column++)
    corner_x_.insert (corner_x_.end (), corner_rows (), x_end[column]);

  for (size_t column = 0; column < corner_columns (); column++)
    {
      corner_z_.push_back (0.0);
      for (size_t row = 0; row < cell_rows (); row++)
        corner_z_.push_back (z_end[row]);
    }

  corner_cells_.insert (corner_cells_.end (), corner_size (),
                        std::vector<int> ());
  daisy_assert (corner_cells_.size () == corner_size ());
  for (size_t cell = 0; cell < cell_size (); cell++)
    {
      daisy_assert (cell < cell_corners_.size ());
      for (size_t i = 0; i < cell_corners_[cell].size (); i++)
        {
          const int corner = cell_corners_[cell][i];
          daisy_assert (corner >= 0);
          daisy_assert (corner < corner_cells_.size ());
          corner_cells_[corner].push_back (cell);
        }
    }

  // Done.
  daisy_assert (zplus_.size () == cell_size ());
  daisy_assert (z_.size () == cell_size ());
  daisy_assert (dz_.size () == cell_size ());
  daisy_assert (xplus_.size () == cell_size ());
  daisy_assert (x_.size () == cell_size ());
  daisy_assert (dx_.size () == cell_size ());
  daisy_assert (cell_corners_.size () == cell_size ());
  daisy_assert (edge_center_z_.size () == edge_size ());
  daisy_assert (edge_center_x_.size () == edge_size ());
  daisy_assert (edge_area_.size () == edge_size ());
  daisy_assert (edge_from_.size () == edge_size ());
  daisy_assert (edge_to_.size () == edge_size ());
  daisy_assert (edge_corners_.size () == edge_size ());
  daisy_assert (edge_size () == (edge_rows () * cell_columns () 
                                 + edge_columns () * cell_rows ()));
  daisy_assert (corner_z_.size () == corner_size ());
  daisy_assert (corner_x_.size () == corner_size ());
  for (size_t row = 0; row < cell_rows (); row++)
    for (size_t column = 0; column < cell_columns (); column++)
      {
        const size_t cell = cell_index (row, column);
        const std::vector<int>& corners = cell_corners (cell);
        daisy_assert (corners.size () == 4);
        const double nw_z = corner_z (corners[0]);
        const double nw_x = corner_x (corners[0]);
        const double ne_z = corner_z (corners[1]);
        const double ne_x = corner_x (corners[1]);
        const double se_z = corner_z (corners[2]);
        const double se_x = corner_x (corners[2]);
        const double sw_z = corner_z (corners[3]);
        const double sw_x = corner_x (corners[3]);
        const double z = this->cell_z (cell);
        const double x = this->cell_x (cell);
        daisy_assert (z < nw_z);
        daisy_assert (z < ne_z);
        daisy_assert (z > se_z);
        daisy_assert (z > sw_z);
        daisy_assert (x > nw_x);
        daisy_assert (x < ne_x);
        daisy_assert (x < se_x);
        daisy_assert (x > sw_x);
      }
}

GeometryRect::~GeometryRect ()
{ }

static DeclareSubmodel
geometry_rect_submodel (GeometryRect::load_syntax, "GeometryRect", "\
A rectangular discretization of the soil.");

// geometry_rect.C ends here.
