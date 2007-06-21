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
#include "block.h"
#include "alist.h"
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

double 
GeometryRect::fraction_in_volume (size_t n, const Volume& volume) const
{ return volume.box_fraction (zplus (n), zminus (n), 
                              xminus (n), xplus (n));
}

size_t 
GeometryRect::cell_pseudo_number (const int n) const
{
  switch (n)
    {
    case cell_above:
      return cell_size () + 0;
    case cell_below:
      return cell_size () + 1;
    case cell_left:
      return cell_size () + 2;
    case cell_right:
      return cell_size () + 3;
    case cell_front:
      return cell_size () + 4;
    case cell_back:
      return cell_size () + 5;
    default:
      daisy_assert (n >= 0);
      daisy_assert (n < cell_size ());
      return n;
    }
}

double 
GeometryRect::xplus (size_t n) const
{ 
  daisy_assert (n < xplus_.size ());
  return xplus_[n]; 
}

double 
GeometryRect::xminus (size_t n) const
{ return (n < cell_rows_) ? 0.0 : xplus (n-cell_rows_); }

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
          << " [cm]on x-axis, log results will be inexact";
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
GeometryRect::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "GeometryRect");
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

  // Cell edges.
  cell_edges_.insert (cell_edges_.end (), cell_pseudo_size (),
                      std::vector<int> ());
  for (size_t e = 0; e < edge_size (); e++)
    { 
      cell_edges_[cell_pseudo_number (edge_from (e))].push_back (e);
      cell_edges_[cell_pseudo_number (edge_to (e))].push_back (e);
    }

  // Edges.
  daisy_assert (edge_area_.size () == edge_size ());
  daisy_assert (edge_length_.size () == edge_size ());

  for (size_t e = 0; e < edge_size (); e++)
    { 
      const double length = edge_length (e);
      daisy_assert (length > 0.0);
      edge_area_per_length_.push_back (edge_area (e) / length);
    }

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
        const double z = this->z (cell);
        const double x = this->x (cell);
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

static Submodel::Register 
geometry_rect_submodel ("GeometryRect", GeometryRect::load_syntax);
