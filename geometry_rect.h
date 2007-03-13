// geometry_rect.h -- Horizontal and vertical grid lines.
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


#ifndef GEOMETRY_RECT_H
#define GEOMETRY_RECT_H

#include "geometry_vert.h"

class GeometryRect : public GeometryVert
{
  // Parameters.
private:
  const size_t cell_rows_;
  const size_t cell_columns_;
  std::vector<double> xplus_;	// Right edge of each cell.
  std::vector<double> x_;       // Horizontal center of each cell.
  std::vector<double> dx_;      // Horizontal size of each cell.
  std::vector<std::vector<int> > cell_corners_; // The corners of cell.
  std::vector<std::vector<int> > cell_edges_; // Edges connected with cell.
  std::vector<int> edge_from_;
  std::vector<int> edge_to_;
  std::vector<double> edge_area_;
  std::vector<double> edge_length_;
  std::vector<double> edge_area_per_length_;
  std::vector<double> edge_sin_angle_;
  std::vector<double> edge_center_z_;
  std::vector<double> edge_center_x_;
  std::vector<double> corner_z_;
  std::vector<double> corner_x_;
  std::vector<std::vector<int> > edge_corners_;
  
  // Cell operations.
public:
  inline size_t cell_rows () const
  { return cell_rows_; }
  inline size_t cell_columns () const
  { return cell_columns_; }
  inline size_t cell_index (const size_t row, const size_t column) const
  { return column * cell_rows () + row; }
  inline double zminus (size_t n) const // Cell top [cm].
  { return (n % cell_rows_ == 0) ? 0.0 : zplus (n-1U); }
  inline double x (size_t n) const // Cell horizontal center [cm]
  { return x_[n]; }
  inline double dx (size_t n) const // Horizontal width of cell [cm]
  { return dx_[n]; }
private:
  double xplus (size_t n) const; // Right side of cell [cm].
  double xminus (size_t n) const; // Left side of cell [cm].
public:
  inline double cell_volume (size_t n) const // Cell volume [cm^3]
  { return dz (n) * dx (n) * 1.0 /* [cm] */; }
  size_t cell_at (double z, double x, double y) const;
  double fraction_in_z_interval (size_t n, double from, double to) const;
  double fraction_in_volume (size_t n, const Volume& volume) const;
  const std::vector<int>& cell_corners (size_t n) const
  { return cell_corners_[n]; }
private:
  size_t cell_pseudo_size () const // Add top, bottom, left, right, front, back
  { return cell_size () + 6U; }
  size_t cell_pseudo_number (int n) const;
public:
  inline const std::vector<int>& cell_edges (int n) const
  { return cell_edges_[cell_pseudo_number (n)]; }

  // Edge operations.
public:
  inline size_t edge_rows () const
  { return cell_rows () + 1U; }
  inline size_t edge_columns () const
  { return cell_columns () + 1U; }
  inline size_t edge_size () const
  { return edge_from_.size (); }
  inline int edge_from (size_t e) const // Cell where edge originates.
  { return edge_from_[e]; }
  inline int edge_to (size_t e) const   // Cell where edge leads.
  { return edge_to_[e]; }
  inline int edge_other (size_t e, size_t n) const // Other cell at edge.
  { return edge_from (e) == n ? edge_to (e) : edge_from (e); }
  inline double edge_area (size_t e) const // Area connecting cells [cm^2]
  { return edge_area_[e]; }
  inline double edge_length (size_t e) const // Distance between c-cent. [cm^2]
  { return edge_length_[e]; }
  inline double edge_area_per_length (size_t e) const 
  { return edge_area_per_length_[e]; }
  inline double edge_sin_angle (size_t e) const // To horizontal plane -1:1 []
  { return edge_sin_angle_[e]; }
  inline double edge_center_z (size_t e) const // Center z-pos [cm]
  { return edge_center_z_[e]; }
  inline double edge_center_x (size_t e) const // Center x-pos [cm]
  { return edge_center_x_[e]; }
  inline const std::vector<int>& edge_corners (size_t e) const
  { return edge_corners_[e]; }

  // Corners.
public:
  inline size_t corner_size () const
  { return corner_z_.size (); }
  inline size_t corner_rows ()const
  { return cell_rows () + 1; }
  inline size_t corner_columns ()const
  { return cell_columns () + 1; }
  inline size_t corner_index (const size_t row, const size_t column) const
  { return column * corner_rows () + row; }
  inline double corner_z (size_t k) const
  { return corner_z_[k]; }
  inline double corner_x (size_t k) const
  { return corner_x_[k]; }
  
  // Operations on whole volume.
public:
  inline int dimensions () const
  { return 2; }
  inline double surface_area () const // Total surface area [cm^2].
  { return right () * back (); }
  double right () const
  { return xplus_[cell_size () - 1]; }  
  inline double bottom () const // Bottom of deepest cell [cm]
  { return zplus_[cell_rows_ - 1]; }

  // Creation.
public:
  bool check (Treelog&) const;
  bool check_x_border (const double value, Treelog& err) const;
  bool check_y_border (const double value, Treelog& err) const;
  void initialize_zplus (const bool, const std::vector<double>&,
                         const double, const double, Treelog&)
  { }
  static void load_syntax (Syntax& syntax, AttributeList&);
  GeometryRect (Block&);
  ~GeometryRect ();
};

#endif // GEOMETRY_RECT_H
