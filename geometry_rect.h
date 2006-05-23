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
  const size_t cell_rows_;
  const size_t cell_columns_;
  std::vector<double> xplus_;	// Right edge of each cell.
  std::vector<double> x_;       // Horizontal center of each cell.
  std::vector<double> dx_;      // Horizontal size of each cell.
  std::vector<int> edge_from_;
  std::vector<int> edge_to_;
  std::vector<double> edge_area_;
  
  // Accessors.
public:
  inline size_t edge_size () const
  { return edge_from_.size (); }
  inline int dimensions () const
  { return 2; }
  inline int edge_from (size_t e) const // Cell where edge originates.
  { return edge_from_[e]; }
  inline int edge_to (size_t e) const   // Cell where edge leads.
  { return edge_to_[e]; }
  inline double edge_area (size_t e) const // Area connecting cells [cm^2]
  { return edge_area_[e]; }
  inline double surface_area () const // Total surface area.
  { return xplus_[cell_size () - 1] * 1.0 /* cm */; }
  inline double zminus (size_t n) const
  { return (n % cell_rows_ == 0) ? 0.0 : zplus (n-1U); }
  inline double x (size_t n) const // Cell horizontal center [cm]
  { return x_[n]; }
  inline double volume (size_t n) const // Cell volume [cm^3]
  { return dz_[n] * dx_[n] * 1.0 /* [cm] */; }
  inline double bottom () const // Bottom of deepest cell [cm]
  { return zplus_[cell_rows_ - 1]; }
  size_t cell_at (double z, double x, double y) const;
  double fraction_in_z_interval (size_t n, double from, double to) const;
  bool edge_cross_z (size_t e, double z) const; // Cross depth?
  bool contain_z (size_t n, double z) const;

  // Layers -- Support initializing soil arrays layer by layer.
  void initialize_layer (std::vector<double>& value, 
                         const AttributeList& al, 
                         const std::string& name, Treelog&) const;

  // Creation.
public:
  bool check (Treelog&) const;
  bool check_border (const double border, Treelog& err) const;
  void initialize_zplus (const bool, const std::vector<double>&,
                         const double, const double, Treelog&)
  { }
  static void load_syntax (Syntax& syntax, AttributeList&);
  GeometryRect (Block&);
  ~GeometryRect ();
};

#endif // GEOMETRY_RECT_H
