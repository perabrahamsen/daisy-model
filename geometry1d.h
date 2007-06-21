// geometry1d.h -- Soil discretization based on horizontal layers.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2006 Per Abrahamsen and KVL.
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


#ifndef GEOMETRY1D_H
#define GEOMETRY1D_H

#include "geometry_vert.h"
#include <vector>
#include <string>

class Block;
class AttributeList;
class Treelog;

class Geometry1D : public GeometryVert
{
public:
  // Accessors.
  inline size_t edge_size () const
  { return cell_size () + 1; }
  std::string edge_name (const size_t) const;
  inline int dimensions () const // Number of non-trivial dimensions.
  { return 1; }
  inline int edge_from (size_t e) const // Cell where edge originates.
  { return e == cell_size () ? cell_below : static_cast<int> (e); }
  inline int edge_to (size_t e) const // Cell where edge leads.
  { return e == 0 ? cell_above : static_cast<int> (e) - 1; };
  inline double edge_area (size_t) const // Area connecting the cells.
  { return 1.0; }
  inline double edge_center_z (size_t e) const
  { return (e == 0) ? 0.0 : zplus (e - 1); }
  inline double edge_sin_angle (size_t) const // Positive upwards. 
  { return 1.0; }
  inline double edge_cos_angle (size_t) const // Vertical
  { return 0.0; }
  inline double surface_area () const // Total surface area.
  { return 1.0; }
  inline double zminus (size_t n) const
  { return (n == 0) ? 0.0 : zplus (n-1U); }
  inline double cell_volume (size_t n) const
  { return dz (n) * 1.0 /* [cm] */ * 1.0 /* [cm] */; }
  inline double bottom () const // Bottom of deepest cell. [cm]
  { return zplus (cell_size () - 1); }
  size_t cell_at (double z, double x, double y) const ;
  double fraction_in_z_interval (size_t i, double from, double to) const;
  double fraction_in_volume (size_t n, const Volume& volume) const;

  size_t interval_plus (double z) const;
  size_t interval_border (double z) const;

  // Creation.
public:
  bool check (Treelog&) const;
  bool check_x_border (const double value, Treelog& err) const;
  bool check_y_border (const double value, Treelog& err) const;
  static void load_syntax (Syntax&, AttributeList&);
  Geometry1D (Block&);
  void initialize_zplus (bool volatile_bottom,
			 const std::vector<double>& fixed,
			 const double max_rooting_depth,
			 const double max_interval,
			 Treelog& msg);
  virtual ~Geometry1D ();
};

#endif // GEOMETRY1D_H
