// geometry1d.h
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

#include "geometry.h"
#include "syntax.h"
#include <vector>
#include <string>

class Block;
class AttributeList;
class Treelog;
class Groundwater;

class Geometry1D : public Geometry
{
  // Parameters.
  std::vector<double> zplus_;	// Lower boundary of each interval.
  std::vector<double> z_;       // Center of each interval.
  std::vector<double> dz_;      // Size of each interval.

public:
  // Accessors.
  inline size_t edge_size () const
  { return node_size () + 1; }
  std::string edge_name (const size_t) const;
  inline int dimensions () const // Number of non-trivial dimensions.
  { return 1; }
  inline int edge_from (size_t e) const // Node where edge originates.
  { return e == node_size () ? edge_bottom : static_cast<int> (e); }
  inline int edge_to (size_t e) const // Node where edge leads.
  { return e == 0 ? edge_top : static_cast<int> (e) - 1; };
  inline double zplus (size_t n) const
  { return zplus_[n]; }
  inline double zminus (size_t n) const
  { return (n == 0) ? 0.0 : zplus (n-1U); }
  inline double z (size_t n) const
  { return z_[n]; }
  inline double dz (size_t n) const
  { return dz_[n]; }
  inline double volume (size_t n) const
  { return dz_[n] * 1.0 /* [cm] */ * 1.0 /* [cm] */; }
  inline double bottom () const // Bottom of deepest node. [cm]
  { return zplus_[node_size () - 1]; }
  double fraction_in_z_interval (size_t i, double from, double to) const;
  bool contain_z (size_t i, double z) const;

  size_t interval_plus (double z) const;
  size_t interval_border (double z) const;

  // Vector operations.
  void swap (std::vector<double>& v,
             double from, double middle, double to) const;

  // Layers -- Support initializing soil arrays layer by layer.
  static void add_layer (Syntax& syntax, Syntax::category, 
                         const std::string& name,
			 const std::string& dimension,
                         const std::string& description);
  void initialize_layer (std::vector<double>& value, 
			 const AttributeList& al, 
			 const std::string& name, Treelog&) const;

  // Creation.
public:
  bool check (Treelog&) const;
  bool check_border (const double border, Treelog& err) const;
  static void load_syntax (Syntax&, AttributeList&);
  Geometry1D (Block&);
  void initialize_zplus (const Groundwater& groundwater,
			 const std::vector<double>& fixed,
			 const double max_rooting_depth,
			 const double max_interval,
			 Treelog& msg);
  virtual ~Geometry1D ();
};

#endif // GEOMETRY1D_H
