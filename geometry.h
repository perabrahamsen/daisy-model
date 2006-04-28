// geometry.h -- Abstract interface to geometric information.
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


#ifndef GEOMETRY_H
#define GEOMETRY_H

#include "syntax.h"
#include "mathlib.h"
#include <vector>
#include <string>

class Block;
class AttributeList;
class Treelog;
class Groundwater;

class Geometry
{
  // Pseudo-node numbers.
protected:
  static const int edge_top = -13311331;
  static const int edge_bottom = -424242;
  // Parameters.
protected:
  size_t size_;		// Number of intervals.
  
public:

  // Accessors.
  inline size_t node_size () const // Number of nodes.
  { return size_; }
  virtual size_t edge_size () const = 0; // Number of edges.
  std::string node_name (int) const; // For array logging.
  virtual std::string edge_name (size_t) const;
  virtual int dimensions () const = 0; // Number of non-trivial dimensions.
  virtual int edge_from (size_t) const = 0; // Node where edge originates.
  virtual int edge_to (size_t) const = 0; // Node where edge leads.
  virtual double z (size_t) const = 0; // Node depth [cm]
  double z_safe (int) const;    // Same, handles edge_top and edge_bottom.
  virtual double x (size_t) const 
  { return 0.5; }
  virtual double y (size_t) const 
  { return 0.5; }
  virtual double volume (size_t) const = 0; // Node volume [cm^3]
  inline double top () const    // Top of highest node. [cm]
  { return 0.0; }
  virtual double bottom () const = 0; // Bottom of deepest node. [cm]
  virtual double fraction_in_z_interval (// The fraction of a node
                                         // volume that is within a
                                         // specific depth interval.
                                         size_t n, 
                                         double from, double to) const = 0;
  double volume_in_z_interval (double from, double to, 
                               // Find fractions of all nodes in
                               // interval, as well as the total
                               // volume.
                               std::vector<double>& frac) const;
  bool edge_cross_z (size_t e, double z) const; // Cross depth?
  virtual bool contain_z (size_t n, double z) const = 0; // True iff node i
                                                         // includes depth z
  template<class T> // Here we we calculate a volume weighted average
                    // value at a specific depth.
  double content_at (T& obj, double (T::*content) (size_t),
                     const double z) const
  {
    double total_volume = 0.0;
    double total_content = 0.0;
  
    for (size_t i = 0; i < this->node_size (); i++)
      if (this->contain_z (i, z))
        {
          const double volume = this->volume (i);
          total_volume += volume;
          total_content += volume * (obj.*content) (i);
        }
    if (!std::isnormal (total_volume))
      return 0.0;
    
    return total_content / total_volume;
  }

  // Vector operations.
  void mix (std::vector<double>& v, double from, double to) const;
  void mix (std::vector<double>& v, double from, double to, 
            std::vector<double>& change) const;
  void add (std::vector<double>& v,
            double from, double to, double amount) const;
  void add (std::vector<double>& v, const std::vector<double>& density,
            double amount) const;
  double extract (std::vector<double>& v, double from, double to) const;
  void set (std::vector<double>& v,
            double from, double to, double amount) const;
  void swap (std::vector<double>& v,
             double from, double middle, double to) const;
  void swap (std::vector<double>& v, 
             double from, double middle, double to, 
             std::vector<double>& change) const;
  double total (const std::vector<double>& v) const;
  double total (const std::vector<double>& v, double from, double to) const;

  // Layers -- Support initializing soil arrays layer by layer.
  static void add_layer (Syntax& syntax, Syntax::category, 
                         const std::string& name,
			 const std::string& dimension,
                         const std::string& description);
  virtual void initialize_layer (std::vector<double>& value, 
                                 const AttributeList& al, 
                                 const std::string& name, Treelog&) const = 0;

  // Creation.
public:
  virtual bool check (Treelog&) const = 0;
  virtual bool check_border (const double border, Treelog& err) const = 0;
  Geometry (Block&);
  virtual void initialize_zplus (const Groundwater& groundwater,
                                 const std::vector<double>& fixed,
                                 const double max_rooting_depth,
                                 const double max_interval,
                                 Treelog& msg) = 0;
  virtual ~Geometry ();
};

#endif // GEOMETRY_H
