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
class Volume;

class Geometry
{
  // Pseudo-cell numbers.
public:
  static const int cell_above = -13311331;
  static const int cell_below = -424242;
  static const int cell_left = -123456;
  static const int cell_right = -654321;
  static const int cell_front = -9999;
  static const int cell_back = -8888;

  // Parameters.
protected:
  size_t size_;		// Number of cells.

  // Helper data.
protected:
  std::vector<std::vector<int> > cell_edges_; // Edges connected with cell.
  std::vector<double> edge_length_;	      // Distance between cell centers.
  std::vector<double> edge_area_;             // Area connecting cells.
  std::vector<double> edge_area_per_length_;  // One divided by the other.
  
  // Cell operations.
public:
  inline size_t cell_size () const // Number of cells.
  { return size_; }
  inline bool cell_is_internal (int cell) const
  { return cell >= 0; }
  std::string cell_name (int) const; // For array logging.
  virtual double z (size_t) const = 0; // Cell center depth [cm]
  double z_safe (int) const;    // Same, handles cell_top and cell_bottom.
  virtual double x (size_t) const 
  { return 0.5; }
  double x_safe (int) const;    // Same, handles cell_left and cell_right.
  virtual double y (size_t) const 
  { return 0.5; }
  double y_safe (int) const;    // Same, handles cell_front and cell_back.
  virtual double cell_volume (size_t) const = 0; // Cell volume [cm^3]
  virtual size_t cell_at (double z, double x, double y) const = 0;
  virtual double fraction_in_z_interval (// The fraction of a cell
                                         // volume that is within a
                                         // specific depth interval.
                                         size_t n, 
                                         double from, double to) const = 0;
  virtual double fraction_in_volume (// The fraction of a cell
                                     // volume that is within a
                                     // specific volume.
                                     size_t n, 
                                     const Volume& volume) const = 0;
  virtual bool contain_z (size_t n, double z) const = 0; // True iff cell n
                                                         // includes depth z
  virtual bool contain_x (size_t n, double x) const = 0; // True iff cell n
                                                         // includes width z
  virtual bool contain_y (size_t n, double y) const = 0; // True iff cell n
                                                         // includes length y
  bool node_center_in_volume (int c, const Volume& volume) const;
protected:
  size_t cell_pseudo_size () const // Add top, bottom, left, right, front, back
  { return cell_size () + 6U; }
  size_t cell_pseudo_number (int n) const;
public:
  inline const std::vector<int>& cell_edges (int n) const
  { return cell_edges_[cell_pseudo_number (n)]; }

  // Edge operations.
public:
  virtual size_t edge_size () const = 0; // Number of edges.
  virtual std::string edge_name (size_t) const;
  virtual int edge_index (int from, int to) const; // Find edge between cells.
  virtual int edge_from (size_t) const = 0; // Cell where edge originates.
  virtual int edge_to (size_t) const = 0; // Cell where edge leads.
  inline int edge_other (size_t e, size_t n) const // Other cell at edge.
  { return edge_from (e) == n ? edge_to (e) : edge_from (e); }
  inline bool edge_is_internal (size_t e) const // Edge does not lead out of volume.
  { return cell_is_internal (edge_from (e))
      && cell_is_internal (edge_to (e)); }

  inline double edge_length (size_t e) const // Distance between c-cent. [cm]
  { return edge_length_[e]; }
  inline double edge_area (size_t e) const // Area connecting cells [cm^2]
  { return edge_area_[e]; }
  inline double edge_area_per_length (size_t e) const // [cm]
  { return edge_area_per_length_[e]; }
  bool edge_cross_z (size_t e, double z) const; // Cross depth?
  virtual double edge_center_z (size_t e) const = 0;
  virtual double edge_center_x (size_t) const
  { return 0.5; }
  virtual double edge_center_y (size_t) const
  { return 0.5; }
  virtual double edge_sin_angle (size_t e) const = 0; // Rel. hor. plane [-1:1]
  virtual double edge_cos_angle (size_t e) const; // Rel. hor. plane [-1:1]

  // Operations on whole volume.
public:
  virtual int dimensions () const = 0; // Number of non-trivial dimensions.
  virtual double surface_area () const = 0; // Total surface area. [cm^2]
  inline double top () const    // Top of highest cell. [cm]
  { return 0.0; }
  virtual double bottom () const = 0; // Bottom of deepest cell. [cm]
  inline double left () const    // Left side of leftmost cell. [cm]
  { return 0.0; }
  virtual double right () const; // Right side of rightmost cell. [cm]
  inline double front () const    // Front of nearest cell. [cm]
  { return 0.0; }
  inline double back () const // Back of farthest cell. [cm]
  { return 1.0; }

  template<class T> // Here we we calculate a volume weighted average
                    // value at a specific depth.
  double content_at (T& obj, double (T::*content) (size_t),
                     const double z) const
  {
    double total_volume = 0.0;
    double total_content = 0.0;
  
    for (size_t i = 0; i < this->cell_size (); i++)
      if (this->contain_z (i, z))
        {
          const double volume = cell_volume (i);
          total_volume += volume;
          total_content += volume * (obj.*content) (i);
        }
    if (iszero (total_volume))
      return 0.0;
    
    return total_content / total_volume;
  }

  // Vector operations.
private:
  double volume_in_z_interval (double from, double to, 
                               // Find fractions of all cells in
                               // interval, as well as the total
                               // volume.
                               std::vector<double>& frac) const;
public:
  void mix (std::vector<double>& v, double from, double to) const;
  void mix (std::vector<double>& v, const Volume&) const;
  void mix (std::vector<double>& v, double from, double to, 
            std::vector<double>& change, double dt) const;
  void mix (std::vector<double>& v, const Volume&,
            std::vector<double>& change, double dt) const;
  void add_soil (std::vector<double>& v,
                 double from, double to, double amount) const;
  void add_soil (std::vector<double>& v, const Volume&, double amount) const;
  void add_soil (std::vector<double>& v, const std::vector<double>& density,
                 double amount) const;
  void add_surface (std::vector<double>& v /* [X] */,
                    const double from /* [cm] */, const double to /* [cm] */,
                    const double amount /* [X/cm^2] */) const;
  void add_surface (std::vector<double>& v /* [X] */,
                    const std::vector<double>& density,
                    const double amount /* [X/cm^2] */) const;
  void add_surface (std::vector<double>& v /* [X] */,
                    const Volume&,
                    const double amount /* [X/cm^2] */) const;
  double extract_soil (std::vector<double>& v, double from, double to) const;
  double extract_soil (std::vector<double>& v, const Volume&) const;
  double extract_surface (std::vector<double>& v, 
                          double from, double to) const;
  void set_soil (std::vector<double>& v,
                 double from, double to, double amount) const;
  void set_surface (std::vector<double>& v,
                    const double from, const double to, 
                    const double amount) const;
  void swap (std::vector<double>& v,
             double from, double middle, double to) const;
  void swap (std::vector<double>& v, 
             double from, double middle, double to, 
             std::vector<double>& change, double dt) const;
  double total_soil (const std::vector<double>& v) const;
  double total_soil (const std::vector<double>& v, double from, double to) const;
  double total_surface (const std::vector<double>& v) const;
  double total_surface (const std::vector<double>& v, 
                        const double from, const double to) const;

  // Layers -- Support initializing soil arrays layer by layer.
public:
  static void add_layer (Syntax& syntax, Syntax::category, 
                         const std::string& name,
			 const std::string& dimension,
                         const std::string& description);
  void initialize_layer (std::vector<double>& value, 
			 const AttributeList& al, 
			 const std::string& name, Treelog&) const;

  // Creation.
public:
  virtual bool check (Treelog&) const = 0;
  virtual bool check_z_border (double, Treelog& err) const = 0;
  virtual bool check_x_border (double, Treelog& err) const = 0;
  virtual bool check_y_border (double, Treelog& err) const = 0;
  static void initialize_intervals (const std::vector<double>& end, 
                                    std::vector<double>& center,
                                    std::vector<double>& distance);
  virtual void initialize_zplus (bool volatile_bottom,
                                 const std::vector<double>& fixed,
                                 const double max_rooting_depth,
                                 const double max_interval,
                                 Treelog& msg) = 0;
protected:
  void build_common ();
  Geometry (Block&);
  virtual ~Geometry ();
};

#endif // GEOMETRY_H
