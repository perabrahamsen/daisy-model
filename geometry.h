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

#include "symbol.h"
#include "attribute.h"
#include <vector>
#include <string>
#include <boost/noncopyable.hpp>

class Block;
class Treelog;
class Groundwater;
class Volume;
class Frame;
class Check;

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
  static const int cell_error = -1;

  // Parameters.
protected:
  size_t size_;         // Number of cells.

  // Helper data.
protected:
  std::vector<std::vector<size_t> > cell_edges_; // Edges connected with cell.
  std::vector<double> edge_length_;           // Distance between cell centers.
  std::vector<double> edge_area_;             // Area connecting cells.
  std::vector<double> edge_area_per_length_;  // One divided by the other.
  
  // Cell operations.
public:
  inline size_t cell_size () const // Number of cells.
  { return size_; }
  inline bool cell_is_internal (int cell) const
  { return cell >= 0; }
  bool cell_is_external (int cell) const;
  bool cell_is_valid (int cell) const;
  std::string cell_name (int) const; // For array logging.
  virtual double cell_z (size_t) const = 0; // Cell center depth [cm]
  double z_safe (int) const;    // Same, handles cell_top and cell_bottom.
  virtual double cell_x (size_t) const 
  { return 0.5; }
  double x_safe (int) const;    // Same, handles cell_left and cell_right.
  virtual double cell_y (size_t) const 
  { return 0.5; }
  double y_safe (int) const;    // Same, handles cell_front and cell_back.
  virtual double cell_bottom (size_t) const = 0; // Lowest point in cell [cm]
  virtual double cell_top (size_t) const = 0; // Highest point in cell [cm]
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
  bool cell_center_in_volume (int c, const Volume& volume) const;
protected:
  size_t cell_pseudo_size () const // Add top, bottom, left, right, front, back
  { return cell_size () + 6U; }
  size_t cell_pseudo_number (int n) const;
public:
  inline const std::vector<size_t>& cell_edges (int n) const
  { return cell_edges_[cell_pseudo_number (n)]; }

  // Edge operations.
public:
  virtual size_t edge_size () const = 0; // Number of edges.
  virtual std::string edge_name (size_t) const;
  virtual int edge_index (int from, int to) const; // Find edge between cells.
  virtual int edge_from (size_t) const = 0; // Cell where edge originates.
  virtual int edge_to (size_t) const = 0; // Cell where edge leads.
  inline int edge_other (size_t e, int n) const // Other cell at edge.
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
  virtual double edge_sin_angle (size_t e) const = 0; // Rel. ver. plane [-1:1]
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

  // Generic access.
public:
  // The 'Access' class is simply a function object interface.
  struct Access : private boost::noncopyable
  { 
    virtual double operator()(size_t c) const = 0; 
    virtual ~Access ();
  };
  // The 'Accessor' template class will implement the 'Access'
  // interface for a given soil container and member function.
  template<class T>
  class Accessor : public Access
  { 
    const T& object;
    double (T::*member_function) (size_t) const;
  public:
    double operator()(const size_t c) const
    { return (object.*member_function) (c); }
    Accessor (const T& obj, double (T::*content) (size_t) const)
      : object (obj),
        member_function (content)
    { }
  };
  // The 'content_height' template function will calculate an average
  // content at a specific height, where the content of all the cells
  // that contain the height is weighted by their volume.
  template<class T> 
  double content_height (const T& obj, double (T::*content) (size_t) const,
                         const double z) const
  { 
    const Accessor<T> accessor (obj, content);
    return access_content_height (accessor, z); 
  }
  // The 'access_content_height' function does the same, using the
  // 'Access' class instead of a soil container template.
  double access_content_height (const Access& access, double z) const;
  // The 'content_hood' template function will calculate an average
  // content for the neighbors of a specific cell, weighted by the
  // area of the edges connecting the neighbors to the cell.  This is
  // particularily useful for the pseudo-cells, such as
  // Geometry::cell_above.
  template<class T> 
  double content_hood (const T& obj, double (T::*content) (size_t) const,
                       const int c) const
  { 
    const Accessor<T> accessor (obj, content);
    return access_content_hood (accessor, c);
  }
  // The 'access_content_hood' function does the same, using the
  // 'Access' class instead of a soil container template.
  double access_content_hood (const Access&, int c) const;
  // The 'content_hood' function does the same, for a vector.
  double content_hood (const std::vector<double>&, int c) const;
  // The 'content_cell_or_hood' template function will return the value 
  // for the cell, if internal, or the hood, if external.
  template<class T> 
  double content_cell_or_hood (const T& obj, 
                               double (T::*content) (size_t) const,
                               const int c) const
  { 
    const Accessor<T> accessor (obj, content);
    return access_content_cell_or_hood (accessor, c);
  }
  // The 'access_content_cell_or_hood' function does the same, using the
  // 'Access' class instead of a soil container template.
  double access_content_cell_or_hood (const Access&, int c) const;
  // The 'content_interval' template function will calculate an average
  // content for the cells overlapping a specified interval, weighted
  // by the overlapping volume.
  template<class T> 
  double content_interval (const T& obj, double (T::*content) (size_t) const,
                           const double from, const double to) const
  { 
    const Accessor<T> accessor (obj, content);
    return access_content_interval (accessor, from, to); 
  }
  // The 'access_content_interval' function does the same, using the
  // 'Access' class instead of a soil container template.
  double access_content_interval (const Access&, double from, double to) const;
  // The 'content_volume' template function will calculate an average
  // content for the cells overlapping a specified volume, weighted
  // by volume of the overlap.
  template<class T> 
  double content_volume (const T& obj, double (T::*content) (size_t) const,
                         const Volume& vol) const
  { 
    const Accessor<T> accessor (obj, content);
    return access_content_interval (accessor, vol); 
  }
  // The 'access_content_volume' function does the same, using the
  // 'Access' class instead of a soil container template.
  double access_content_volume (const Access&, const Volume&) const;

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
            std::vector<double>& change) const;
  void mix (std::vector<double>& v, const Volume&,
            std::vector<double>& change) const;
  void add_soil (std::vector<double>& v,
                 double top, double bottom, double amount) const;
  virtual void add_soil (std::vector<double>& v, 
                         const double top, const double bottom, 
                         const double left, const double right,
                         const double amount) const = 0;
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
             std::vector<double>& change) const;
  double total_soil (const std::vector<double>& v) const;
  double total_soil (const std::vector<double>& v, 
                     double from, double to) const;
  double total_soil (const std::vector<double>& v, const Volume&) const;
  double total_surface (const std::vector<double>& v) const;
  double total_surface (const std::vector<double>& v, 
                        const double from, const double to) const;

  // Utilities.
public:
  // Assume 'flux' is specified for all top edges and there is no
  // horizontal flux, calculate vertical flux below from mass balance.
  void biopore_pass_below (const std::vector<double>& from_matrix,
			   std::vector<double>& flux) const;

  // Assume 'flux' is specified for all top and bottom edges and there
  // is no horizontal flux, calculate vertical flux and pipe source
  // (negative) from mass balance.
  void biopore_pass_pipes (const double pipe_position,
			   const std::vector<double>& from_matrix,
			   std::vector<double>& flux,
			   std::vector<double>& S_from_drain) const;

  // Layers -- Support initializing soil arrays layer by layer.
  typedef void (*load_syntax_t) (Frame&);
public:
  static void add_layer (Frame&, symbol dimension, const Check&,
			 Attribute::category, 
                         symbol description);
  static void add_layer (Frame& frame, Attribute::category, 
                         symbol name,
                         load_syntax_t load_syntax);
  void initialize_layer (std::vector<double>& value, 
                         const Frame& al, 
                         symbol name, Treelog&) const;
  virtual void fill_xplus (std::vector<double>&) const = 0;

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
  Geometry (const Block&);
  virtual ~Geometry ();
};

#endif // GEOMETRY_H
