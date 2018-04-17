// select.h --- Select a state variable.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#ifndef SELECT_H
#define SELECT_H

#include "destination.h"
#include "model.h"
#include "symbol.h"
#include "units.h"
#include "volume.h"
#include "attribute.h"
#include <vector>
#include <set>

class Column;
class Geometry;
class Soil;
class Vegetation;
class Time;
class Treelog;
class Format;
class Border;
class Volume;
class BlockModel;
class VCheck;

class Select : public Model 
{
  // Types.
public:
  struct Handle
  {
    // Enum in a namespace.
    enum handle_t { min, max, average, sum, content_sum, current };
  private:
    handle_t value;
    static handle_t symbol2handle (symbol s);
  public:
    operator handle_t () const
    { return value; }
    Handle (handle_t v)
      : value (v)
    { }
    Handle (symbol s)
      : value (symbol2handle (s))
    { }
  };
protected:
  struct Multi
  {
    // Enum in a namespace.
    enum handle_t { min, max, sum };
  private:
    handle_t value;
    static handle_t symbol2handle (symbol s);
  public:
    operator handle_t () const
    { return value; }
    Multi (handle_t v)
      : value (v)
    { }
    Multi (symbol s)
      : value (symbol2handle (s))
    { }
  };

  // Content.
public:
  const symbol name;
  struct Implementation;
private:
  std::unique_ptr<Implementation> impl;
protected:
  MultiDest dest;
public:
  static const VCheck& multi_check ();
public:
  const bool accumulate;	// Accumulate numbers over time.
  Handle handle;
  Multi multi;
protected:
  const bool interesting_content; // Is this worth an initial line?
  double convert (double) const; // Convert value.
  bool first_result;             // First match in small time step.
  bool first_small;              // First match since last print.
  double dt;                    // Time passed since last print [h]
public:
  static const char *const description;
  static const char *const component;
  symbol library_id () const;
  symbol get_description () const;
  Attribute::type type () const
  { return Attribute::Number; }
  virtual int type_size () const = 0;
  int original_size () const;
  virtual symbol dimension () const;
  virtual symbol tag () const;
  virtual const Geometry* geometry () const; // For array tags.
  virtual int size () const;	// For array tags.
  static symbol select_get_tag (const BlockModel& al);
  static symbol select_get_tag (const Frame& al);

  // Nesting.
public:
  std::vector<symbol> path;		// Content of this entry.
  const unsigned int last_index;	// Index of last member in path.
  symbol current_name;
  static const symbol wildcard;
  double relative_weight;
  double total_weight;

public:
  bool valid (const symbol name) const
  { return current_name == wildcard || name == current_name; }
  bool valid (const std::set<symbol>& ancestors) const
  { return current_name == wildcard 
      || ancestors.find (current_name) != ancestors.end (); }
  bool open (const symbol name, const int depth)	// Open one leaf level.
  { 
    // Check if next level is also in path.
    if (!valid (name))
      return false;

    // Direct acces to new head of path.
    current_name = path[depth];
    return true;
  }
  bool open (const std::set<symbol>& ancestors, const int depth) 
  // Derived object.
  { 
    // Check if next level is also in path.
    if (!valid (ancestors))
      return false;

    // Direct acces to new head of path.
    current_name = path[depth];
    return true;
  }

  void close (const int depth)		// Close one level.
  { 
    // And restore direct access to current path.
    current_name = path[depth];
  }

  
  // Output routines.
  virtual void set_column (const Column&, Treelog&);
  virtual void output_number (double);
  virtual void output_integer (int);
  virtual void output_name (symbol);
  virtual void output_array (const std::vector<double>&);

  // Reset at start of time step.
public:
  bool is_active;		// Use by log_all.C.
  bool match (bool is_printing)
  { 
    is_active = (handle != Handle::current) || is_printing;
    return is_active;
  }
  // Print result at end of time step.
  virtual void done_initial () = 0;
  virtual void done_small (double ddt) = 0;
  virtual void done_print () = 0;
  bool print_initial () const
  { return interesting_content; }

  // Create and Destroy.
public:
  void document (Format&) const;
protected:
  virtual symbol
  /**/ default_dimension (symbol spec_dim) const;
  virtual const Convert* 
  /**/ special_convert (const Units&, symbol has, symbol want);
public:
  virtual bool initialize (const Units&, const Volume&,
                           const symbol timestep, Treelog&);
  void add_dest (Destination* dest);
  virtual bool check (Treelog& err) const;
  virtual bool check_border (const Border&, const Volume& volume,
                             Treelog&) const;
protected:
  Select (const BlockModel& al);
public:
  ~Select ();
};

#endif // SELECT_H
