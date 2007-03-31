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
#include "condition.h"		// Needed for proper initialization.
#include "number.h"             // Ditto.
#include "model.h"
#include "symbol.h"
#include "units.h"
#include "volume.h"
#include <vector>

class Geometry;
class Soil;
class Time;
class Treelog;
class Format;
class Border;
class Volume;
class Syntax;
class Block;

struct Handle
{
  // Enum in a namespace.
  enum handle_t { min, max, average, geometric, sum, current };
private:
  handle_t value;
  static handle_t symbol2handle (symbol s);
public:
  operator handle_t ()
  { return value; }
  Handle (handle_t v)
    : value (v)
  { }
  Handle (symbol s)
    : value (symbol2handle (s))
  { }
};

class Select : public Model 
{
  // Content.
public:
  const symbol name;
private:
  struct Implementation;
  std::auto_ptr<Implementation> impl;
protected:
  MultiDest dest;
public:
  const bool accumulate;	// Accumulate numbers over time.
  Handle handle;
protected:
  const bool interesting_content; // Is this worth an initial line?
  double convert (double) const; // Convert value.
  int count;			// Number of accumulated values.
public:
  static const char *const description;
  static const char *const component;
  std::string get_description () const;
  enum type_t { NumberSingleton, NumberSequence };
  virtual type_t type () const = 0;
  virtual symbol dimension () const;
  virtual symbol tag () const;
  virtual const Geometry* geometry () const; // For array tags.
  virtual int size () const;	// For array tags.
  static symbol select_get_tag (const AttributeList& al);

  // Nesting.
public:
  std::vector<symbol> path;		// Content of this entry.
  const unsigned int last_index;	// Index of last member in path.
  symbol current_name;
  static const symbol wildcard;
  
public:
  bool valid (const symbol name) const
  { return current_name == wildcard || name == current_name; }
  bool open (const symbol name, const int depth)	// Open one leaf level.
  { 
    // Check if next level is also in path.
    if (!valid (name))
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
  virtual void output_number (double);
  virtual void output_integer (int);
  virtual void output_name (symbol);
  virtual void output_array (const std::vector<double>&,
                             const Geometry*, const Soil*, Treelog&);

  // Reset at start of time step.
public:
  bool is_active;		// Use by log_all.C.
  bool match (bool is_printing)
  { 
    is_active = (handle != Handle::current) || is_printing;
    return is_active;
  }
  bool initial_match ()
  { 
    is_active = (handle == Handle::current);
    return is_active;
  }
  // Print result at end of time step.
  virtual void done (double dt) = 0;
  virtual bool prevent_printing ();

  // Create and Destroy.
public:
  void document (Format&) const;
  static void load_syntax (Syntax&, AttributeList&);
protected:
  virtual symbol
  /**/ default_dimension (symbol spec_dim) const;
  virtual const Units::Convert* 
  /**/ special_convert (symbol has, symbol want);
public:
  virtual bool initialize (const Volume&,
                           const std::string& timestep, Treelog&);
  void add_dest (Destination* dest);
  virtual bool check (Treelog& err) const;
  virtual bool check_border (const Border&, const Volume& volume,
                             Treelog&) const;
protected:
  Select (Block& al);
public:
  ~Select ();
};

#endif // SELECT_H
