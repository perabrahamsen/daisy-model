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

#include "condition.h"		// Needed for proper initialization.
#include "librarian.h"
#include <map>

struct Geometry;
struct Daisy;

typedef map<string, string, less<string>/**/> string_map;

class Select
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
protected:
  const bool accumulate;	// Accumulate numbers over time.
  double convert (double) const; // Convert value.
  int count;			// Number of accumulated values.
public:
  static const char *const description;
  virtual const string& dimension () const;
  virtual const string& tag () const;
  virtual const Geometry* geometry () const; // For array tags.
  virtual int size () const;	// For array tags.
  
  // Destination
  class Destination
  {
    // Add data.
  public:
    virtual void error (const string& tag) = 0;
    virtual void missing (const string& tag) = 0;
    virtual void add (const string& tag, const vector<double>& value) = 0;
    virtual void add (const string& tag, double value) = 0;
    virtual void add (const string& tag, const string& value) = 0;

    // Create and Destroy
  public:
    Destination ();
    virtual ~Destination ();
  };

  // Nesting.
public:
  bool is_active ();		// Active time step.
  bool valid ();		// Currently path entry is valid.
  bool valid (const string& next); // Next path entry is valid.

  void open_group (const string&); // Open one group level.
  void open (const string&);	// Open one leaf level.
  void close ();		// Close one level.

  // Output routines.
  virtual void output_time (const string& name, const Time& time);
  virtual void output_number (const string& name, const double number);
  virtual void output_integer (const string& name, const int integer);
  virtual void output_name (const string& name, const string& a_name);
  virtual void output_array (const string& name, const vector<double>& array,
			     const Geometry* geometry);

  // Reset at start of time step.
  bool match (const Daisy& daisy, Treelog&, bool is_printing);

  // Print result at end of time step.
  virtual void done (Destination& dest) = 0;
  virtual bool prevent_printing ();

  // Create and Destroy.
protected:
  virtual const string default_dimension (const string& spec_dim) const;
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual void initialize (const string_map& conv, double from, double to,
			   const string& timestep);
  virtual bool check (Treelog& err) const;
protected:
  Select (const AttributeList& al);
public:
  virtual ~Select ();
};

static Librarian<Select> Select_init ("select");

#endif // SELECT_H
