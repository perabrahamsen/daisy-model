// geometry.h
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


#ifndef GEOMETRY_H
#define GEOMETRY_H

#include "common.h"
#include <vector>

class AttributeList;
class Syntax;
class Treelog;

class Geometry
{
  const vector<double> zplus_;	// Lower boundary of each interval.
  vector<double> z_;		// (c) Center of each interval.
  vector<double> dz_;		// (c) Size of each interval.
  const unsigned int size_;
public:
  // Accessors.
  inline unsigned int size () const
  { return size_; }
  inline double zplus (unsigned int i) const
  { return zplus_[i]; }
  inline double z (unsigned int i) const
  { return z_[i]; }
  inline double dz (unsigned int i) const
  { return dz_[i]; }
  unsigned int interval_plus (double z) const;
  unsigned int interval_border (double z) const;

  // Vector operations.
  void mix (vector<double>& v, double from, double to) const;
  void add (vector<double>& v, double from, double to, double amount) const;
  double extract (vector<double>& v, double from, double to) const;
  void set (vector<double>& v, double from, double to, double amount) const;
  void swap (vector<double>& v, double from, double middle, double to) const;
  double total (const vector<double>& v) const;
  double total (const vector<double>& v, double from, double to) const;

  // Layers -- Support initializing soil arrays layer by layer.
  static void add_layer (Syntax& syntax, const string& name,
			 const string& dimension, const string& description);
  void initialize_layer (vector<double>& value, 
			 const AttributeList& al, 
			 const string& name, Treelog&) const;

  // Creation.
  bool check (Treelog&) const;
  static void load_syntax (Syntax&, AttributeList&);
  Geometry (const AttributeList&);
  virtual ~Geometry ();
};

#endif // GEOMETRY_H
