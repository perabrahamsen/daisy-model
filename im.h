// im.h
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


#ifndef IM_H
#define IM_H

#include <string>

struct Log;
struct Syntax;
struct AttributeList;

class IM
{
  // Content.
public:
  double NH4;
  double NO3;

  // Operations.
public:
  void output (Log&) const;
  void clear ();
  void operator+= (const IM&);
  void operator-= (const IM&);
  void operator*= (double);
  void operator/= (double);
  bool empty () const;

  // Create. 
public:
  IM operator* (double flux) const;
  IM operator+ (const IM&) const;
  static void define_syntax (Syntax&, AttributeList&, const std::string& dim);
  static void load_ppm (Syntax&, AttributeList&);
  static void load_soil (Syntax&, AttributeList&);
  static void load_soil_flux (Syntax&, AttributeList&);
  static void load_field_flux (Syntax&, AttributeList&);
  IM (const IM& im);
  explicit IM (const AttributeList&);
  explicit IM (const IM&, double flux);
  explicit IM ();
  ~IM ();
};


#endif // IM_H
