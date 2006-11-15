// bound.h --- Specify an interval boundary.
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


#ifndef BOUND_H
#define BOUND_H

#include "librarian.h"

class Bound
{
  // Identity.
public:
  const symbol name;
  static const char *const description;

  // Content.
public:
  enum type_t { none, full, finite };
private:
  type_t type_;
  double value_;
public:
  type_t type () const
  { return type_; }
  double value () const;
  void set_finite (double value);
  void set_none ();
  void set_full ();

  // Create and Destroy.
public:
  static const AttributeList& none_model ();
  Bound (Block& al, type_t type_, double val);
  ~Bound ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Bound>::Content* Librarian<Bound>::content;
#endif

static Librarian<Bound> Bound_init ("bound");

#endif // BOUND_H
