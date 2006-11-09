// volume.C - a subset of 3D space.
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

#include "volume.h"

template<>
Librarian<Volume>::Content* Librarian<Volume>::content = NULL;

const char *const Volume::description = "\
A subset of 3D space.";

Volume::Volume (Block& al)
  : name (al.identifier ("type"))
{ }

Volume::~Volume ()
{ }

// volume.C ends here.

#if 0
struct ext_number
{
  // Content.
  enum type {
    minus_infinite,
    finite,
    plus_infinite } state;
  double value;
  
  // Create and Destroy.
  ext_number (double v)
    : state (finite),
      value (v)
  { }
  ext_number (type s)
    : state (s),
      value (-42.42e42)
  { daisy_assert (state != finite); }
  ext_number (const ext_number& other)
    : state (other.state),
      value (other.value)
  { }
};

struct ext_interval
{
  // Content.
  ext_number from;
  ext_number to;

  // Create and Destroy;
  ext_interval (const ext_number& f, const ext_number& t)
    : from (f),
      to (t)
  { daisy_assert (f < t); }
  ext_interval (const ext_interval& other)
    : from (other.from),
      to (other.to)
  { }
};

struct Box
{
  // Content.
  ext_interval x;
  ext_interval y;
  ext_interval z;

  // Create and Destroy.
  Box (const ext_interval& x_, const ext_interval& y _, const ext_interval& z_)
    : x (x_),
      y (y_),
      z (z_)
  { }
};

#endif
