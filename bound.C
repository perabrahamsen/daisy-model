// bound.C --- Specify interval boundary.
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

#define BUILD_DLL

#include "bound.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

// bound component.

const char *const Bound::component = "bound";

symbol
Bound::library_id () const
{
  static const symbol id (component);
  return id;
}

std::string
Bound::describe () const
{
  std::ostringstream tmp;
  switch (type ())
    {
    case none:
      tmp << "none";
      break;
    case full:
      tmp << "full";
      break;
    case finite:
      tmp << value ();
      break;
    }
  return tmp.str ();
}

double 
Bound::value () const
{
  daisy_assert (type_ == finite);
  return value_;
}

void 
Bound::set_finite (const double value)
{
  type_ = finite;
  value_ = value;
}

void 
Bound::set_none ()
{
  type_ = none;
  value_ = -43.43e43;
}

void 
Bound::set_full ()
{
  type_ = full;
  value_ = 68.68e68;
}

Bound::Bound (Block& al, const type_t type, const double value)
  : type_ (type),
    value_ (value)
{ }

Bound::Bound (const char *const, const type_t type, const double value)
  : type_ (type),
    value_ (value)
{ }

Bound::~Bound ()
{ }

// "none" model.
static struct BoundNoneSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new Bound (al, Bound::none, -42.42e42); }
  BoundNoneSyntax ()
    : DeclareModel (Bound::component, "none", "No boundary specified.")
  { }
  void load_frame (Frame& frame) const
  { }
} BoundNone_syntax;

// "full" model.
static struct BoundFullSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new Bound (al, Bound::full, 69.69e69); }
  BoundFullSyntax ()
    : DeclareModel (Bound::component, "full", "\
Maximum value for the interval boundary.")
  { }
  void load_frame (Frame& frame) const
  { }
} BoundFull_syntax;

// finite model.
static struct BoundFiniteSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new Bound (al, Bound::finite, al.number ("bound")); }
  BoundFiniteSyntax ()
    : DeclareModel (Bound::component, "finite", "Finite interval bound.")
  { }
  void load_frame (Frame& frame) const
  {
    
    frame.add ("bound", "cm", Value::Const, "Interval bound to use.");
    frame.order ("bound");

  }
} BoundFinite_syntax;

static struct BoundInit : public DeclareComponent 
{
  BoundInit ()
    : DeclareComponent (Bound::component, "\
Specify one end of an interval boundary.")
  { }
} Bound_init;

// bound.C ends here.
