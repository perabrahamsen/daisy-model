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


#include "bound.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include <sstream>

// bound component.

const char *const Bound::description = "\
Specify one end of an interval boundary.";

const char *const Bound::component = "bound";

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
  : name (al.identifier ("type")),
    type_ (type),
    value_ (value)
{ }

Bound::Bound (const char *const id, const type_t type, const double value)
  : name (id),
    type_ (type),
    value_ (value)
{ }

Bound::~Bound ()
{ }

// "none" model.
static struct BoundNoneSyntax
{
  static Model& make (Block& al)
  { return *new Bound (al, Bound::none, -42.42e42); }
  BoundNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No boundary specified.");
    Librarian<Bound>::add_type ("none", alist, syntax, &make);
  }
} BoundNone_syntax;

const AttributeList& 
Bound::none_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
      alist.add ("type", "none");

  return alist;
}

// "full" model.
static struct BoundFullSyntax
{
  static Model& make (Block& al)
  { return *new Bound (al, Bound::full, 69.69e69); }
  BoundFullSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Maximum value for the interval boundary.");
    Librarian<Bound>::add_type ("full", alist, syntax, &make);
  }
} BoundFull_syntax;

// finite model.
static struct BoundFiniteSyntax
{
  static Model& make (Block& al)
  { return *new Bound (al, Bound::finite, al.number ("bound")); }
  BoundFiniteSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Finite interval bound.");
    
    syntax.add ("bound", "cm", Syntax::Const, "Interval bound to use.");
    syntax.order ("bound");

    Librarian<Bound>::add_type ("finite", alist, syntax, &make);
  }
} BoundFinite_syntax;

// bound.C ends here.
