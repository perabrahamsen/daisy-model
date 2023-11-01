// attribute.C - A type system for attributes.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "attribute.h"
#include "assertion.h"
#include <sstream>

symbol 
Attribute::size_name (const int size)
{
  switch (size)
    {
    case Singleton:
      return "singleton";
    case Variable:
      return "variable";
    case CanopyCells:
      return "canopy cells";
    case CanopyEdges:
      return "canopy edges";
    case SoilCells:
      return "soil cells";
    case SoilEdges:
      return "soil edges";
    case Unspecified:
      return "unspecified";
    }
  std::ostringstream tmp;
  tmp << "[" << size << "]";
  return tmp.str ();
}

// Each attribute should have an associated type.

symbol
Attribute::type_name (type t)
{
  static const symbol names[] = 
    { "Number", "Submodel", "PLF", "Boolean", "String",
      "Integer", "Model", "Scalar", "Function", "Reference", "Error" };
  daisy_assert (t >= 0);
  daisy_assert (t < sizeof (names) / sizeof (symbol));
  return names[t];
}

static Attribute::type operator++ (Attribute::type& t)
{ 
  t = static_cast<Attribute::type> (static_cast<int> (t) + 1);
  return t;
}

Attribute::type
Attribute::type_number (const symbol name)
{ 
  for (type i = Number; i != Error; ++i)
    if (name == type_name (i))
      return i;
  return Error;
}

symbol
Attribute::Unknown ()
{
  static const symbol unknown ("<unknown>");
  return unknown; 
}

symbol
Attribute::None ()
{
  static const symbol unit ("<none>");
  return unit;
}

symbol
Attribute::Fraction ()
{
  static const symbol unit ("<fraction>");
  return unit; 
}

symbol
Attribute::User ()
{
  static const symbol unit ("<user>");
  return unit; 
}

symbol 
Attribute::category_name (category c)
{ 
  static const symbol names[] = 
    { "Const", "State", "OptionalState", "OptionalConst", "LogOnly"};

  daisy_assert (c >= 0);
  daisy_assert (c < sizeof (names) / sizeof (symbol));
  return names[c]; 
}

int
Attribute::category_number (const symbol name)
{ 
  static const symbol category_end ("LogOnly");

  for (int i = 0;; i++)
    {
      const symbol entry = category_name (category (i));
      if (name == entry)
        return i;
      else if (entry == category_end)
        return -1;
    }
}

// attribute.C ends her.
