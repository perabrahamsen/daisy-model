// value.C - A type system for values.
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

#include "value.h"
#include "assertion.h"

// Each value should have an associated type.

symbol
Value::type_name (type t)
{
  static const symbol names[] = 
    { "Number", "AList", "PLF", "Boolean", "String",
      "Integer", "Object", "Library", "Error" };
  daisy_assert (t >= 0);
  daisy_assert (t < sizeof (names) / sizeof (symbol));
  return names[t];
}

Value::type operator++ (Value::type& t)
{ 
  t = static_cast<Value::type> (static_cast<int> (t) + 1);
  return t;
}

Value::type
Value::type_number (const symbol name)
{ 
  for (type i = Number; i != Error; ++i)
    if (name == type_name (i))
      return i;
  return Error;
}

symbol
Value::Unknown ()
{
  static const symbol unknown ("<unknown>");
  return unknown; 
}

symbol
Value::None ()
{
  static const symbol unit ("<none>");
  return unit;
}

symbol
Value::Fraction ()
{
  static const symbol unit ("<fraction>");
  return unit; 
}

symbol
Value::User ()
{
  static const symbol unit ("<user>");
  return unit; 
}

symbol 
Value::category_name (category c)
{ 
  static const symbol names[] = 
    { "Const", "State", "OptionalState", "OptionalConst", "LogOnly"};

  daisy_assert (c >= 0);
  daisy_assert (c < sizeof (names) / sizeof (symbol));
  return names[c]; 
}

int
Value::category_number (const symbol name)
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

// value.C ends her.
