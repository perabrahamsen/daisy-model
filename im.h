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

#include "symbol.h"
#include "syntax.h"
#include <map>

class Log;
class Syntax;
class AttributeList;
class Block;

class Scalar 
{
  // Content.
private:
  const symbol dim;
  const double val;

  // Use.
public:
  symbol dimension () const
  { return dim; }
  double value () const
  { return val; }

  // Create and Destroy.
public:
  Scalar (const double v, const symbol d)
    : dim (d),
      val (v)
  { }
  Scalar (const double v, const char *const d)
    : dim (d),
      val (v)
  { }
  Scalar (const Scalar& s)
    : dim (s.dim),
      val (s.val)
  { }
private:
  Scalar& operator= (const Scalar&); // Disable
};

class IM
{
  // Utility.
public:
  static symbol storage_unit (); // [g/cm^2]
  static symbol flux_unit ();    // [g/cm^2/h]
  static symbol solute_unit ();  // [g/cm^2/mm]

  // Content.
private:
  symbol dimension;
  std::map<symbol, double> content;

  // Accessors.
public:
  double get_value (symbol chem, symbol dim) const;
  void set_value (symbol chem, symbol dim, double value);
  void add_value (symbol chem, symbol dim, double value);
private:
  double get_value_raw (symbol chem) const;
  void set_value_raw (symbol chem, double value);

  // Iterate.
public:
  struct const_iterator
  {
    std::map<symbol, double>::const_iterator i;

    symbol operator* () const
    { return (*i).first; }
    bool operator!= (const_iterator j)
    { return i != j.i; }
    const_iterator operator++(int)
    { const_iterator old = *this; i++; return old; }
    const_iterator operator++()
    { i++; return *this; }
    explicit const_iterator (std::map<symbol, double>::const_iterator x)
      : i (x)
    { }
  };
  const_iterator begin () const
  { return const_iterator (content.begin ()); }
  const_iterator end () const
  { return const_iterator (content.end ()); }

  // Operations.
public:
  void output (Log&) const;
  void rebase (const symbol dim);
  void rebase (const char* dim);
  void operator+= (const IM&);
  IM operator+ (const IM&) const;
  void operator*= (const Scalar&);
  IM operator* (const Scalar&) const;
  IM& operator= (const IM&);
  void clear ();

  // Create. 
public:
  static void add_syntax (Syntax& parent_syntax, AttributeList& parent_alist,
			  Syntax::category cat, 
			  const char *const key,
			  const symbol dimension,
			  const char *const description);
  explicit IM (Block&, const char* key);
  explicit IM ();
  IM (const IM& im);
  explicit IM (symbol dim);
  explicit IM (symbol dim, const IM&);
  ~IM ();
};

#endif // IM_H
