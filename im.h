// im.h -- Keep track of inorganic matter.
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
#include "attribute.h"
#include <map>
#include <boost/noncopyable.hpp>

class Log;
class Frame;
class Block;
class Unit;

class Scalar 
{
  // Content.
private:
  const Unit& unit_;
  const double val;

  // Use.
public:
  const Unit& unit () const
  { return unit_; }
  symbol dimension () const;
  double value () const
  { return val; }

  // Create and Destroy.
private:
  const Scalar& operator= (const Scalar&);
public:
  Scalar (const Scalar& old)
    : unit_ (old.unit_),
      val (old.val)
  { }
  Scalar (const double v, const Unit& u)
    : unit_ (u),
      val (v)
  { }
};

class IM
{
  // Utility.
public:
  static symbol mass_unit (); // [g]
  static symbol storage_unit (); // [g/cm^2]
  static symbol flux_unit ();    // [g/cm^2/h]
  static symbol sink_unit ();    // [g/cm^3/h]
  static symbol soil_unit ();    // [g/cm^3]
  static symbol solute_unit ();  // [g/cm^2/mm]

  // Content.
private:
  const Unit* unit_;
  std::map<symbol, double> content;

  // Accessors.
public:
  const Unit& unit () const;
  double get_value (symbol chem, const Unit&) const;
  void set_value (symbol chem, const Unit&, double value);
  void add_value (symbol chem, const Unit&, double value);
  double get_value_raw (symbol chem) const;
  void set_value_raw (symbol chem, double value);
  void add_value_raw (symbol chem, double value);

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
  void rebase (const Unit& unit);
  
  void operator+= (const IM&);
  IM operator+ (const IM&) const;
  void multiply_assign (const Scalar&, const Unit& result);
  IM multiply (const Scalar&, const Unit& result) const;
  void clear ();

  // Create. 
public:
  static void add_syntax (Frame&, Attribute::category cat, 
			  const symbol dimension);
  explicit IM (const Block&, const char* key);
  explicit IM ();
  explicit IM (const Unit&);
  explicit IM (const Unit&, const IM&);
  ~IM ();
};

#endif // IM_H
