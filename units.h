// units.h -- Unit conversions.
// 
// Copyright 2007, 2008 Per Abrahamsen and KVL.
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

#ifndef UNITS_H
#define UNITS_H

#include "memutils.h"
#include "symbol.h"
#include <boost/noncopyable.hpp>

class Metalib;
class Treelog;
class Unit;
class Convert;
class Frame;

class Units : private boost::noncopyable
{
  // Symbols.
public:
  static symbol error_symbol ();
  static symbol h ();
  static symbol mm ();
  static symbol per_mm ();
  static symbol mm_per_h ();
  static symbol cm ();
  static symbol cm_per_h ();
  static symbol cm2 ();
  static symbol cm3 ();
  static symbol per_h ();
  static symbol ppm ();
  static symbol dry_soil_fraction ();
  static symbol dgC ();
  
  // Special conversion rules.
private:
  static struct special_convert_type
  {
    const symbol from;
    const symbol to;
    const double factor;
  } special_convert[];
  static const size_t special_convert_size;
  static double base_convert (symbol from, symbol to, double value);

  // Utilities.
public:
  static bool compatible (const Unit& from, const Unit& to);
  static double unit_convert (const Unit& from, const Unit& to, double value);
  static double multiply (const Unit&, const Unit&, double, const Unit& result);
  static symbol multiply (symbol, symbol);

  // Content.
private:
  typedef auto_map<symbol, const Unit*> unit_map;
  unit_map units;
  typedef auto_map<symbol, const Convert*> convert_map;
  mutable convert_map conversions;
  const bool allow_old_;

  // Special units.
public:
  const Unit& unknown () const;
  bool is_known (const Unit&) const;
  const Unit& error () const;
  bool is_error (const Unit&) const;

  // Interface.
private:
  bool allow_old () const;
public:
  bool has_unit (symbol name) const;
  const Unit& get_unit (symbol name) const;
  bool can_convert (symbol from, symbol to, Treelog&) const;
  bool can_convert (symbol from, symbol to) const;
  bool can_convert (symbol from, symbol to, double) const;
  double convert (symbol from, symbol to, double) const;
private:
  static const Convert* create_convertion (const Unit& from, const Unit& to);
public:
  const Convert& get_convertion (symbol from, symbol to) const;

  // Create and destroy.
public:
  void add_unit (Metalib&, const symbol name);
  static void load_syntax (Frame&);
  Units (Metalib&);
  ~Units ();
};

#endif // UNITS_H
