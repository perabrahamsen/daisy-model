// unit.h -- Specify unit for scalar.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#ifndef UNIT_H
#define UNIT_H

#include "model.h"
#include "symbol.h"

class Block;
class Metalib;
class Treelog;

class Unit : public Model
{
  // Identity.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;
private:
  /* const */ symbol base_name_;
public:
  symbol base_name () const
  { return base_name_; }
  
  // Use.
public:
  virtual double to_base (double) const = 0;
  virtual double to_native (double) const = 0;
  virtual bool in_native (double) const = 0;
  virtual bool in_base (double) const = 0;

  // Examine.
public:
  virtual int length () const;
  virtual int mass () const;
  virtual int time () const;
  virtual int electric_current () const;
  virtual int thermodynamic_temperature () const;
  virtual int amount_of_substance () const;
  virtual int luminous_intensity () const;
  // Utilities.
private:
  static bool allow_old (const Metalib& metalib);
  static bool compatible (const Unit& from, const Unit& to);
  static double base_convert (symbol from, symbol to, const double value);
public:
  static bool can_convert (Metalib&, symbol from, symbol to, Treelog&);
  static bool can_convert (Metalib&, symbol from, symbol to);
  static bool can_convert (Metalib&, symbol from, symbol to, double);
  static double convert (Metalib&, symbol from, symbol to, double);

  // Create and Destroy.
protected:
  void find_base_name ();
  Unit (Block& al);
public:
  virtual ~Unit ();
};

#endif // UNIT_H
