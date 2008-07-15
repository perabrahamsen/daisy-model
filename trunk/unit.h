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

  // Use:
public:
  virtual double to_base (double) const = 0;
  virtual double from_base (double) const = 0;
  virtual bool in_domain (double) const = 0;
  virtual bool in_range (double) const = 0;

  // Utilities.
public:
  static bool can_convert (Metalib&, symbol from, symbol to, Treelog&);
  static bool can_convert (Metalib&, symbol from, symbol to);
  static bool can_convert (Metalib&, symbol from, symbol to, double);
  static double convert (Metalib&, symbol from, symbol to, double);

  // Create and Destroy.
protected:
  Unit (Block& al);
public:
  virtual ~Unit ();
};

#endif // UNIT_H
