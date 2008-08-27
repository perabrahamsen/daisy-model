// unit.h -- Specify unit for scalar.
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

#ifndef UNIT_H
#define UNIT_H

#include "model.h"
#include "symbol.h"

class Block;
class Metalib;
class Treelog;
class Convert;

class Unit : public Model
{
  // Identity.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;
private:
  const symbol base_name_;
public:
  symbol base_name () const
  { return base_name_; }

  // Base units.
public:
  static symbol pressure ();
  static symbol mass_per_volume ();
  static symbol amount_of_substance_per_area_per_time ();
  static symbol energy_per_area_per_time ();

  // Use.
public:
  virtual double to_base (double) const = 0;
  virtual double to_native (double) const = 0;
  virtual bool in_native (double) const = 0;
  virtual bool in_base (double) const = 0;

  // Convert.
public:
  virtual const Convert* create_convertion (const Unit& to) const;

  // Create and Destroy.
protected:
  Unit (Block& al, symbol base);
public:
  virtual ~Unit ();
};

#endif // UNIT_H
