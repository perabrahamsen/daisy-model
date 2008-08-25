// secondary.h --- Specify secondary domain solute parameters.
// 
// Copyright 2008 Per Abrahamsen, Mikkel Mollerup and KU.
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


#ifndef SECONDARY_H
#define SECONDARY_H

#include "model.h"
#include "symbol.h"

class Block;
class AttributeList;

class Secondary : public Model
{
  // Identity.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;
 
  // Content.
public:
  virtual bool none () const = 0;    // True iff all water is in primary domain.
  virtual double h_lim () const = 0; // The value of the 'h_lim' parameter.
  virtual double alpha () const = 0; // The value of the 'alpha' parameter.

  // Create and Destroy.
public:
  static const AttributeList& none_model ();
  Secondary (Block& al);
  virtual ~Secondary ();
};

#endif // SECONDARY_H
