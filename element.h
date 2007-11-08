// element.h --- An element of a compound.
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


#ifndef ELEMENT_H
#define ELEMENT_H

#include "model.h"
#include "symbol.h"

class Block;
class AttributeList;

class Element : public Model
{
  // Identity.
public:
  const symbol name;
  static const char *const component;
  
  // Content.
public:
  virtual double weight () const = 0; // Relative weight [?]
  virtual symbol dimension () const = 0; // Dimension of relative weight.

  // Create and Destroy.
public:
  Element (Block& al);
  ~Element ();
};

#endif // ELEMENT_H
