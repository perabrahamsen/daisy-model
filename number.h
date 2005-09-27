// number.h --- Numbers in Daisy.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#ifndef NUMBER_H
#define NUMBER_H

#include "librarian.h"
#include <vector>

class Scope;

class Number
{
  // Content.
public:
  static const char *const description;
  const symbol name;

  // Simulation.
public:
  virtual bool missing (const Scope& scope) const = 0;
  virtual double value (const Scope&) const = 0; 
  virtual const std::string& dimension (const Scope&) const = 0;


  // Create and Destroy.
public:
  Number (const AttributeList&);
  virtual ~Number ();
};

#ifdef FORWARD_TEMPLATES
EMPTY_TEMPLATE
Librarian<Number>::Content* Librarian<Number>::content;
#endif

static Librarian<Number> Number_init ("number");

#endif // NUMBER_H
