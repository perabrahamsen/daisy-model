// average.h --- Find the average of two numbers.
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


#ifndef AVERAGE_H
#define AVERAGE_H

#include "librarian.h"

class Average : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  // Simulation.
public:
  virtual double operator()(double a, double b) const = 0;

  // Create and Destroy.
public:
  static const AttributeList& arithmetic_model ();
protected:
  Average (Block& al);
public:
  ~Average ();
};

#ifdef FORWARD_TEMPLATES
template<>
BuildBase* Librarian<Average>::content;
#endif

static Librarian<Average> Average_init ("average");

#endif // AVERAGE_H
