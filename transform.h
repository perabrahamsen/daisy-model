// transform.h --- Transformation between two soil chemicals.
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


#ifndef TRANSFORM_H
#define TRANSFORM_H

#include "librarian.h"

class Soil;
class SoilWater;
class SoilChemicals;

class Transform
{
  // Content.
public:
  static const char *const description;
  const string name;
  const AttributeList& alist;

  // Simulation.
public:
  virtual void tick (const Soil&, const SoilWater&, SoilChemicals&, 
		     Treelog&) = 0;
  virtual void output (Log&) const;
  virtual bool check (const Soil&, Treelog& err) const;

  // Create and Destroy.
public:
  virtual void initialize (const Soil&);
  static void load_syntax (Syntax&, AttributeList&);
protected:
  Transform (const AttributeList& al);
public:
  virtual ~Transform ();
};

#if !defined (__BORLANDC__)
EMPTY_TEMPLATE
Librarian<Transform>::Content* Librarian<Transform>::content;
#endif

static Librarian<Transform> Transform_init ("transform");

#endif // TRANSFORM_H
