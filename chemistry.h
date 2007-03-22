// chemistry.h --- Tranformation between soil chemicals.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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


#ifndef CHEMISTRY_H
#define CHEMISTRY_H

#include "librarian.h"

class Log;
class Geometry;
class Soil;
class SoilWater;
class SoilChemicals;

class Chemistry : public Model
{
  // Content.
public:
  static const char *const description;
  static const char *const component;
  const symbol name;
  const AttributeList& alist;

  // Simulation.
public:
  virtual void tick (const Geometry& geo,
                     const Soil&, const SoilWater&, 
                     SoilChemicals&, const double dt, Treelog&) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (Block&, const Soil&);
  virtual bool check (const Soil&, Treelog& err) const;
  static void load_syntax (Syntax&, AttributeList&);
protected:
  Chemistry (Block& al);
public:
  ~Chemistry ();
};

static Librarian<Chemistry> Chemistry_init;

#endif // CHEMISTRY_H
