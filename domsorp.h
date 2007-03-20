// domsorp.h --- Sorption and desorption of dom to som.
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


#ifndef DOMSORP_H
#define DOMSORP_H

#include "librarian.h"

class Soil;
class SoilWater;
class SOM;
class DOM;
class Domsorp : public Model
{
  // Content.
public:
  static const char *const description;
  const symbol name;
  const AttributeList& alist;

  // Simulation.
public:
  virtual void tick (const Soil&, const SoilWater&, 
                     const std::vector<DOM*>&, const std::vector<SOM*>&,
                     Treelog&) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (const Soil&, Treelog& err);
  virtual bool check (const Soil&, size_t dom_size, size_t som_size, 
                      Treelog& err) const = 0;
  static void load_syntax (Syntax&, AttributeList&);
protected:
  Domsorp (Block& al);
public:
  ~Domsorp ();
};

#ifdef FORWARD_TEMPLATES
template<>
BuildBase* Librarian<Domsorp>::content;
#endif

static Librarian<Domsorp> Domsorp_init ("domsorp");

#endif // DOMSORP_H
