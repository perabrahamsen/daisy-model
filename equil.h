// equil.h --- Find equilibrium between two soil chemicals.
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


#ifndef EQUILIBRIUM_H
#define EQUILIBRIUM_H

#include "librarian.h"

class Soil;
class SoilWater;

class Equilibrium
{
  // Content.
public:
  static const char *const description;
  const symbol name;

  // Simulation.
public:
  virtual void find (const Soil&, const SoilWater&, unsigned int i,
		     double has_A, double has_B, 
		     double& want_A, double& want_B) const = 0;
  virtual bool check (const Soil&, Treelog& err) const;

  // Create and Destroy.
public:
  virtual void initialize (const Soil&);
  static void load_syntax (Syntax&, AttributeList&);
protected:
  Equilibrium (const AttributeList& al);
public:
  virtual ~Equilibrium ();
};

#if !defined (__BORLANDC__)
EMPTY_TEMPLATE
Librarian<Equilibrium>::Content* Librarian<Equilibrium>::content;
#endif

static Librarian<Equilibrium> Equilibrium_init ("equilibrium");

#endif // EQUILIBRIUM_H
