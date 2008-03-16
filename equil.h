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

#include "model.h"
#include "symbol.h"

class Block;
class Syntax;
class AttributeList;
class Treelog;
class Scope;

class Equilibrium : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;
  const symbol name;
  const AttributeList& alist;
  
  // Simulation.
public:
  virtual void find (const Scope&,
		     double has_A, double has_B, 
		     double& want_A, double& want_B, Treelog&) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (Treelog&) = 0;
  virtual bool check (const Scope&, Treelog&) const = 0;
  static void load_syntax (Syntax&, AttributeList&);
private:
  Equilibrium ();
  Equilibrium (const Equilibrium&);
protected:
  explicit Equilibrium (Block& al);
public:
  ~Equilibrium ();
};

#endif // EQUILIBRIUM_H
