// mactrans.h --- Transportation of solute in macropores.
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


#ifndef MACTRANS_H
#define MACTRANS_H

#include "librarian.h"
#include <vector>

class Geometry1D;
class SoilWater1D;
class Log;

class Mactrans
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (const Geometry1D& geo, const SoilWater1D&,
		     const std::vector<double>& M,
		     const std::vector<double>& C,
		     std::vector<double>& S,
		     std::vector<double>& S_p,
		     std::vector<double>& J_p, Treelog&) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
protected:
  Mactrans (Block& al);
public:
  virtual ~Mactrans ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Mactrans>::Content* Librarian<Mactrans>::content;
#endif

static Librarian<Mactrans> Mactrans_init ("mactrans");

#endif // MACTRANS_H
