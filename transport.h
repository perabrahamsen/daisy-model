// transport.h
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


#ifndef TRANSPORT_H
#define TRANSPORT_H

#include "librarian.h"

class Soil;
class SoilWater;
class Adsorption;

class Transport
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  // Simulation.
public:
  virtual void tick (Treelog&, const Soil&, const SoilWater&, 
		     const Adsorption&, 
		     double diffusion_coefficient,
		     std::vector<double>& M, 
		     std::vector<double>& C,
		     const std::vector<double>& S,
		     std::vector<double>& J) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
protected:
  Transport (const AttributeList&);
public:
  virtual ~Transport ();
};

#ifdef FORWARD_TEMPLATES
EMPTY_TEMPLATE
Librarian<Transport>::Content* Librarian<Transport>::content;
#endif

static Librarian<Transport> Transport_init ("transport");

#endif // TRANSPORT_H
