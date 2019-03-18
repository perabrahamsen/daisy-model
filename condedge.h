// condedge.h --- Find the hydraulic conductivity between two cells.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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


#ifndef CONDEDGE_H
#define CONDEDGE_H

#include "model.h"
#include "symbol.h"

#include <memory>

class Soil;
class Geometry;

class Condedge : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual double average (const Soil& soil, const Geometry& geo, size_t edge,
                          double K1, double h1, double h1_ice, double h1_old, double T1,
                          double K2, double h2, double h2_ice, double h2_old, double T2)
    const = 0;
  
  // Create and Destroy.
public:
  static std::unique_ptr<const Condedge> build_arithmetic ();
  static std::unique_ptr<const Condedge> build_geometric ();
protected:
  Condedge ();
public:
  ~Condedge ();
};

#endif // CONDEDGE_H
