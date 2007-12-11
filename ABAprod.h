// ABAprod.h -- Production of ABA in soil.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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
// GNU Lesser Public License for more details.// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef ABAPROD_H
#define ABAPROD_H

#include "model.h"
#include "symbol.h"
#include <vector>

class Treelog;
class Block;
class Log;
class AttributeList;
class Geometry;
class SoilWater;

class ABAProd : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;

  // Simulation.
public:
  virtual void production (const Geometry&, const SoilWater&,
			   const std::vector<double>& S /* [cm^3/cm^3] */,
			   const std::vector<double>& l /* [cm/cm^3] */,
			   std::vector<double>& ABA /* [g/cm/h] */,
			   Treelog& msg) const = 0; 
  virtual void output (Log& log) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (Treelog&) = 0;
  virtual bool check (Treelog&) const = 0;
protected:
  ABAProd (Block&);

public:
  static const AttributeList& default_model ();
  ~ABAProd ();
};

#endif // ABAPROD_H
