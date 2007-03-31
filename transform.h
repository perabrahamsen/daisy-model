// transform.h --- Transformation between two soil components.
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

#include "model.h"
#include "symbol.h"
#include <vector>

class Soil;
class SoilWater;
class Treelog;
class Syntax;
class AttributeList;
class Block;

class Transform : public Model
{
  // Content.
public:
  static const char *const component;
  const symbol name;
  const AttributeList& alist;

  // Simulation.
public:
  virtual void tick (const Soil&, const SoilWater&, 
                     const std::vector<double>& A,
                     const std::vector<double>& B, 
                     std::vector<double>& S_AB, Treelog&) const = 0;
  virtual bool check (const Soil&, Treelog& err) const;

  // Create and Destroy.
public:
  virtual void initialize (Block&, const Soil&);
  static void load_syntax (Syntax&, AttributeList&);
private:
  Transform ();
  Transform (const Transform&);
protected:
  explicit Transform (Block&);
public:
  ~Transform ();
};

#endif // TRANSFORM_H
