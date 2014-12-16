// soilph.h --- pH of the soil.
// 
// Copyright 2013 KU.
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

#ifndef SOILPH_H
#define SOILPH_H

#include "model_derived.h"

class BlockModel;
class Geometry;
class Time;
class Treelog;

// The 'soilph' component.

class SoilpH : public ModelDerived
{
  // Content.
public:
  const symbol name;
  symbol library_id () const;
  static const char *const component;

  // Use.
public:
  virtual double pH (size_t c) const = 0;
  virtual void tick (const Geometry&, const Time&, Treelog&) = 0;
  void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual void initialize (const Geometry&, const Time&, Treelog&) = 0;
protected:
  explicit SoilpH (const BlockModel&);
public:
  virtual ~SoilpH ();
};

#endif // SOILPH_H
