// uzrect.h --- 2D water movement in a rectangular grid.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#ifndef UZRect_H
#define UZRect_H

#include "librarian.h"
#include "symbol.h"

struct GeometryRect;
struct Soil;
struct SoilWater;
struct SoilHeat;
struct Surface;
struct Groundwater;
struct Treelog;

class UZRect
{
  // Content.
public: 
  const symbol name;
  static const char *const description;

  // Simulate.
public:
  virtual void tick (const GeometryRect&, const Soil&, SoilWater&, 
                     const SoilHeat&, const Surface&, const Groundwater&, 
                     double dt, Treelog&) = 0;

  // Create and Destroy.
public:
  virtual void has_macropores (bool) = 0;
  static const AttributeList& default_model ();
  static const AttributeList& reserve_model ();
private:
  UZRect ();
  UZRect (const UZRect&);
  UZRect& operator= (const UZRect&);
protected:
  explicit UZRect (Block&);
public:
  virtual ~UZRect ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<UZRect>::Content* Librarian<UZRect>::content;
#endif

static Librarian<UZRect> UZRect_init ("uzrect");

#endif // UZRect_H
