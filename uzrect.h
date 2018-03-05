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

#include "model_framed.h"
#include "symbol.h"
#include <vector>

class Geometry;
class GeometryRect;
class Soil;
class SoilWater;
class SoilHeat;
class Surface;
class Groundwater;
class Treelog;
class BlockModel;

class UZRect : public ModelFramed
{
  // Content.
public: 
  static const char *const component;
  symbol library_id () const;

  // Simulate.
public:
  virtual bool obey_surface ();
  virtual void tick (const GeometryRect&, 
                     const std::vector<size_t>& drain_cell,
		     const double drain_water_level, // [cm]
		     const Soil&, SoilWater&, 
                     const SoilHeat&, const Surface&, const Groundwater&, 
                     double dt, Treelog&) = 0;

  // Create and Destroy.
public:
  virtual void initialize (const Geometry& geo, const bool has_macropores) = 0;
private:
  UZRect ();
protected:
  explicit UZRect (const BlockModel&);
public:
  virtual void summarize (Treelog& msg) const;
  ~UZRect ();
};

#endif // UZRect_H
