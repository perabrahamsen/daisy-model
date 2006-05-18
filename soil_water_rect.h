// soil_water_rect.h -- Soil water movement in a a rectangular grid.
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


#ifndef SOIL_WATER_RECT_H
#define SOIL_WATER_RECT_H

#include "soil_water.h"

struct GeometryRect;

class SoilWaterRect : public SoilWater
{
  // Content.
  std::vector<double> h_old_;

  // Sink.
public:
  void clear ();
  
  // Queries
public:
  double top_flux () const;
 
  // Simulation.
public:
  void tick (const Soil& soil);
  bool check (size_t n, Treelog& msg) const;
  void output (Log& log) const;
  
  // Communication with surface.
public:
  double MaxExfiltration (const Geometry& geo,
                          const Soil&, double T) const;

  // Creation.
public:
  void initialize (const AttributeList& al,
                   const GeometryRect& geo, const Soil& soil,
                   Treelog& msg);
  static void load_syntax (Syntax&, AttributeList&);
  SoilWaterRect (Block&);
  ~SoilWaterRect ();
};

#endif // SOIL_WATER_RECT_H
