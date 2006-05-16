// movement_rect.C --- Movement in a rectangular 2D grid.
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

#include "movement.h"
#include "geometry_rect.h"
#include "soil_water_rect.h"
#include "soil_heat_rect.h"
#include "log.h"
#include "submodeler.h"

struct MovementRect : public Movement
{
  // Water.
  std::auto_ptr<GeometryRect> geo;
  std::auto_ptr<SoilWaterRect> water;
  std::auto_ptr<SoilHeatRect> heat;

  // Simulation.
  Geometry& geometry () const
  { return *geo; }
  SoilWater& soil_water () const
  { return *water; }
  SoilHeat& soil_heat () const
  { return *heat; }

  void macro_tick (const Soil&, Surface&, Treelog&)
  { }

  void tick (const Soil&, Surface&, const Time&,
             const Weather&, Treelog&) 
  {
#if 0
    heat->tick (time, *geo, soil, *water, surface, weather);
    water->tick (*geo, soil, *heat, surface, msg);
#endif
  }

  void solute (const Soil&, const double, Solute&, Treelog&)
  { }

  void element (const Soil&, Element&, Adsorption&, double, Treelog&)
  { }

  void ridge (Surface&, const Soil&, const AttributeList&)
  { throw "Can't make ridges on a rectangular grid"; }

  void output (Log& log) const
  { 
    output_submodule (*water, "Water", log);
    output_submodule (*heat, "Heat", log);
  }

  // Create.
  bool check (Treelog& err) const;
  bool volatile_bottom () const
  { return false; }
  void initialize_soil (Soil&, Treelog&) const
  { }

  void initialize (const AttributeList& alist,
                   const Soil& soil, const Time&, const Weather&,
                   Treelog& msg)
  {
    heat->initialize (alist.alist ("Heat"), *geo, msg);
    water->initialize (alist.alist ("Water"), *geo, soil, msg);
  }
  MovementRect (Block& al)
    : Movement (al),
      geo (submodel<GeometryRect> (al, "Geometry")),
      water (submodel<SoilWaterRect> (al, "Water")),
      heat (submodel<SoilHeatRect> (al, "Heat"))
  { }
};

bool
MovementRect::check (Treelog& err) const
{
  const size_t n = geo->cell_size ();

  bool ok = true;
  {
    Treelog::Open nest (err, "Water");
    if (!water->check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "Heat");
    if (!heat->check (n, err))
      ok = false;
  }
  return ok;
}

static struct MovementRectSyntax
{
  static Movement& make (Block& al)
  { return *new MovementRect (al); }

  MovementRectSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Two dimensional movement in a rectangular grid.");
    syntax.add_submodule ("Geometry", alist, Syntax::State,
                          "Discretization of the soil.",
                          GeometryRect::load_syntax);
    syntax.add_submodule ("Water", alist, Syntax::State,
                          "Soil water content and transportation.",
                          SoilWaterRect::load_syntax);
    syntax.add_submodule ("Heat", alist, Syntax::State,
                          "Soil heat and flux.",
                          SoilHeatRect::load_syntax);
 
    Librarian<Movement>::add_type ("rectangle", alist, syntax, &make);
  }
} MovementRect_syntax;
