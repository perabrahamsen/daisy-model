// movement.C
// 
// Copyright 2006 Per Abrahamsen andKVL.
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

#define BUILD_DLL

#include "movement.h"
#include "block.h"
#include "librarian.h"
#include "tertiary.h"
#include "groundwater.h"
#include "log.h"

const char *const Movement::component = "movement";

symbol
Movement::library_id () const
{
  static const symbol id (component);
  return id;
}

void 
Movement::tick_tertiary (const Units& units,
                         const Geometry& geo, const Soil& soil, 
                         const SoilHeat& soil_heat, const double dt, 
                         SoilWater& soil_water, Surface& surface, Treelog& msg)
{  tertiary->tick (units, geo, soil, soil_heat, dt, soil_water, surface, msg); }

void 
Movement::output (Log& log) const
{ 
  output_derived (tertiary, "Tertiary", log);
}

bool 
Movement::check (Treelog& msg) const
{
  bool ok = true;

  {
    Treelog::Open nest (msg, "Tertiary");
    if (!tertiary->check (geometry (), msg))
      ok = false;
  }

  if (!check_derived (msg))
    ok = false;

  return ok;
}

bool
Movement::initialize (const Units& units,
                      const Soil& soil, const Groundwater& groundwater,
                      const Scope& scope, Treelog& msg)
{
  bool ok = true;
  
  // Tertiary transport depends on groundwater and soil.
  const double pipe_position = groundwater.is_pipe ()
    ? groundwater.pipe_height ()
    : 42.42e42;
  if (!tertiary->initialize (units, geometry (),
                             soil, scope, pipe_position, msg))
    ok = false;
  
  initialize_derived (soil, groundwater, tertiary->has_macropores (), msg);

  return ok;
}

void
Movement::load_base (Syntax& syntax, AttributeList&)
{
  syntax.add_object ("Tertiary", Tertiary::component, 
                     Syntax::OptionalState, Syntax::Singleton, "\
Tertiary (that is, non-matrix) transport method.");
}

Movement::Movement (Block& al)
  : ModelLogable (al.identifier ("type")),
    tertiary (Librarian::build_item<Tertiary> (al, "Tertiary"))
{ }

Movement::~Movement ()
{ }

static Librarian Movement_init (Movement::component, "\
This component handles the movement in the soil.");

