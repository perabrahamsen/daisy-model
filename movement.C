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
#include "log.h"
#include "treelog.h"
#include "assertion.h"
#include <sstream>

const char *const Movement::component = "movement";

symbol
Movement::library_id () const
{
  static const symbol id (component);
  return id;
}

void 
Movement::water_attempt (const size_t level)
{
  while (water_total.size () <= level)
    water_total.push_back (0);
  water_total[level]++;
}

void 
Movement::water_failure (const size_t level)
{
  while (water_fail.size () <= level)
    water_fail.push_back (0);
  water_fail[level]++;
}

void 
Movement::solute_attempt (const size_t level)
{
  while (solute_total.size () <= level)
    solute_total.push_back (0);
  solute_total[level]++;
}

void 
Movement::solute_failure (const size_t level)
{
  while (solute_fail.size () <= level)
    solute_fail.push_back (0);
  solute_fail[level]++;
}

void 
Movement::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  bool found = false;
  for (size_t i = 0; i < water_fail.size (); i++)
    if (water_fail[i] > 0)
      {
        found = true;
        Treelog::Open nest (msg, "matrix_water", i, name);
        daisy_assert (water_total[i] > 0);
        std::ostringstream tmp;
        tmp << "Matrix water transport model " << i << " failed " 
            << water_fail[i] << " times out of " << water_total[i] << ", or "
            << (100.0 * water_fail[i] / (water_total[i] + 0.0)) << "%";
        msg.warning (tmp.str ());
      }
  for (size_t i = 0; i < solute_fail.size (); i++)
    if (solute_fail[i] > 0)
      {
        found = true;
        Treelog::Open nest (msg, "matrix_solute", i, name);
        daisy_assert (solute_total[i] > 0);
        std::ostringstream tmp;
        tmp << "Matrix solute transport model " << i << " failed " 
            << solute_fail[i] << " times out of " << solute_total[i] << ", or "
            << (100.0 * solute_fail[i] / (solute_total[i] + 0.0)) << "%";
        msg.warning (tmp.str ());
      }
  if (found)
    msg.message ("See 'daisy.log' for details.");
}

void 
Movement::deactivate_tertiary (const int steps)
{ tertiary->deactivate (steps); }

void 
Movement::tick_tertiary (const Units& units,
                         const Geometry& geo, const Soil& soil, 
                         const SoilHeat& soil_heat, const double dt, 
                         SoilWater& soil_water, Surface& surface, Treelog& msg)
{ tertiary->tick (units, geo, soil, soil_heat, dt, soil_water, surface, msg); }

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
  if (!tertiary->initialize (units, geometry (),
                             soil, scope, groundwater, msg))
    ok = false;
  
  initialize_derived (soil, groundwater, tertiary->has_macropores (), msg);

  return ok;
}

void
Movement::load_base (Syntax& syntax, AttributeList&)
{
  syntax.add_object ("Tertiary", Tertiary::component, 
                     Value::OptionalState, Value::Singleton, "\
Tertiary (that is, non-matrix) transport method.");
}

Movement::Movement (Block& al)
  : ModelLogable (al.name ("type")),
    tertiary (Librarian::build_item<Tertiary> (al, "Tertiary"))
{ }

Movement::~Movement ()
{ }

static Librarian Movement_init (Movement::component, "\
This component handles the movement in the soil.");

