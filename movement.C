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
#include "block_model.h"
#include "librarian.h"
#include "tertiary.h"
#include "log.h"
#include "treelog.h"
#include "assertion.h"
#include "frame.h"
#include "mathlib.h"
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
  if (water_failure_level < static_cast<int> (level))
    water_failure_level = level;
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
  if (solute_failure_level < static_cast<int> (level))
    solute_failure_level = level;
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
        Treelog::Open nest (msg, "matrix_water", i, objid);
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
        Treelog::Open nest (msg, "matrix_solute", i, objid);
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
Movement::tick_source (const Soil& soil, const SoilHeat& soil_heat,
                       SoilWater& soil_water, Treelog& msg)
{
  tertiary->tick_source (geometry (), soil, soil_heat, soil_water, msg); 
}

void 
Movement::tick_tertiary (const Units& units,
                         const Geometry& geo, const Soil& soil, 
                         const SoilHeat& soil_heat, const double dt, 
                         SoilWater& soil_water, Surface& surface, Treelog& msg)
{ 
  tertiary->tick (units, geo, soil, soil_heat, dt, soil_water, surface, msg); 
}

void
Movement::clear ()
{
  water_failure_level = -1;
  solute_failure_level = -1;
}

double 
Movement::suggest_dt (const double weather_dt, const double max_pond) const
{ return tertiary->suggest_dt (weather_dt, max_pond); }

void
Movement::remove_solute (const symbol chem)
{ tertiary->remove_solute (chem); }

double 
Movement::total_solute (const Geometry& geo, const symbol chem) const //[g/m^2]
{ return tertiary->total_solute (geo, chem); }

void 
Movement::output_base (Log& log) const
{ 
  output_variable (water_failure_level, log);
  output_variable (solute_failure_level, log);
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
                      const Soil& soil, SoilWater& soil_water, 
                      const Groundwater& groundwater,
                      const Time& time, const Scope& scope, Treelog& msg)
{
  bool ok = true;
  
  // Tertiary transport depends on groundwater, soil, and soil_water,
  if (!tertiary->initialize (units, geometry (),
                             soil, soil_water, scope, groundwater, msg))
    ok = false;
  
  initialize_derived (time, scope, soil, groundwater, tertiary->has_macropores (), msg);

  return ok;
}

Movement::Movement (const BlockModel& al)
  : ModelDerived (al.type_name ()),
    water_failure_level (-1),
    solute_failure_level (-1),
    tertiary (Librarian::build_item<Tertiary> (al, "Tertiary"))
{ }

Movement::~Movement ()
{ }

static struct MovementInit : public DeclareComponent 
{
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("Tertiary", Tertiary::component, 
                          Attribute::State, Attribute::Singleton, "\
Tertiary (that is, non-matrix) transport method.");
    frame.declare_integer ("water_failure_level", Attribute::LogOnly, "\
The number of the last water transport model to fail.\n\
It is -1 if the first model succeded, and 0 if the first model failed but\n\
the second succeded.");
    frame.declare_integer ("solute_failure_level", Attribute::LogOnly, "\
The number of the last solute transport model to fail.\n                \
It is -1 if the first model succeded, and 0 if the first model failed but\n\
the second succeded.");
  }
  MovementInit ()
    : DeclareComponent (Movement::component, "\
This component handles the movement in the soil.")
  { }
} Movement_init;

// movement.C ends here.
