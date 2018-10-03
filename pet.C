// pet.C  -- Potential evopotranspiration
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "pet.h"
#include "frame.h"
#include "block_model.h"
#include "log.h"
#include "vegetation.h"
#include "surface.h"
#include "librarian.h"
#include "mathlib.h"

const char *const Pet::component = "pet";

symbol 
Pet::library_id () const
{
  static const symbol id (component);
  return id;
}

double
Pet::reference_to_potential_dry (const Vegetation& crops, 
                                 const Surface& surface,
                                 double ref)
{
  daisy_assert (std::isfinite (ref));
  const double cover = crops.cover ();
  daisy_assert (std::isfinite (cover));
  daisy_assert (std::isfinite (crops.EpFactorDry ()));
  daisy_assert (std::isfinite (surface.EpFactor ()));
  const double EpFactor = cover * crops.EpFactorDry ()
    + (1.0 - cover) * surface.EpFactor ();
  daisy_assert (std::isfinite (EpFactor));
  return EpFactor * std::max (0.0, ref);
}

double
Pet::reference_to_potential_wet (const Vegetation& crops, 
                                 const Surface& surface,
                                 double ref)
{
  const double cover = crops.cover ();
  const double EpFactor = cover * crops.EpFactorWet ()
    + (1.0 - cover) * surface.EpFactor ();
  return EpFactor * std::max (0.0, ref);
}

void
Pet::output (Log& log) const
{
  output_value (wet (), "wet", log);
  output_value (dry (), "dry", log);
}

bool 
Pet::check (const Weather&, Treelog&) const
{ 
  const bool ok = true;
  return ok;
}

void
Pet::initialize (const Weather&)
{ }

Pet::Pet (const BlockModel& al)
  : ModelFramed (al)
{ }

Pet::~Pet ()
{ }

static struct PetInit : public DeclareComponent 
{
  PetInit ()
    : DeclareComponent (Pet::component, "\
The 'pet' component should calculate the potential evapotranspiration\n\
from meteorological data, as well as the crop and soil state.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare ("wet", "mm/h", Attribute::LogOnly, 
                   "Potential evapotranspiration for a wet system.");
    frame.declare ("dry", "mm/h", Attribute::LogOnly, 
                   "Potential evapotranspiration for a dry system.");
    frame.declare ("reference_evapotranspiration", "mm/h", Attribute::LogOnly, 
                   "Reference evapotranspiration for a dry system.");
  }
} Pet_init;

// pet.C ends here.
