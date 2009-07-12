// pet_FAO_PM.C -- FAO potential evopotranspiration using Penman-Monteith.
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003 KVL.
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
#include "fao.h"
#include "weather.h"
#include "soil.h"
#include "surface.h"
#include "soil_heat.h"
#include "fao.h"
#include "vegetation.h"
#include "log.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>
#include <memory>

class PetFAO_PM : public Pet
{
public:
  // State.
  double reference_evapotranspiration_wet;
  double reference_evapotranspiration_dry;
  double potential_evapotranspiration_wet;
  double potential_evapotranspiration_dry;

  // Simulation.
  void tick (const Time&, const Weather& weather, const double Rn,
	     const Vegetation& crops,
	     const Surface& surface, const Geometry& geo,
             const Soil& soil,
	     const SoilHeat& soil_heat, const SoilWater& soil_water, Treelog&);

  void output (Log& log) const
  {
    Pet::output (log);
    output_value (reference_evapotranspiration_dry,
                  "reference_evapotranspiration", log);
    output_variable (reference_evapotranspiration_wet, log);
  }

  double wet () const
  { 
    // BUG!!!!! should be WET.
    // But FAO::RefPenmanMonteithWet might be wrong!
    return dry ();
    // return potential_evapotranspiration_wet; 
}

  double dry () const
  { return potential_evapotranspiration_dry; }

  // Create & Destroy.
  PetFAO_PM (const BlockModel& al)
    : Pet (al)
  { }
  ~PetFAO_PM ()
  { }
};

void
PetFAO_PM::tick (const Time&, const Weather& weather, const double Rn,
		 const Vegetation& crops,
                 const Surface& surface, const Geometry& geo, const Soil& soil,
                 const SoilHeat& soil_heat, const SoilWater& soil_water,
                 Treelog&)
{
  // Weather.
  const double Temp = weather.air_temperature ();
  const double VaporPressure = weather.vapor_pressure ();
  const double U2 = weather.wind ();
  const double elevation = weather.elevation ();
  const double AtmPressure = FAO::AtmosphericPressure (elevation);

  // Ground heat flux.
  const double G = soil_heat.top_flux (geo, soil, soil_water);


  reference_evapotranspiration_dry
    = FAO::RefPenmanMonteith (Rn, G, Temp, VaporPressure, U2,
                              AtmPressure)
    * 3600;

  potential_evapotranspiration_dry
    = reference_to_potential (crops, surface, 
                              reference_evapotranspiration_dry);

  reference_evapotranspiration_wet
    = FAO::RefPenmanMonteithWet (Rn, G, Temp, VaporPressure, U2,
                              AtmPressure)
    * 3600;
  potential_evapotranspiration_wet
    = reference_to_potential (crops, surface,
                              reference_evapotranspiration_wet);
}

static struct PetFAO_PMSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PetFAO_PM (al); }
  PetFAO_PMSyntax ()
    : DeclareModel (Pet::component, "FAO_PM",
	       "Potential evopotranspiration using Penman-Monteith.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("reference_evapotranspiration_wet", "mm/h", Attribute::LogOnly, 
                "Reference evapotranspiration for a dry system.");
  }
} PetFAO_PM_syntax;
