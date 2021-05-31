// pet_weather.C -- Potential evopotranspiration using weather data.
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
#include "weather.h"
#include "log.h"
#include "librarian.h"
#include "frame.h"

struct PetWeather : public Pet
{
  // State.
  double reference_evapotranspiration;
  double potential_evapotranspiration_dry;
  double potential_evapotranspiration_wet;

  // Simulation.
  void tick (const Weather& weather,
	     const double, const double, const double,
	     const Vegetation& crops,
	     const Surface& surface, const Geometry&,
             const Soil&, const SoilHeat&,
	     const SoilWater&, Treelog&)
  {
    reference_evapotranspiration = weather.reference_evapotranspiration ();
    potential_evapotranspiration_dry 
      = reference_to_potential_dry (crops, surface, 
                                    reference_evapotranspiration);
    potential_evapotranspiration_wet 
      = reference_to_potential_wet (crops, surface, 
                                    reference_evapotranspiration);
  }

  void output (Log& log) const
  {
    Pet::output (log);
    output_value (reference_evapotranspiration, 
		  "reference_evapotranspiration", log);
  }

  double dry () const
  { return potential_evapotranspiration_dry; }
  double wet () const
  { return potential_evapotranspiration_wet; }

  // Create.
  PetWeather (const BlockModel& al)
    : Pet (al)
  { }
};

static struct PetWeatherSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PetWeather (al); }
  PetWeatherSyntax ()
    : DeclareModel (Pet::component, "weather", 
                    "Potential evopotranspiration using weather data.")
  { }
  void load_frame (Frame&) const
  { }
} PetWeather_syntax;

// pet_weather.C ends here.
