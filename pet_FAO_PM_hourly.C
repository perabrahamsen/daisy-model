// pet_FAO_PM_hourly.C -- FAO ETc using Penman-Monteith with hourly data
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003 KVL.
// Copyright 2018 KU.
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
#include "block_model.h"
#include "mathlib.h"
#include <sstream>
#include <memory>

class PetFAO_PM_hourly : public Pet
{
  // Parameters.
  const bool use_wet;
  const double rb;

public:
  // LogOnly.
  double reference_evapotranspiration_wet;
  double reference_evapotranspiration_dry;
  double potential_evapotranspiration_wet;
  double potential_evapotranspiration_dry;

  // Simulation.
  void tick (const Weather& weather, 
	     const double Rn, const double Rn_ref, const double G,
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
    output_variable (potential_evapotranspiration_dry, log);
    output_variable (potential_evapotranspiration_wet, log);
  }

  double wet () const
  { 
    if (use_wet)
      {
	daisy_assert (std::isfinite (potential_evapotranspiration_wet));
	return potential_evapotranspiration_wet;
      }
    else
      return dry ();
  }

  double dry () const
  {
    daisy_assert (std::isfinite (potential_evapotranspiration_dry));
    return potential_evapotranspiration_dry;
  }

  // Create & Destroy.
  bool check (const Weather& weather, Treelog& msg) const
  {
    TREELOG_MODEL (msg);
    
    bool ok = true;
    if (!weather.has_wind ())
      {
	msg.error ("Wind data required for this model");
	ok = false;
      }
    if (!weather.has_vapor_pressure ())
      msg.warning ("Vapor pressure required for this model");
    return ok;
  }
  void initialize (const Weather& weather)
  { }

  PetFAO_PM_hourly (const BlockModel& al)
    : Pet (al),
      use_wet (al.flag ("use_wet")),
      rb (al.number ("rb"))
  { }
  ~PetFAO_PM_hourly ()
  { }
};

void
PetFAO_PM_hourly::tick (const Weather& weather,
			const double, const double Rn_ref, const double G, 
                        const Vegetation& crops,
                        const Surface& surface, const Geometry& geo, const Soil& soil,
                        const SoilHeat& soil_heat, const SoilWater& soil_water,
                        Treelog& msg)
{
  daisy_assert (std::isfinite (Rn_ref));

  // Weather.
  const double Temp = weather.air_temperature ();
  const double VaporPressure = weather.vapor_pressure ();
  const double U2 = weather.wind ();
  const double AtmPressure = weather.air_pressure ();

  reference_evapotranspiration_dry
    = FAO::RefPenmanMonteithAllen2006 (Rn_ref, G, Temp, VaporPressure, U2,
                                       AtmPressure)
    * 3600;
  daisy_assert (std::isfinite (reference_evapotranspiration_dry));

  potential_evapotranspiration_dry
    = reference_to_potential_dry (crops, surface, 
                                  reference_evapotranspiration_dry);
  daisy_assert (std::isfinite (potential_evapotranspiration_dry));

  reference_evapotranspiration_wet
    = FAO::RefPenmanMonteithWet (Rn_ref, G, Temp, VaporPressure, U2,
                                 AtmPressure, rb)
    * 3600;
  potential_evapotranspiration_wet
    = reference_to_potential_wet (crops, surface,
                                  reference_evapotranspiration_wet);
}

static struct PetFAO_PM_hourlySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PetFAO_PM_hourly (al); }
  PetFAO_PM_hourlySyntax ()
    : DeclareModel (Pet::component, "FAO_PM_hourly",
                    "Potential evopotranspiration using Penman-Monteith.\n\
Modified to work with hourly data.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "FAO-PM", "allen2006recommendation");
    frame.declare ("reference_evapotranspiration_wet",
                   "mm/h", Attribute::LogOnly, 
                   "Reference evapotranspiration for a wet system.");
    frame.declare ("potential_evapotranspiration_wet", 
                   "mm/h", Attribute::LogOnly, 
                   "Potential evapotranspiration for a wet system.");
    frame.declare ("potential_evapotranspiration_dry", 
                   "mm/h", Attribute::LogOnly, 
                   "Potential evapotranspiration for a dry system.");
    frame.declare_boolean ("use_wet", Attribute::Const,
                           "Use wet PM for wet surface.\n\
\n\
This will give significantly higher potential evapotranspiration\n\
especially when feed with daily weather data.");
    frame.set ("use_wet", false);
    frame.declare ("rb", "s/m", Attribute::Const, 
                   "Boundary layer resistance for wet surface.");
    frame.set ("rb", 20.0);
  }
} PetFAO_PM_hourly_syntax;

// pet_FAO_PM_hourly.C ends here.
