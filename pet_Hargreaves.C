// pet_Hargreaves.C -- Potential evopotransp. using Samani and Hargreaves.
// 
// Copyright 2003 Per Abrahamsen KVL.
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
#include "fao.h"
#include "log.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include "treelog.h"

struct PetHargreaves : public Pet
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
      if (!weather.has_min_max_temperature ())
        throw "Min/max temperature required by Hargreaves";

      // K_hs is 0.00023 in reference, but later corrected to 0.0023.
      const double K_hs = 0.0023; 
      const double T_avg = weather.daily_air_temperature ();
      const double T_diff = std::max (weather.daily_max_air_temperature () 
                                      - weather.daily_min_air_temperature (),
                                      0.0);
      const double latent_heat_of_vaporation = 
        FAO::LatentHeatVaporization (T_avg); // [J/kg] 
      static const double s_per_h = 60.0 * 60.0; // [W] -> [J/h]
      static const double W_per_m2_to_mm_per_h = s_per_h 
        / latent_heat_of_vaporation;
      double Ra =  weather.extraterrestrial_radiation ()
        * W_per_m2_to_mm_per_h; // [mm/h]

      reference_evapotranspiration 
        = K_hs * (T_avg + 17.8) * sqrt (T_diff) * Ra;
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
  bool check (const Weather& weather, Treelog& msg) const
  { 
    bool ok = true;
    if (!weather.has_min_max_temperature ())
      {
        ok = false;
        TREELOG_MODEL (msg);
        msg.error ("Min/max temperature required");
      }
    return ok;
  }

  PetHargreaves (const BlockModel& al)
    : Pet (al)
    { }
};

static struct PetHargreavesSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
    { return new PetHargreaves (al); }
  PetHargreavesSyntax ()
    : DeclareModel (Pet::component, "Hargreaves", "\
Potential evopotranspiration based on temperature.")
  { }
  void load_frame (Frame& frame) const
  { frame.set_strings ("cite", "hargreaves1985reference"); }
} PetHargreaves_syntax;

// pet_Hargreaves.C ends here.
