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
#include "block_model.h"
#include "mathlib.h"
#include <sstream>
#include <memory>

class PetFAO_PM : public Pet
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
  double Rn;
  double G;

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
      return potential_evapotranspiration_wet; 
    else
      return dry ();
  }

  double dry () const
  { return potential_evapotranspiration_dry; }

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
    if (!weather.has_daily_vapor_pressure ())
      msg.warning ("Vapor pressure required for this model (not RelHum)");
    return ok;
  }
  void initialize (const Weather& weather)
  { }

  PetFAO_PM (const BlockModel& al)
    : Pet (al),
      use_wet (al.flag ("use_wet")),
      rb (al.number ("rb"))
  { }
  ~PetFAO_PM ()
  { }
};

void
PetFAO_PM::tick (const Weather& weather,
		 const double, const double Rn_ref, const double G,
		 const Vegetation& crops,
                 const Surface& surface, const Geometry& geo, const Soil& soil,
                 const SoilHeat& soil_heat, const SoilWater& soil_water,
                 Treelog& msg)
{
  TREELOG_MODEL (msg);

  // Weather.
  const double Temp = (weather.daily_max_air_temperature ()
		       + weather.daily_min_air_temperature ()) * 0.5;
  const double VaporPressure = weather.daily_vapor_pressure ();
  const double U2 = weather.daily_wind ();
  const double AtmPressure = weather.daily_air_pressure ();

  daisy_assert (std::isfinite (Rn_ref));
  daisy_assert (std::isfinite (G));
  daisy_assert (std::isfinite (Temp));
  if (!(std::isfinite (VaporPressure)))
    {
      msg.error ("No known vapour pressure, using ET0 = 0");
      potential_evapotranspiration_dry = reference_evapotranspiration_dry = 0.0;
      return;
    }
  daisy_assert (std::isfinite (U2));
  daisy_assert (std::isfinite (AtmPressure));
  reference_evapotranspiration_dry
    = FAO::RefPenmanMonteith (Rn_ref, G, Temp, VaporPressure, U2,
                              AtmPressure)
    * 3600;
  if (!std::isfinite (reference_evapotranspiration_dry))
    {
      std::ostringstream tmp;
      tmp
	<< "; Rn_ref = " << Rn_ref 
	<< "; G = " << G
	<< "; Temp = " << Temp
	<< "; VaporPressure = " << VaporPressure
	<< "; U2 = " << U2 
	<< "; reference_evapotranspiration_dry = " << reference_evapotranspiration_dry;
      msg.error (tmp.str ());
    }
  daisy_assert (std::isfinite (reference_evapotranspiration_dry));
  potential_evapotranspiration_dry
    = reference_to_potential_dry (crops, surface, 
                                  reference_evapotranspiration_dry);

  daisy_assert (std::isfinite (rb));
  reference_evapotranspiration_wet
    = FAO::RefPenmanMonteithWet (Rn_ref, G, Temp, VaporPressure, U2,
                                 AtmPressure, rb)
    * 3600;
  daisy_assert (std::isfinite (reference_evapotranspiration_wet));
  potential_evapotranspiration_wet
     = reference_to_potential_wet (crops, surface,
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
     frame.set_strings ("cite", "FAO-PM");
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
} PetFAO_PM_syntax;

// pet_FAO_PM.C ends here.
