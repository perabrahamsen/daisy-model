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
#include "net_radiation.h"
#include <sstream>
#include <memory>

class PetFAO_PM : public Pet
{
  // Parameters.
  std::unique_ptr<NetRadiation> net_radiation;
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
    output_variable (potential_evapotranspiration_dry, log);
    output_variable (potential_evapotranspiration_wet, log);
    if (Rn >= 0.0)
      output_variable (Rn, log);
    output_variable (G, log);
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
    bool ok = true;
    return ok;
  }
  void initialize (const Weather& weather)
  { }

  PetFAO_PM (const BlockModel& al)
    : Pet (al),
      net_radiation (Librarian::build_stock<NetRadiation> (al.metalib (),
                                                           al.msg (),
                                                           "brunt", objid)),
      use_wet (al.flag ("use_wet")),
      rb (al.number ("rb")),
      Rn (-42.42e42)
  { }
  ~PetFAO_PM ()
  { }
};

void
PetFAO_PM::tick (const Time&, const Weather& weather, const double /* Rn */,
		 const Vegetation& crops,
                 const Surface& surface, const Geometry& geo, const Soil& soil,
                 const SoilHeat& soil_heat, const SoilWater& soil_water,
                 Treelog& msg)
{
  // Weather.
  const double Temp = weather.air_temperature ();
  const double VaporPressure = weather.vapor_pressure ();
  const double U2 = weather.wind ();
  const double Cloudiness = weather.cloudiness ();
  const double AtmPressure = weather.air_pressure ();
  const double Si = weather.global_radiation ();

  // Ground heat flux.
  G = soil_heat.top_flux (geo, soil, soil_water);

  // Reference net radiation.
  const double ref_albedo = 0.23; // Reference crop.
  net_radiation->tick (Cloudiness, Temp, VaporPressure, Si, ref_albedo, msg);
  Rn = net_radiation->net_radiation ();

  reference_evapotranspiration_dry
    = FAO::RefPenmanMonteith (Rn, G, Temp, VaporPressure, U2,
                              AtmPressure)
    * 3600;

  potential_evapotranspiration_dry
    = reference_to_potential_dry (crops, surface, 
                                  reference_evapotranspiration_dry);

  reference_evapotranspiration_wet
    = FAO::RefPenmanMonteithWet (Rn, G, Temp, VaporPressure, U2,
                                 AtmPressure, rb)
    * 3600;
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
This flag is for compatibility with older version of Daisy, which always\n\
used dry PM (and which overestimated the effect of wet PM).");
     frame.set ("use_wet", true);
     frame.declare ("rb", "s/m", Attribute::Const, 
                    "Boundary layer resistance for wet surface.");
     frame.set ("rb", 20.0);
     frame.declare ("Rn", "W/m^2", Attribute::LogOnly, 
                    "Reference net radiation.");
     frame.declare ("G", "W/m^2", Attribute::LogOnly, 
                    "Soil heat flux.");
   }
} PetFAO_PM_syntax;

// pet_FAO_PM.C ends here.
