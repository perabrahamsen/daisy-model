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


#include "pet.h"
#include "syntax.h"
#include "fao.h"
#include "weather.h"
#include "soil.h"
#include "surface.h"
#include "soil_heat.h"
#include "fao.h"
#include "vegetation.h"
#include "log.h"
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
  }

  double wet () const
  { return potential_evapotranspiration_wet; }

  double dry () const
  { return potential_evapotranspiration_dry; }

  // Create & Destroy.
  PetFAO_PM (Block& al)
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
  const double Temp = weather.hourly_air_temperature ();
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
    = FAO::RefPenmanMonteith (Rn, G, Temp, VaporPressure, U2,
                              AtmPressure)
    * 3600;
  potential_evapotranspiration_wet
    = reference_to_potential (crops, surface,
                              reference_evapotranspiration_wet);
}

static struct PetFAO_PMSyntax
{
  static Model& make (Block& al)
  { return *new PetFAO_PM (al); }
  PetFAO_PMSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description",
	       "Potential evopotranspiration using Penman-Monteith.");
    Pet::load_syntax (syntax, alist);
    Librarian<Pet>::add_type ("FAO_PM", alist, syntax, &make);
  }
} PetFAO_PM_syntax;
