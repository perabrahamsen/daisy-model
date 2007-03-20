// pet_PM.C -- Potential evopotranspiration using Penman-Monteith.
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

class PetPM : public Pet
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
    {
      return potential_evapotranspiration_wet;
    }

  double dry () const
    {
      return potential_evapotranspiration_dry;
    }

  // Create & Destroy.
  PetPM (Block& al)
    : Pet (al)
    { }
  ~PetPM ()
    { }
};

void
PetPM::tick (const Time&, const Weather& weather, const double Rn, 
	     const Vegetation& crops,
	     const Surface& surface, const Geometry& geo,
             const Soil& soil,
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

  const double LAI = crops.LAI ();
  if (LAI > 0.0)
    {
      const double CropHeight = 0.01 * crops.height (); //cm -> m
      const double ScreenHeight = weather.screen_height ();

      // Dry.
      reference_evapotranspiration_dry
	= FAO::PenmanMonteith (CropHeight, ScreenHeight, 
                               LAI, crops.rs_min (),
                               Rn, G, Temp, VaporPressure,
                               U2, AtmPressure) * 3600;
      potential_evapotranspiration_dry
	= std::max (0.0, reference_evapotranspiration_dry);

      // Wet.
      reference_evapotranspiration_wet
	= FAO::PenmanMonteith (CropHeight, ScreenHeight, 
                               LAI, 0.0, Rn, G, Temp, VaporPressure,
                               U2, AtmPressure) * 3600;
      potential_evapotranspiration_wet
	= std::max (0.0, reference_evapotranspiration_wet);
    }
  else
    {
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
}

static struct PetPMSyntax
{
  static Model& make (Block& al)
  { return *new PetPM (al); }
  PetPMSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description",
	       "Potential evopotranspiration using Penman-Monteith.");
    Pet::load_syntax (syntax, alist);
    Syntax Rn_syntax;
    AttributeList Rn_alist;
    Rn_alist.add ("type", "brunt");
    alist.add ("net_radiation", Rn_alist);
    Librarian<Pet>::add_type ("PM", alist, syntax, &make);
  }
} PetPM_syntax;
