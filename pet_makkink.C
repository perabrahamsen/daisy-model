// pet_makkink.C -- Potential evopotranspiration using Makkink's Equation.
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
#include "weather.h"
#include "fao.h"
#include "log.h"
#include "librarian.h"

struct PetMakkink : public Pet
{
  // State.
  double reference_evapotranspiration;
  double potential_evapotranspiration;

  // Simulation.
  void tick (const Time&, const Weather& weather, const double, const Vegetation& crops,
	     const Surface& surface, const Geometry&,
             const Soil&, const SoilHeat&,
	     const SoilWater&, Treelog&)
    {
      reference_evapotranspiration 
	= FAO::Makkink (weather.hourly_air_temperature (),
			weather.hourly_global_radiation ());
      potential_evapotranspiration 
	= reference_to_potential (crops, surface, 
				  reference_evapotranspiration);
    }

  void output (Log& log) const
    {
      Pet::output (log);
      output_value (reference_evapotranspiration, 
		  "reference_evapotranspiration", log);
    }

  double wet () const
    { return potential_evapotranspiration; }

  // Create.
  PetMakkink (Block& al)
    : Pet (al)
    { }
};

static struct PetMakkinkSyntax
{
  static Model& make (Block& al)
    { return *new PetMakkink (al); }
  PetMakkinkSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", 
		 "Potential evopotranspiration using Makkink's Equation.");
      Pet::load_syntax (syntax, alist);
      Librarian::add_type (Pet::component, "makkink", alist, syntax, &make);
    }
} PetMakkink_syntax;
