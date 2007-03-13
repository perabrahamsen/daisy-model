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


#include "pet.h"
#include "syntax.h"
#include "weather.h"
#include "fao.h"
#include "log.h"
#include "mathlib.h"

using namespace std;

struct PetHargreaves : public Pet
{
  // State.
  double reference_evapotranspiration;
  double potential_evapotranspiration;

  // Simulation.
  void tick (const Time& time, const Weather& weather, const double, const Vegetation& crops,
	     const Surface& surface, const Geometry&,
             const Soil&, const SoilHeat&,
	     const SoilWater&, Treelog&)
    {
      const double K_hs = 0.0023;
      const double T_avg = weather.daily_air_temperature ();
      const double T_diff = max (weather.daily_max_air_temperature () 
                                 - weather.daily_min_air_temperature (),
                                 0.0);
      static const double s_per_d = 60.0 * 60.0 * 24.0; // [W] -> [J/d]
      const double latent_heat_of_vaporation = 
        FAO::LatentHeatVaporization (T_avg); // [J/kg] 
      static const double W_per_m2_to_mm_per_d = s_per_d 
        / latent_heat_of_vaporation;
      // Note: andr. uses 0.4081633.  What are his units?
      double Ra = weather.ExtraterrestrialRadiation (time)
        * weather.day_cycle (time) // [d^-1] -> [h^-1]
        * W_per_m2_to_mm_per_d; // [mm/h]
      

      reference_evapotranspiration 
        = K_hs * (T_avg + 17.8) * sqrt (T_diff) * Ra;

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
  PetHargreaves (Block& al)
    : Pet (al)
    { }
};

static struct PetHargreavesSyntax
{
  static Pet&
  make (Block& al)
    { return *new PetHargreaves (al); }
  PetHargreavesSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", 
		 "Potential evopotranspiration using Samani and Hargreaves.\n\
\n\
Hargreaves, G.H., and Samani, Z.A. (1982) Estimating potential\n\
evapotranspiration. Tech. Note, J. Irrig. and Drain. Engrg., ASCE,\n\
108(3):225-230.\n\
\n\
Hargreaves, G.H., and Samani, Z.A. (1985) Reference crop\n\
evapotranspiration from temperature. Appl. Engrg. in Agric.,\n\
1(2):96-99.");
      Pet::load_syntax (syntax, alist);
      Librarian<Pet>::add_type ("Hargreaves", alist, syntax, &make);
    }
} PetHargreaves_syntax;
