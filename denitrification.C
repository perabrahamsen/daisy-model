// denitrification.C
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


#include "denitrification.h"
#include "alist.h"
#include "syntax.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic_matter.h"
#include "soil_NO3.h"
#include "plf.h"
#include "log.h"
#include "submodel.h"
#include "check.h"

static double f_Theta (double x)
{
  if (x < 0.8)
    return 0.0;
  if (x < 0.9)
    return 2.0 * (x - 0.8);
  
  return 0.2 + 8.0 * (x - 0.9);
}

static double f_T (double T)
{
  if (T < 2.0)
    return 0.0;
  if (T < 6.0)
    return 0.15 * (T - 2.0);
  if (T < 20.0)
    return 0.10 * T;
  if (T < 40)
    return exp (0.47 - 0.027 * T + 0.00193 * T * T);

  daisy_assert (false);
}

void
Denitrification::output (Log& log) const
{
  log.output ("converted", converted);
}

void Denitrification::tick (const Soil& soil, const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    SoilNO3& soil_NO3, 
			    const OrganicMatter& organic_matter)
{
  converted.erase (converted.begin (), converted.end ());

  unsigned int size = soil.size ();
  if (!active_underground)
    size = min (size, soil.interval_plus (soil.MaxRootingDepth ()));
  if (!active_groundwater)
    size = soil_water.first_groundwater_node ();

  for (unsigned int i = 0; i < size; i++)
    {
      const double CO2 = organic_matter.CO2 (i);
      const double Theta = soil_water.Theta (i);
      const double Theta_sat = soil_water.Theta (soil, i, 0.0);
      const double Theta_fraction = Theta / Theta_sat;

      const double T = soil_heat.T (i);
      
      const double T_factor = (heat_factor.size () < 1)
	? f_T (T)
	: heat_factor (T);
      const double w_factor = (water_factor.size () < 1)
	? f_Theta (Theta_fraction)
	: water_factor (Theta_fraction);

      const double rate = w_factor * T_factor * alpha * CO2 ;
      const double M = min (rate, K * soil_NO3.M_left (i) / dt);
      converted.push_back (M);
    }
  soil_NO3.add_to_sink (converted);
}

void
Denitrification::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Denitrification");
  alist.add ("description", "Denitrification in soil (conversion\n\
of nitrate to atmospheric nitrogen).  In this model, it is made\n\
proportional to the CO2 development, as specified by the parameter\n\
alpha, with a maximum rate specified by the parameter 'K'.  The\n\
denitrification is also affected by temperature and water pressure.");
  syntax.add ("active_underground", Syntax::Boolean, Syntax::Const, "\
Set this flag to turn on denitrification below the root zone.");
  alist.add ("active_underground", false);
  syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off denitrification in groundwater.");
  alist.add ("active_groundwater", true);
  syntax.add ("converted", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Amount of denitrification.");
  syntax.add ("K", "h^-1", Check::fraction (), Syntax::Const, "\
Maximum fraction of nitrate converted at each time step.");
  alist.add ("K", 0.020833);
  syntax.add ("alpha", "(g NO3-N/h)/(g CO2-C/h)", Check::non_negative (),
	      Syntax::Const, "Anaerobic denitrification constant.");
  alist.add ("alpha", 0.1);
  PLF empty;
  syntax.add ("heat_factor", "dg C", Syntax::None (), Check::non_negative (),
	      Syntax::Const, "Heat factor.");
  alist.add ("heat_factor", empty);
  syntax.add ("water_factor", Syntax::Fraction (), Syntax::None (), 
	      Check::non_negative (),
	      Syntax::Const,
	      "Water potential factor, a function of the current\n\
water content as a fraction of the maximal water content.");
  alist.add ("water_factor", empty);
}

Denitrification::Denitrification (const AttributeList& al)
  : active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    K (al.number ("K")),
    alpha (al.number ("alpha")),
    heat_factor (al.plf ("heat_factor")),
    water_factor (al.plf ("water_factor"))
{ }

static Submodel::Register 
denitrification_submodel ("Denitrification", Denitrification::load_syntax);
