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
#include "mathlib.h"

using namespace std;

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
  output_variable (converted, log);
  output_variable (converted_fast, log);
  output_variable (converted_redox, log);
  output_variable (potential, log);
  output_variable (potential_fast, log);
}

void Denitrification::tick (const Soil& soil, const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    SoilNO3& soil_NO3, 
			    const OrganicMatter& organic_matter)
{
  converted.erase (converted.begin (), converted.end ());
  converted_fast.erase (converted_fast.begin (), converted_fast.end ());
  converted_redox.erase (converted_redox.begin (), converted_redox.end ());
  potential.erase (potential.begin (), potential.end ());
  potential_fast.erase (potential_fast.begin (), potential_fast.end ());

  unsigned int size = soil.size ();
  if (!active_underground)
    size = min (size, soil.interval_plus (soil.MaxRootingDepth ()));
  if (!active_groundwater)
    size = soil_water.first_groundwater_node ();

  for (unsigned int i = 0; i < size; i++)
    {
      const double CO2_fast = organic_matter.CO2_fast (i);
      const double CO2_slow = organic_matter.CO2 (i) - CO2_fast;
      const double Theta = soil_water.Theta (i);
      const double Theta_sat = soil_water.Theta (soil, i, 0.0);
      const double Theta_fraction = Theta / Theta_sat;
      const double NO3 = soil_NO3.M_left (i) / dt;
      const double T = soil_heat.T (i);
      const double height = soil.z (i);
      const double T_factor = (heat_factor.size () < 1)
	? f_T (T)
	: heat_factor (T);
      const double pot = T_factor * alpha * CO2_slow;
      const double w_factor = water_factor (Theta_fraction);
      const double rate = w_factor * pot;

      const double pot_fast = T_factor * alpha_fast * CO2_fast;
      const double w_factor_fast = water_factor_fast (Theta_fraction);
      const double rate_fast = w_factor_fast * pot_fast;

      const double M = min (rate, K * NO3) + min (rate_fast, K_fast * NO3);
      if (redox_height <= 0 && height < redox_height)
	{
	  converted.push_back (NO3);
	  converted_redox.push_back (NO3 - M);
	}
      else
	{
	  converted.push_back (M);
	  converted_redox.push_back (0.0);
	}
      converted_fast.push_back (M > rate ? M - rate : 0.0);
      potential.push_back (pot);
      potential_fast.push_back (pot_fast);
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
denitrification is also affected by temperature and water pressure.\n\
Additional denitrification from CO2 produced from fast OM pools can\n\
be triggered by setting alpha_fast or water_factor_fast different.\n\
This additional denitrification is limited by K_fast.");
  syntax.add ("active_underground", Syntax::Boolean, Syntax::Const, "\
Set this flag to turn on denitrification below the root zone.");
  alist.add ("active_underground", false);
  syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off denitrification in groundwater.");
  alist.add ("active_groundwater", true);
  syntax.add ("converted", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Amount of denitrification.");
  syntax.add ("converted_fast", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Additional denitrification due to turnover in fast pools.");
  syntax.add ("converted_redox", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Additional denitrification due to chemical redox processes.");
  syntax.add ("potential", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Potential amount of denitrification at anarobic conditions.");
  syntax.add ("potential_fast", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Additional potential due to turnover in fast pools.");
  syntax.add ("K", "h^-1", Check::fraction (), Syntax::Const, "\
Maximum fraction of nitrate converted at each time step from slow pools.");
  alist.add ("K", 0.020833);
  syntax.add ("K_fast", "h^-1", Check::fraction (), Syntax::OptionalConst, "\
Maximum fraction of nitrate converted at each time step from fast pools.\n\
By default this is identical to 'K'.");
  syntax.add ("alpha", "(g NO3-N/h)/(g CO2-C/h)", Check::non_negative (),
	      Syntax::Const, "\
Anaerobic denitrification constant for slow pools.");
  alist.add ("alpha", 0.1);
  syntax.add ("alpha_fast", "(g NO3-N/h)/(g CO2-C/h)", Check::non_negative (),
	      Syntax::OptionalConst, "\
Anaerobic denitrification constant for fast pools.\n\
This applies to the CO2 produced from turnover of fast OM pools.\n\
By default, this is identical to alpha.");
  syntax.add ("heat_factor", "dg C", Syntax::None (), Check::non_negative (),
	      Syntax::OptionalConst, "Heat factor.\n\
By default, use a build in function valid for temperate climates.");
  syntax.add ("water_factor", Syntax::Fraction (), Syntax::None (), 
	      Check::non_negative (),
	      Syntax::OptionalConst,
	      "Water potential factor for slow pools.\n\
This is a function of the current water content as a fraction of the\n\
maximal water content.");
  PLF water_factor;
  water_factor.add (0.8, 0.0);
  water_factor.add (0.9, 0.2);
  water_factor.add (1.0, 1.0);
  alist.add ("water_factor", water_factor);
  syntax.add ("water_factor_fast", Syntax::Fraction (), Syntax::None (), 
	      Check::non_negative (),
	      Syntax::OptionalConst,
	      "Water potential factor for fast pools\n\
By default, this is identical to the 'water_factor' parameter.");
  syntax.add  ("redox_height", "cm", Check::non_positive (),
	       Syntax::OptionalConst,  "\
Height (a negative number) blow which redox processes start.\n\
All NO3 below this height will be denitrified immediately.\n\
By default no redox denitrification occurs.");
}

Denitrification::Denitrification (const AttributeList& al)
  : active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    K (al.number ("K")),
    K_fast (al.check ("K_fast") ? al.number ("K_fast") : K),
    alpha (al.number ("alpha")),
    alpha_fast (al.check ("alpha_fast") ? al.number ("alpha_fast") : alpha),
    heat_factor (al.check ("heat_factor") 
		 ? al.plf ("heat_factor") 
		 : PLF::empty ()),
    water_factor (al.plf ("water_factor")),
    water_factor_fast (al.check ("water_factor_fast" )
		       ? al.plf ("water_factor_fast")
		       : water_factor),
    redox_height (al.check ("redox_height") 
		  ? al.number ("redox_height")
		  : 1.0)
{ }

static Submodel::Register 
denitrification_submodel ("Denitrification", Denitrification::load_syntax);
