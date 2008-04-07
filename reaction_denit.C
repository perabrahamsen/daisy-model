// reaction_denit.C -- Denitrification.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "reaction.h"
#include "abiotic.h"
#include "librarian.h"
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic_matter.h"
#include "chemistry.h"
#include "chemical.h"
#include "plf.h"
#include "log.h"
#include "check.h"
#include "mathlib.h"

struct ReactionDenit : public Reaction
{
  // Parameters.
  const double K;
  const double K_fast;
  const double alpha;
  const double alpha_fast;
  const PLF heat_factor;
  const PLF water_factor;
  const PLF water_factor_fast;
  const double redox_height;	// Chemical denitrification below this depth.
  
  // Log variable.
  std::vector<double> converted;
  std::vector<double> converted_fast;
  std::vector<double> converted_redox;
  std::vector<double> potential;
  std::vector<double> potential_fast;

  // Output.
  void output (Log& log) const;

  // Simulation.
  void tick (const Geometry& geo,
	     const Soil& soil, const SoilWater& soil_water, 
	     const SoilHeat& soil_heat,
	     const OrganicMatter& organic_matter, 
             Chemistry& chemistry, const double dt, Treelog& msg);

  // Create.
  bool check (const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const Chemistry& chemistry, Treelog& msg) const;
  void initialize (const Soil& soil, Treelog&);
  explicit ReactionDenit (Block& al);
};

void
ReactionDenit::output (Log& log) const
{
  output_variable (converted, log);
  output_variable (converted_fast, log);
  output_variable (converted_redox, log);
  output_variable (potential, log);
  output_variable (potential_fast, log);
}

void 
ReactionDenit::tick (const Geometry& geo,
		     const Soil& soil, const SoilWater& soil_water,
		     const SoilHeat& soil_heat,
		     const OrganicMatter& organic_matter, 
		     Chemistry& chemistry, 
		     const double dt, Treelog&)
{
  const size_t cell_size = geo.cell_size ();
  const std::vector<bool> active = organic_matter.active (); 
  Chemical& soil_NO3 = chemistry.find (Chemical::NO3 ());

  for (size_t i = 0; i < cell_size; i++)
    {
      if (!active[i])
        {
          converted[i] = converted_fast[i] = converted_redox[i]
            = potential[i] = potential_fast[i] = 0.0;
          continue;
        }
      const double CO2_fast = organic_matter.CO2_fast (i);
      const double CO2_slow = organic_matter.CO2 (i) - CO2_fast;
      const double Theta = soil_water.Theta (i);
      const double Theta_sat = soil_water.Theta_ice (soil, i, 0.0);
      const double Theta_fraction = Theta / Theta_sat;
      const double NO3 = soil_NO3.C_primary (i) * Theta;
      const double T = soil_heat.T (i);
      const double height = geo.z (i);
      const double T_factor = (heat_factor.size () < 1)
	? Abiotic::f_T2 (T)
	: heat_factor (T);
      const double pot = T_factor * alpha * CO2_slow;
      const double w_factor = water_factor (Theta_fraction);
      const double rate = w_factor * pot;

      const double pot_fast = T_factor * alpha_fast * CO2_fast;
      const double w_factor_fast = water_factor_fast (Theta_fraction);
      const double rate_fast = w_factor_fast * pot_fast;

      const double M 
        = std::min (rate, K * NO3) + std::min (rate_fast, K_fast * NO3);
      if (redox_height <= 0 && height < redox_height)
	{
	  converted[i] = NO3 / dt;
	  converted_redox[i] = (NO3 - M) / dt;
	}
      else
	{
	  converted[i] = M / dt;
	  converted_redox[i] = 0.0;
	}
      converted_fast[i] = (M / dt > rate ? M / dt - rate : 0.0);
      potential[i] = pot;
      potential_fast[i] = pot_fast;
    }
  soil_NO3.add_to_transform_sink (converted, dt);
}

bool 
ReactionDenit::check (const Soil&, const SoilWater&, const SoilHeat&,
		      const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.require (Chemical::NO3 (), msg))
    ok = false;

  return ok;
}

void
ReactionDenit::initialize (const Soil& soil, Treelog&)
{
  const size_t cell_size = soil.size ();

  converted.insert (converted.begin (), cell_size, 0.0);
  converted_fast.insert (converted_fast.begin (), cell_size, 0.0);
  converted_redox.insert (converted_redox.begin (), cell_size, 0.0);
  potential.insert (potential.begin (), cell_size, 0.0);
  potential_fast.insert (potential_fast.begin (), cell_size, 0.0);
}

ReactionDenit::ReactionDenit (Block& al)
  : Reaction (al),
    K (al.number ("K")),
    K_fast (al.number ("K_fast", K)),
    alpha (al.number ("alpha")),
    alpha_fast (al.number ("alpha_fast", alpha)),
    heat_factor (al.check ("heat_factor") 
		 ? al.plf ("heat_factor") 
		 : PLF::empty ()),
    water_factor (al.plf ("water_factor")),
    water_factor_fast (al.check ("water_factor_fast" )
		       ? al.plf ("water_factor_fast")
		       : water_factor),
    redox_height (al.number ("redox_height", 1.0))
{ }

static struct ReactionDenitSyntax
{
  static Model& make (Block& al)
  { return *new ReactionDenit (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    alist.add ("description", "Denitrification in soil (conversion\n\
of nitrate to atmospheric nitrogen).  In this model, it is made\n\
proportional to the CO2 development, as specified by the parameter\n\
alpha, with a maximum rate specified by the parameter 'K'.  The\n\
denitrification is also affected by temperature and water pressure.\n\
Additional denitrification from CO2 produced from fast OM pools can\n\
be triggered by setting alpha_fast or water_factor_fast different.\n\
This additional denitrification is limited by K_fast.");
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
Maximum fraction of nitrate converted at each time step from fast pools.\n \
By default this is identical to 'K'.");
    syntax.add ("alpha", "(g NO3-N/h)/(g CO2-C/h)", Check::non_negative (),
		Syntax::Const, "\
Anaerobic denitrification constant for slow pools.");
    alist.add ("alpha", 0.1);
    syntax.add ("alpha_fast", "(g NO3-N/h)/(g CO2-C/h)", Check::non_negative (),
		Syntax::OptionalConst, "\
Anaerobic denitrification constant for fast pools.\n			\
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
    water_factor.add (0.7, 0.0);
    water_factor.add (1.0, 1.0);
    alist.add ("water_factor", water_factor);
    syntax.add ("water_factor_fast", Syntax::Fraction (), Syntax::None (), 
		Check::non_negative (),
	      Syntax::OptionalConst,
		"Water potential factor for fast pools\n\
By default, this is identical to the 'water_factor' parameter.");
    syntax.add  ("redox_height", "cm", Check::non_positive (),
		 Syntax::OptionalConst,  "\
Height (a negative number) blow which redox processes start.\n	\
All NO3 below this height will be denitrified immediately.\n\
By default no redox denitrification occurs.");
  }
  ReactionDenitSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    load_syntax (syntax, alist);
    Librarian::add_type (Reaction::component, "denitrification",
			 alist, syntax, &make);
  }
} ReactionDenit_syntax;

const AttributeList& 
Reaction::denitrification_model ()
{
  static AttributeList alist;
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ReactionDenitSyntax::load_syntax (dummy, alist);
      alist.add ("type", "denitrification");
    }
  return alist;

}

// reaction_denit.C ends here.
