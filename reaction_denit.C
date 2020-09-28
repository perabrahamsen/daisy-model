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
#include "block_model.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic.h"
#include "chemistry.h"
#include "chemical.h"
#include "plf.h"
#include "log.h"
#include "check.h"
#include "mathlib.h"
#include "frame.h"
#include "treelog.h"

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
  const double redox_height;    // Chemical denitrification below this depth.
  
  // Log variable.
  std::vector<double> converted;
  std::vector<double> converted_fast;
  std::vector<double> converted_redox;
  std::vector<double> potential;
  std::vector<double> potential_fast;

  // Output.
  void output (Log& log) const;

  // Simulation.
  void tick_soil (const Geometry& geo,
                  const Soil& soil, const SoilWater& soil_water, 
                  const SoilHeat& soil_heat,
                  OrganicMatter& organic_matter, 
                  Chemistry& chemistry, const double dt, Treelog& msg);

  // Create.
  bool check (const Geometry&, 
              const Soil& soil, const SoilWater& soil_water, 
              const SoilHeat& soil_heat,
	      const OrganicMatter&, const Chemistry& chemistry,
	      Treelog& msg) const;
  void initialize (const Geometry&, 
                   const Soil&, const SoilWater&, const SoilHeat&,
                   const OrganicMatter&, const Surface&, Treelog&);
  explicit ReactionDenit (const BlockModel& al);
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
ReactionDenit::tick_soil (const Geometry& geo,
                          const Soil& soil, const SoilWater& soil_water,
                          const SoilHeat& soil_heat,
                          OrganicMatter& organic_matter, Chemistry& chemistry, 
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
      const double height = geo.cell_z (i);
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
        = (std::min (rate, K * NO3) + std::min (rate_fast, K_fast * NO3)) * dt;
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
  soil_NO3.add_to_transform_sink (converted);
}

bool 
ReactionDenit::check (const Geometry&,
                      const Soil&, const SoilWater&, const SoilHeat&,
                      const OrganicMatter&, const Chemistry& chemistry,
		      Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.know (Chemical::NO3 ()))
    {
      msg.error ("Denitrification requires NO3 to be tracked");
      ok = false;
    }
  return ok;
}

void
ReactionDenit::initialize (const Geometry&,
                           const Soil& soil, const SoilWater&,
                           const SoilHeat&, const OrganicMatter&,
			   const Surface&, Treelog&)
{
  const size_t cell_size = soil.size ();

  converted.insert (converted.begin (), cell_size, 0.0);
  converted_fast.insert (converted_fast.begin (), cell_size, 0.0);
  converted_redox.insert (converted_redox.begin (), cell_size, 0.0);
  potential.insert (potential.begin (), cell_size, 0.0);
  potential_fast.insert (potential_fast.begin (), cell_size, 0.0);
}

ReactionDenit::ReactionDenit (const BlockModel& al)
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

static struct ReactionDenitSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionDenit (al); }
  ReactionDenitSyntax ()
    : DeclareModel (Reaction::component, "denitrification", "\
Denitrification in soil, (conversion of nitrate to atmospheric nitrogen).\n\
\n\
In this model, it is made proportional to the CO2 development, as\n\
specified by the parameter alpha, with a maximum rate specified by the\n\
parameter 'K'.  The denitrification is also affected by temperature\n\
and water pressure.  Additional denitrification from CO2 produced from\n\
fast OM pools can be triggered by setting alpha_fast or\n\
water_factor_fast different.  This additional denitrification is\n\
limited by K_fast.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("converted", "g/cm^3/h",
                   Attribute::LogOnly, Attribute::Variable,
                   "Amount of denitrification.");
    frame.declare ("converted_fast", "g/cm^3/h",
                   Attribute::LogOnly, Attribute::SoilCells,
                   "Additional denitrification due to turnover in fast pools.");
    frame.declare ("converted_redox", "g/cm^3/h",
                   Attribute::LogOnly, Attribute::SoilCells, "\
Additional denitrification due to chemical redox processes.");
    frame.declare ("potential", "g/cm^3/h",
                   Attribute::LogOnly, Attribute::SoilCells, "\
Potential amount of denitrification at anarobic conditions.");
    frame.declare ("potential_fast", "g/cm^3/h",
                   Attribute::LogOnly, Attribute::SoilCells,
                   "Additional potential due to turnover in fast pools.");
    frame.declare ("K", "h^-1", Check::fraction (), Attribute::Const, "\
Maximum fraction of nitrate converted at each time step from slow pools.");
    frame.set ("K", 0.020833);
    frame.declare ("K_fast", "h^-1", Check::fraction (),
                   Attribute::OptionalConst, "\
Maximum fraction of nitrate converted at each time step from fast pools.\n \
By default this is identical to 'K'.");
    frame.declare ("alpha", "(g NO3-N/h)/(g CO2-C/h)", Check::non_negative (),
                   Attribute::Const, "\
Anaerobic denitrification constant for slow pools.");
    frame.set ("alpha", 0.1);
    frame.declare ("alpha_fast",
                   "(g NO3-N/h)/(g CO2-C/h)", Check::non_negative (),
                   Attribute::OptionalConst, "\
Anaerobic denitrification constant for fast pools.\n                    \
This applies to the CO2 produced from turnover of fast OM pools.\n\
By default, this is identical to alpha.");
    frame.declare ("heat_factor", "dg C", Attribute::None (),
                   Check::non_negative (),
                   Attribute::OptionalConst, "Heat factor.\n\
By default, use a build in function valid for temperate climates.");
    frame.declare ("water_factor", Attribute::Fraction (), Attribute::None (), 
                   Check::non_negative (),
                   Attribute::OptionalConst,
                   "Water potential factor for slow pools.\n\
This is a function of the current water content as a fraction of the\n\
maximal water content.");
    PLF water_factor;
    water_factor.add (0.7, 0.0);
    water_factor.add (1.0, 1.0);
    frame.set ("water_factor", water_factor);
    frame.declare ("water_factor_fast",
                   Attribute::Fraction (), Attribute::None (), 
                   Check::non_negative (),
                   Attribute::OptionalConst,
                   "Water potential factor for fast pools\n\
By default, this is identical to the 'water_factor' parameter.");
    frame.declare  ("redox_height", "cm", Check::non_positive (),
                    Attribute::OptionalConst,  "\
Height (a negative number) blow which redox processes start.\n  \
All NO3 below this height will be denitrified immediately.\n\
By default no redox denitrification occurs.");
  }
} ReactionDenit_syntax;

// reaction_denit.C ends here.
