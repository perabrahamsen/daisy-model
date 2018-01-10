// adsorption_freundlich.C
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

#define BUILD_DLL

#include "adsorption.h"
#include "block_model.h"
#include "soil.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include "units.h"
#include "metalib.h"

static const double c_fraction_in_humus = 0.587;

class AdsorptionFreundlich : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double K_OC;
  const double C_factor;
  const double m;

  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C, double sf) const;
  double M_to_C (const Soil&, double Theta, int, double M, double sf) const;

  // Create.
private:
  static double find_C_factor (const BlockModel& al)
  {
    const bool has_K_clay = al.check ("K_clay");
    const bool has_K_OC = al.check ("K_OC");

    if (!has_K_clay && !has_K_OC)
      return -42.42e42;
    
    const symbol K_user_unit 
      = has_K_clay ? al.name ("K_clay") : al.name ("K_OC");
    const symbol K_base_unit = "cm^3/g";
    const Units& units = al.units ();
    if (!units.can_convert (K_user_unit, K_base_unit))
      return -42.42e42;
    
    // TODO: Er det rigtigt?
    return units.convert (K_user_unit, K_base_unit, 1.0);
  }
public:
  AdsorptionFreundlich (const BlockModel& al)
    : Adsorption (al),
      K_clay (al.number ("K_clay", 0.0)),
      K_OC (al.number ("K_OC", K_clay)),
      C_factor (find_C_factor (al)),
      m (al.number ("m"))
    { }
};

double 
AdsorptionFreundlich::C_to_M (const Soil& soil,
			      double Theta, int i, double C, double sf) const
{
  daisy_assert (C >= 0.0);
  daisy_assert (Theta >= 0.0); 
  const double K = soil.clay (i) * K_clay 
    + soil.humus (i) * c_fraction_in_humus * K_OC;
  const double rho = soil.dry_bulk_density (i);
  const double S = K * pow (C * C_factor, m);
  return sf * rho * S + Theta * C;
}

double 
AdsorptionFreundlich::M_to_C (const Soil& soil,
			      double Theta, int i, double M, double sf) const
{
  // Check for zero.
  if (iszero (M))
    return 0.0;

  // Guess start boundary.
  double min_C = 0.0;
  double min_M = C_to_M (soil, Theta, i, min_C, sf);
  double max_C = 1.0;
  double max_M = C_to_M (soil, Theta, i, max_C, sf);

  // Find upper boundary by doubling repeatedly.
  while (max_M < M)
    {
      max_C *= 2;
      daisy_assert (max_C > 0.0); // Overlow detection.
      max_M = C_to_M (soil, Theta, i, max_C, sf);
    }

  // Guess by middling the C value.
  while (!approximate (min_M, max_M))
    {
      const double new_C = (min_C + max_C) / 2.0;
      const double new_M = C_to_M (soil, Theta, i, new_C, sf);
      if (new_M < M)
	{
          daisy_assert (min_C < new_C);
	  min_C = new_C;
	  min_M = new_M;
	}
      else
	{
          daisy_assert (max_C > new_C);
	  max_C = new_C;
	  max_M = new_M;
	}
    }
  return (min_C + max_C) / 2.0;
}

static struct AdsorptionFreundlichSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new AdsorptionFreundlich (al); }
  static bool check_alist (const Metalib& metalib, const Frame& al, Treelog& err)
  {
    bool ok = true;

    const bool has_K_clay = al.check ("K_clay");
    const bool has_K_OC = al.check ("K_OC");

    if (!has_K_clay && !has_K_OC)
      {
        err.entry ("You must specify either 'K_clay' or 'K_OC'");
        ok = false;
      }
    if (has_K_clay && has_K_OC)
      {
        const symbol K_clay_unit = al.name ("K_clay");
        const symbol K_OC_unit = al.name ("K_clay");
        if (K_clay_unit != K_OC_unit)
          {
            err.entry ("K_clay is [" + K_clay_unit + "] and K_OC is ["
                       + K_OC_unit + "], they must be identical");
            ok = false;
          }
      }
    const Units& units = metalib.units ();
    static const symbol base_unit = "cm^3/g";
    if (has_K_clay)
      {
        const symbol unit =  al.name ("K_clay");
        if (!units.can_convert (unit, base_unit, 1.0))
          {
            err.entry ("Can't convert K_clay [" + unit 
                       + "] to [" + base_unit + "]");
            ok = false;
          }
      }
    if (has_K_OC)
      {
        const symbol unit =  al.name ("K_OC");
        if (!units.can_convert (unit, base_unit, 1.0))
          {
            err.entry ("Can't convert K_OC [" + unit
                       + "] to [" + base_unit + "]");
            ok = false;
          }
      }
    return ok;
  }
  AdsorptionFreundlichSyntax ()
    : DeclareModel (Adsorption::component, "Freundlich", "\
M = rho K C^m + Theta C.\n\
For calculating 'K C^m', C is first converted to the resiprocal\n\
unit of K, and C^m is considered to have the same unit as C, so\n\
the expression becomes dimensionless.\n\
\n\
WARNING: Because the unit you specify for K (OC and clay) also determines\n\
the unit used for C, it is important that you specify the origial unit.\n\
\n\
K = 1000 mL/g is NOT the same a K = 1 L/g unless m = 1.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare ("K_clay", Attribute::User (), Check::non_negative (),
                   Attribute::OptionalConst, 
                   "Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n  \
the 'K' factor.  If 'K_OC' is specified, 'K_clay' defaults to 0.");
    frame.declare ("K_OC", Attribute::User (), Check::non_negative (), 
                   Attribute::OptionalConst, 
                   "Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the 'K' factor.  By default, 'K_OC' is equal to 'K_clay'.");
    frame.declare ("m", Attribute::None (), Check::non_negative (), Attribute::Const,
                   "Freundlich parameter (1/n).");
  }
} AdsorptionFreundlich_syntax;

// adsorption_freundlich.C ends here.
