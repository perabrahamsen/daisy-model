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


#include "adsorption.h"
#include "block.h"
#include "alist.h"
#include "soil.h"
#include "check.h"
#include "mathlib.h"

static const double c_fraction_in_humus = 0.587;

class AdsorptionFreundlich : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double K_OC;
  const double m;

  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C) const;
  double M_to_C (const Soil&, double Theta, int, double M) const;

  // Create.
public:
  AdsorptionFreundlich (Block& al)
    : Adsorption (al),
      K_clay (al.number ("K_clay", 0.0)),
      K_OC (al.number ("K_OC", K_clay)),
      m (al.number ("m"))
    { }
};

double 
AdsorptionFreundlich::C_to_M (const Soil& soil,
			      double Theta, int i, double C) const
{
  daisy_assert (C >= 0.0);
  daisy_assert (Theta >= 0.0); 
  const double K = soil.clay (i) * K_clay 
    + soil.humus (i) * c_fraction_in_humus * K_OC;
  const double rho = soil.dry_bulk_density (i);
  const double S = K * pow (C, m);
  return rho * S + Theta * C;
}

double 
AdsorptionFreundlich::M_to_C (const Soil& soil,
			      double Theta, int i, double M) const
{
  // Check for zero.
  if (iszero (M))
    return 0.0;

  // Guess start boundary.
  double min_C = 0.0;
  double min_M = C_to_M (soil, Theta, i, min_C);
  double max_C = 1.0;
  double max_M = C_to_M (soil, Theta, i, max_C);

  // Find upper boundary by doubling repeatedly.
  while (max_M < M)
    {
      max_C *= 2;
      daisy_assert (max_C > 0.0); // Overlow detection.
      max_M = C_to_M (soil, Theta, i, max_C);
    }

  // Guess by middling the C value.
  while (!approximate (min_M, max_M))
    {
      const double new_C = (min_C + max_C) / 2.0;
      const double new_M = C_to_M (soil, Theta, i, new_C);
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

static struct AdsorptionFreundlichSyntax
{
  static Model& make (Block& al)
  {
    return *new AdsorptionFreundlich (al);
  }
  static bool check_alist (const AttributeList& al, Treelog& err)
    {
      bool ok = true;

      const bool has_K_clay = al.check ("K_clay");
      const bool has_K_OC = al.check ("K_OC");
      
      if (!has_K_clay && !has_K_OC)
	{
	  err.entry ("You must specify either 'K_clay' or 'K_OC'");
	  ok = false;
	}
      return ok;
    }

  AdsorptionFreundlichSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "M = rho K C^m + Theta C");
    syntax.add ("K_clay", "(g/cm^3)^-m", Check::non_negative (),
		Syntax::OptionalConst, 
		"Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n\
the 'K' factor.  If 'K_OC' is specified, 'K_clay' defaults to 0.\n\
The dimension depends on the 'm' parameter.");
    syntax.add ("K_OC", "(g/cm^3)^-m", Check::non_negative (), 
		Syntax::OptionalConst, 
		"Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the 'K' factor.  By default, 'K_OC' is equal to 'K_clay'.\n\
The dimension depends on the 'm' parameter.");
    syntax.add ("m", Syntax::None (), Check::non_negative (), Syntax::Const,
		"Freundlich parameter");
    BuildBase::add_type (Adsorption::component, "Freundlich", alist, syntax, &make);
  }
} AdsorptionFreundlich_syntax;
