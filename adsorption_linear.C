// adsorption_linear.C -- Lininear Freundlich adsorption.
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
#include "block.h"
#include "alist.h"
#include "check.h"
#include "soil.h"
#include "librarian.h"

static const double c_fraction_in_humus = 0.587;

class AdsorptionLinear : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double K_OC;

  // Simulation.
public:
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    {
      const double K = soil.clay (i) * K_clay
	+ soil.humus (i) * c_fraction_in_humus * K_OC;
      const double rho = soil.dry_bulk_density (i);
      return C * (K * rho + Theta);
    }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    {
      const double K = soil.clay (i) * K_clay 
	+ soil.humus (i) * c_fraction_in_humus * K_OC;
      const double rho = soil.dry_bulk_density (i);
      return M / (Theta + K * rho);
    }
  // Create.
public:
  AdsorptionLinear (Block& al)
    : Adsorption (al),
      K_clay (al.number ("K_clay", 0.0)),
      K_OC (al.check ("K_OC") ? al.number ("K_OC") : al.number ("K_clay"))
    { }
};

static struct AdsorptionLinearSyntax
{
  static Model& make (Block& al)
  {
    return *new AdsorptionLinear (al);
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
  AdsorptionLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "M = rho K C + Theta C");
    syntax.add ("K_clay", "cm^3/g", Check::non_negative (), 
		Syntax::OptionalConst, 
		"Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n\
the 'K' factor.  If 'K_OC' is specified, 'K_clay' defaults to 0.");
    syntax.add ("K_OC", "cm^3/g", Check::non_negative (), 
		Syntax::OptionalConst, 
		"Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the 'K' factor.  By default, 'K_OC' is equal to 'K_clay'.");

    Librarian::add_type (Adsorption::component, "linear", alist, syntax, &make);
  }
} AdsorptionLinear_syntax;
