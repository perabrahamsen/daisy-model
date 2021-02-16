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
#include <sstream>

static const double c_fraction_in_humus = 0.587;

class AdsorptionFreundlich : public Adsorption
{
  // Parameters.
  const double K_clay;
  const double K_OC;
  const double C_ref;
  const double m;

  // Simulation.
public:
  double C_to_M (const Soil&, double Theta, int, double C, double sf) const;
  double M_to_C (const Soil&, double Theta, int, double M, double sf) const;

  // Create.
public:
  AdsorptionFreundlich (const BlockModel& al)
    : Adsorption (al),
      K_clay (al.number ("K_clay", 0.0)),
      K_OC (al.number ("K_OC", K_clay)),
      C_ref (al.number ("C_ref")),
      m (al.number ("m"))
    { }
};

double 
AdsorptionFreundlich::C_to_M (const Soil& soil,
			      double Theta, int i, double C, double sf) const
{
  daisy_assert (std::isfinite (C));
  if (C <= 0.0)
    {
      if (C < 0.0)
	{
	  std::ostringstream tmp;
	  tmp << "C = " << C << "; should be >= 0.0";
	  Assertion::warning (tmp.str ());
	}
      return 0.0;
    }
  daisy_assert (Theta >= 0.0); 
  const double K = soil.clay (i) * K_clay 
    + soil.humus (i) * c_fraction_in_humus * K_OC;
  const double rho = soil.dry_bulk_density (i);
  return sf * rho * K * C_ref * pow (C / C_ref, m) + Theta * C;
}

double 
AdsorptionFreundlich::M_to_C (const Soil& soil,
			      double Theta, int i, double M, double sf) const
{
  // Check for zero.
  if (iszero (M))
    return 0.0;
  daisy_assert (M > 0.0);
  
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
      daisy_assert (new_C >= 0.0);
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
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
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
    : DeclareModel (Adsorption::component, "Freundlich", "\
M = rho K C_ref (C / C_ref)^m + Theta C.\n\
Formula from FOCUS page 93.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.set_strings ("cite", "freundlich1939adsorption", "focus2000");
    frame.declare ("K_clay", "cm^3/g", Check::non_negative (),
                   Attribute::OptionalConst, 
                   "Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n  \
the 'K' factor.  If 'K_OC' is specified, 'K_clay' defaults to 0.");
    frame.declare ("K_OC", "cm^3/g", Check::non_negative (), 
                   Attribute::OptionalConst, 
                   "Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the 'K' factor.  By default, 'K_OC' is equal to 'K_clay'.");
    frame.declare_number_cited ("m", Attribute::None (), Check::positive (),
				Attribute::Const, Attribute::Singleton, "\
Freundlich parameter (1/n).\n\
If not known, FOCUS recommend a value of 0.9 (page 93).",
				"focus2000");
    frame.declare_number_cited ("C_ref", "g/cm^3", Check::positive (),
				Attribute::Const, Attribute::Singleton, "\
Reference concentration for determining the other parameters.\n\
According to FOCUS, the usual value is 1 mg/L (page 93).",
				"focus2000");
  }
} AdsorptionFreundlich_syntax;

// adsorption_freundlich.C ends here.
