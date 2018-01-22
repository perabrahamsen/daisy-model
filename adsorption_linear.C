// adsorption_linear.C -- Lininear adsorption.
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
#include "check.h"
#include "soil.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

static const double c_fraction_in_humus = 0.587;

class AdsorptionLinearOld : public AdsorptionLinear
{
  // Parameters.
  const double K_d;
  const double K_clay;
  const double K_OC;

  // Simulation.
public:
  double K (const Soil& soil, size_t c) const
  { 
    if (K_d >= 0.0)
      return K_d;

    return soil.clay (c) * K_clay 
      + soil.humus (c) * c_fraction_in_humus * K_OC;
  }

  double C_to_M (const Soil& soil, double Theta, int i, 
                 double C, double sf) const
  {
    const double K = this->K (soil, i);
    const double rho = soil.dry_bulk_density (i);
    return C * (K * rho * sf + Theta);
  }
  double M_to_C (const Soil& soil, double Theta, int i, 
                 double M, double sf) const
  {
    const double K = this->K (soil, i);
    const double rho = soil.dry_bulk_density (i);
    return M / (Theta + K * rho * sf);
  }

  // Create.
public:
  AdsorptionLinearOld (const BlockModel& al)
    : AdsorptionLinear (al),
      K_d (al.number ("K_d", -1.0)),
      K_clay (al.number ("K_clay", 0.0)),
      K_OC (al.check ("K_OC") ? al.number ("K_OC") : K_clay)
  { }
};

static struct AdsorptionLinearOldSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  {
    return new AdsorptionLinearOld (al);
  }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  {
    bool ok = true;

    const bool has_K_d = al.check ("K_d");
    const bool has_K_clay = al.check ("K_clay");
    const bool has_K_OC = al.check ("K_OC");
      
    if (!has_K_d && !has_K_clay && !has_K_OC)
      {
	err.entry ("You must specify either 'K_d', 'K_clay' or 'K_OC'");
	ok = false;
      }
    return ok;
  }
  AdsorptionLinearOldSyntax ()
    : DeclareModel (Adsorption::component, "linear", "M = rho K C + Theta C")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare ("K_d", "cm^3/g", Check::non_negative (), 
		Attribute::OptionalConst, 
		"Soil dependent distribution parameter.\n\
By default, it will be calculated from 'K_OC' and 'K_clay'.");
    frame.declare ("K_clay", "cm^3/g", Check::non_negative (), 
		Attribute::OptionalConst, 
		"Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n\
the 'K_d' factor.  If 'K_OC' is specified, 'K_clay' defaults to 0.");
    frame.declare ("K_OC", "cm^3/g", Check::non_negative (), 
		Attribute::OptionalConst, 
		"Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the 'K_d' factor.  By default, 'K_OC' is equal to 'K_clay'.");

  }
} AdsorptionLinearOld_syntax;

// adsorption_linear.C ends here.
