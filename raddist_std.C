// raddist_std.C -- Radiation distribution standard model.
// 
// Copyright 2006 Birgitte Gjettermann, Per Abrahamsen and KVL
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "raddist.h"
#include "syntax.h"
#include "vegetation.h"
#include "mathlib.h"
#include <sstream>

struct RaddistStandard : public Raddist
{
  // Simulation.
  void tick (std::vector <double>& sun_LAI, std::vector <double>& sun_PAR, 
	     std::vector <double>& total_PAR, double global_radiation, 
	     double diffuse_radiation, double sin_beta, const Vegetation&, Treelog&);
  void output (Log& log) const
  {
    Raddist::output (log);
  }

  // Create.
  RaddistStandard (Block& al)
    : Raddist (al)
  { }
};

void RaddistStandard::tick (std::vector <double>& sun_LAI_fraction,
			    std::vector <double>& sun_PAR, 
			    std::vector <double>& total_PAR, double global_radiation, 
			    double /*diffuse_radiation*/, double /*sin_beta*/, 
			    const Vegetation& vegetation,
			    Treelog&)
{
  const size_t No = sun_LAI_fraction.size ();
  daisy_assert (No + 1 == total_PAR.size());
  daisy_assert (No + 1 == sun_PAR.size());

  const double LAI = vegetation.LAI ();

  // No LAI
  if (iszero (LAI))
  {
    std::fill (&total_PAR[0], &total_PAR[No+1], 0.0);
    return;
  }
  //Fill empty vectors
  std::fill (&sun_PAR[0], &sun_PAR[No+1], 0.0); 
  std::fill (&sun_LAI_fraction[0], &sun_LAI_fraction[No], 0.0);

  // Average Canopy Extinction coefficient
  // (how fast the light dim as a  function of LAI passed).
  const double ACExt = vegetation.ACExt ();

  // Average Canopy Reflection coefficient
  const double ACRef =  vegetation.ACRef ();

  radiation_distribution (No, LAI, ACRef, global_radiation,
			  ACExt, total_PAR);

}

static struct RaddistStandardSyntax
{
  static Model& make (Block& al)
  { return *new RaddistStandard (al); }
  RaddistStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Default model of radiation distribution in the canopy.");
    Raddist::load_syntax (syntax, alist);
    BuildBase::add_type (Raddist::component, "default", alist, syntax, &make);
  }
} RaddistStandard_syntax;

const AttributeList& 
Raddist::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax syntax;
      Raddist::load_syntax (syntax, alist);
      alist.add ("type", "default");
    }
  return alist;
}
