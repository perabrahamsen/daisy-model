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

#define BUILD_DLL

#include "raddist.h"
#include "vegetation.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

struct RaddistStandard : public Raddist
{
  // Simulation.
  void tick (std::vector <double>& sun_LAI, 
             std::vector <double>& sun_PAR, std::vector <double>& total_PAR, 
             std::vector <double>& sun_NIR, std::vector <double>& total_NIR, 
             double global_radiation, double diffuse_radiation, 
             double min_sin_beta, double sin_beta, const Vegetation&, double, Treelog&);

  void output(Log& log) const 
  {}

  // Create.
  RaddistStandard (const BlockModel& al)
    : Raddist (al)
  { }
};

void RaddistStandard::tick (std::vector <double>& sun_LAI_fraction,
			    std::vector <double>& sun_PAR, 
			    std::vector <double>& total_PAR, 
                            std::vector <double>& sun_NIR, 
                            std::vector <double>& total_NIR, 
                            double global_radiation, 
			    double /*diffuse_radiation*/, 
                            double /* min_sin_beta */, double /*sin_beta*/, 
			    const Vegetation& vegetation, double,
			    Treelog&) 
{
  const size_t No = sun_LAI_fraction.size ();
  daisy_assert (No + 1 == total_PAR.size());
  daisy_assert (No + 1 == sun_PAR.size());
  daisy_assert (No + 1 == total_NIR.size());
  daisy_assert (No + 1 == sun_NIR.size());

  const double LAI = vegetation.LAI ();

  //Fill empty vectors
  std::fill (sun_PAR.begin (), sun_PAR.end (), 0.0); 
  std::fill (sun_NIR.begin (), sun_NIR.end (), 0.0); 
  std::fill (sun_LAI_fraction.begin (), sun_LAI_fraction.end (), 0.0);

  // PAR:
  // Average Canopy Extinction coefficient of PAR
  // (how fast the light dim as a  function of LAI passed).
  const double ACExt_PAR = vegetation.ACExt_PAR ();

  // Average Canopy Reflection coefficient of PAR
  const double ACRef_PAR =  vegetation.ACRef_PAR ();
  daisy_assert (ACRef_PAR < 1.0);

  //Distribution of PAR in the canopy layers
  radiation_distribution (No, LAI, ACRef_PAR, global_radiation,	
                          ACExt_PAR, total_PAR, PARinSi);

  // NIR:
  // Average Canopy Extinction coefficient (of NIR)
  // (how fast the light dim as a  function of LAI passed).
  const double ACExt_NIR = vegetation.ACExt_NIR ();

  // Average Canopy Reflection coefficient (of NIR)
  const double ACRef_NIR =  vegetation.ACRef_NIR ();

  //Distribution of NIR in the canopy layers
  radiation_distribution (No, LAI, ACRef_NIR, global_radiation,
			  ACExt_NIR, total_NIR, NIRinSi);

}

static struct RaddistStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RaddistStandard (al); }
  RaddistStandardSyntax ()
    : DeclareModel (Raddist::component, "default", 
	       "Default model of radiation distribution in the canopy.")
  { }
  void load_frame (Frame&) const
  { }
} RaddistStandard_syntax;

// raddist_std.C ends here.
