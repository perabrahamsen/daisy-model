// reaction_nit.C -- Nitrification.
// 
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
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "chemistry.h"
#include "chemical.h"
#include "organic.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"
#include "treelog.h"

struct ReactionNitrification : public Reaction
{
  // Log variables.
  std::vector<double> NH4;
  std::vector<double> NO3;
  std::vector<double> N2O;

  // Output.
  void output (Log& log) const;

  // Simulation.
  void tick_soil (const Geometry& geo,
                  const Soil& soil, const SoilWater& soil_water, 
                  const SoilHeat& soil_heat,
                  OrganicMatter&, Chemistry& chemistry,
		  const double dt, Treelog& msg);

  // Create.
  bool check (const Geometry&, 
              const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const OrganicMatter&, const Chemistry& chemistry,
	      Treelog& msg) const;
  void initialize (const Geometry&, 
                   const Soil&, const SoilWater&, const SoilHeat&, 
                   const OrganicMatter&, const Surface&, Treelog&);
  explicit ReactionNitrification (const BlockModel& al);
};

void
ReactionNitrification::output (Log& log) const
{
  output_variable (NH4, log);
  output_variable (NO3, log);
  output_variable (N2O, log);
}

void 
ReactionNitrification::tick_soil (const Geometry& geo,
                                  const Soil& soil, const SoilWater& soil_water,
                                  const SoilHeat& soil_heat,
                                  OrganicMatter& organic_matter,
				  Chemistry& chemistry, 
                                  const double /* dt */, Treelog&)
{
  const size_t cell_size = geo.cell_size ();
  const std::vector<bool> active = organic_matter.active (); 
  Chemical& soil_NO3 = chemistry.find (Chemical::NO3 ());
  Chemical& soil_NH4 = chemistry.find (Chemical::NH4 ());

  daisy_assert (NH4.size () == cell_size);
  daisy_assert (N2O.size () == cell_size);
  daisy_assert (NO3.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      if (active[i])
        soil.nitrification (i, 
                            soil_NH4.M_primary (i), 
			    soil_NH4.C_primary (i), 
                            soil_water.h (i), soil_heat.T (i),
                            NH4[i], N2O[i], NO3[i]);
      else
        NH4[i] = N2O[i] = NO3[i] = 0.0;        
    }


  soil_NH4.add_to_transform_sink (NH4);
  soil_NO3.add_to_transform_source (NO3);
}

bool 
ReactionNitrification::check (const Geometry&,
                              const Soil&, const SoilWater&, const SoilHeat&,
			      const OrganicMatter&, const Chemistry& chemistry,
			      Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.know (Chemical::NO3 ()))
    {
      msg.error ("Nitrification requires NO3 to be tracked");
      ok = false;
    }
  if (!chemistry.know (Chemical::NH4 ()))
    {
      msg.error ("Nitrification requires NH4 to be tracked");
      ok = false;
    }

  return ok;
}

void
ReactionNitrification::initialize (const Geometry&,
                                   const Soil& soil, 
                                   const SoilWater&, const SoilHeat&,
                                   const OrganicMatter&, const Surface&,
				   Treelog&)
{
  const size_t cell_size = soil.size ();

  NH4 = std::vector<double> (cell_size, 0.0);
  NO3 = std::vector<double> (cell_size, 0.0);
  N2O = std::vector<double> (cell_size, 0.0);
}

ReactionNitrification::ReactionNitrification (const BlockModel& al)
  : Reaction (al)
{ }

static struct ReactionNitrificationSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionNitrification (al); }
  ReactionNitrificationSyntax ()
    : DeclareModel (Reaction::component, "nitrification", "Nitrification.\n\
The actual nitrification specification is part of the horizon models, this\n\
reaction just applies the models and logs the result. ")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare ("NH4", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, 
		"Amount of ammonium consumed this hour.");
    frame.declare ("NO3", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, 
		"Amount of nitrate generated this hour.");
    frame.declare ("N2O", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, 
		"Amount of nitrous oxide generated this hour.");


  }
} ReactionNitrification_syntax;

// reaction_nit.C ends here.
