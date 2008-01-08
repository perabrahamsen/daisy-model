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
#include "organic_matter.h"
#include "log.h"
#include "librarian.h"

struct ReactionNitrification : public Reaction
{
  // Log variables.
  std::vector<double> NH4;
  std::vector<double> NO3;
  std::vector<double> N2O;

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
  explicit ReactionNitrification (Block& al);
};

void
ReactionNitrification::output (Log& log) const
{
  output_variable (NH4, log);
  output_variable (NO3, log);
  output_variable (N2O, log);
}

void 
ReactionNitrification::tick (const Geometry& geo,
			     const Soil& soil, const SoilWater& soil_water,
			     const SoilHeat& soil_heat,
			     const OrganicMatter& organic_matter, 
			     Chemistry& chemistry, 
			     const double dt, Treelog&)
{
  const size_t cell_size = geo.cell_size ();
  const std::vector<bool> active = organic_matter.active (); 
  Chemical& soil_NO3 = chemistry.find (Chemical::NO3 ());
  Chemical& soil_NH4 = chemistry.find (Chemical::NH4 ());

  for (size_t i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4.M_left (i, dt) >= 0.0);
    }

  daisy_assert (NH4.size () == cell_size);
  daisy_assert (N2O.size () == cell_size);
  daisy_assert (NO3.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      if (active[i])
        soil.nitrification (i, 
                            soil_NH4.M (i), 
			    soil_NH4.C (i), 
                            soil_NH4.M_left (i, dt),
                            soil_water.h (i), soil_heat.T (i),
                            NH4[i], N2O[i], NO3[i], dt);
      else
        NH4[i] = N2O[i] = NO3[i] = 0.0;        
    }


  soil_NH4.add_to_transform_sink (NH4, dt);
  soil_NO3.add_to_transform_source (NO3, dt);

  for (size_t i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4.M_left (i, dt) >= 0.0);
    }
}

bool 
ReactionNitrification::check (const Soil&, const SoilWater&, const SoilHeat&,
			      const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.require (Chemical::NO3 (), msg))
    ok = false;
  if (!chemistry.require (Chemical::NH4 (), msg))
    ok = false;

  return ok;
}

void
ReactionNitrification::initialize (const Soil& soil, Treelog&)
{
  const size_t cell_size = soil.size ();

  NH4 = std::vector<double> (cell_size, 0.0);
  NO3 = std::vector<double> (cell_size, 0.0);
  N2O = std::vector<double> (cell_size, 0.0);
}

ReactionNitrification::ReactionNitrification (Block& al)
  : Reaction (al)
{ }

static struct ReactionNitrificationSyntax
{
  static Model& make (Block& al)
  { return *new ReactionNitrification (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    alist.add ("description", "Nitrification.\n\
The actual nitrification specification is part of the horizon models, this\n\
reaction just applies the models and logs the result. ");
    syntax.add ("NH4", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
		"Amount of ammonium consumed this hour.");
    syntax.add ("NO3", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
		"Amount of nitrate generated this hour.");
    syntax.add ("N2O", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
		"Amount of nitrous oxide generated this hour.");
  }
  ReactionNitrificationSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    load_syntax (syntax, alist);

    Librarian::add_type (Reaction::component, "nitrification",
			 alist, syntax, &make);
  }
} ReactionNitrification_syntax;

const AttributeList& 
Reaction::nitrification_model ()
{
  static AttributeList alist;
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ReactionNitrificationSyntax::load_syntax (dummy, alist);
      alist.add ("type", "nitrification");
    }
  return alist;
}

// reaction_nit.C ends here.
