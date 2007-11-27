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
  Chemical& soil_NH4_solute = chemistry.find (Chemical::NH4_solute ());
  Chemical& soil_NH4_sorbed = chemistry.find (Chemical::NH4_sorbed ());

  for (size_t i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4_sorbed.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4_solute.M_left (i, dt) >= 0.0);
    }

  daisy_assert (NH4.size () == cell_size);
  daisy_assert (N2O.size () == cell_size);
  daisy_assert (NO3.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      if (active[i])
        soil.nitrification (i, 
                            soil_NH4_sorbed.M (i) + soil_NH4_solute.M (i), 
			    soil_NH4_solute.C (i), 
                            soil_NH4_sorbed.M_left (i, dt)
			    + soil_NH4_solute.M_left (i, dt),
                            soil_water.h (i), soil_heat.T (i),
                            NH4[i], N2O[i], NO3[i], dt);
      else
        NH4[i] = N2O[i] = NO3[i] = 0.0;        
    }

  daisy_assert (NH4.size () == cell_size);
  std::vector<double> NH4_solute (cell_size, 0.0);
  std::vector<double> NH4_sorbed (cell_size, 0.0);
  
  for (size_t i = 0; i < cell_size; i++)
    {
      // We divide by content.  We really should take all from solute,
      // if using the "solute" nitrification model, bt we don't have
      // that informaiton here.
      const double solute_left = soil_NH4_solute.M_left (i, dt);
      const double sorbed_left = soil_NH4_sorbed.M_left (i, dt);
      const double total_left = solute_left + sorbed_left;
      const double solute_fraction = std::isnormal (total_left)
	? solute_left / total_left
	// Nothing?  Take from solute, and hope we get it!
	: 1.0;
      const double sorbed_fraction = 1.0 - solute_fraction;
      daisy_assert (sorbed_fraction >= 0.0);
      daisy_assert (sorbed_fraction <= 1.0);
      NH4_solute[i] = NH4[i] * solute_fraction;
      NH4_sorbed[i] = NH4[i] * sorbed_fraction;
    }
  
  soil_NH4_solute.add_to_transform_sink (NH4_solute, dt);
  soil_NH4_sorbed.add_to_transform_sink (NH4_sorbed, dt);
  soil_NO3.add_to_transform_source (NO3, dt);

  for (size_t i = 0; i < cell_size; i++)
    {
      daisy_assert (soil_NO3.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4_solute.M_left (i, dt) >= 0.0);
      daisy_assert (soil_NH4_sorbed.M_left (i, dt) >= 0.0);
    }
}

bool 
ReactionNitrification::check (const Soil&, const SoilWater&, const SoilHeat&,
			      const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.require (Chemical::NO3 (), msg))
    ok = false;
  if (!chemistry.require (Chemical::NH4_sorbed (), msg))
    ok = false;
  if (!chemistry.require (Chemical::NH4_solute (), msg))
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
