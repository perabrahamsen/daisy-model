// chemistry_std.C -- Default model for pesticides and other chemicals.
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
#include "chemistry.h"
#include "chemical.h"
#include "reaction.h"
#include "movement.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "block.h"
#include "log.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "vcheck.h"

struct ChemistryStandard : public Chemistry
{
  // Parameters.
  auto_vector<Chemical*> chemicals;
  auto_vector<Reaction*> reactions;

  // Query.
  bool know (symbol chem) const;
  Chemical& find (symbol chem); 
  const std::vector<Chemical*>& all () const;
 
  // Management.
  void deposit (symbol chem, double amount, double dt, Treelog&);
  void spray (symbol chem, double amount, double dt, Treelog&);
  void dissipate (symbol chem, double amount  /* [g/m^2] */,
		  double dt /* [h] */, Treelog&);
  void harvest (double removed, double surface, double dt);
  void mix (const Geometry&, const Soil&, const SoilWater&, 
            double from, double to, double dt);
  void swap (const Geometry&, const Soil&, const SoilWater&,
	     double from, double middle, double to, double dt);
  void incorporate (const Geometry& geo,
		    const symbol chem, const double amount,
		    const double from, const double to, 
		    const double dt, Treelog& msg);
  
  // Simulation.
  void tick_top (const double snow_leak_rate, // [h^-1]
                 const double cover, // [],
                 const double canopy_leak_rate, // [h^-1]
                 double surface_runoff_rate /* [h^-1] */,
                 const double dt, // [h]
		 Treelog&);
  void infiltrate (const Geometry&, 
                   double infiltration /* [mm/h] */, double ponding /* [mm] */,
                   double R_mixing /* [h/mm] */, const double dt /* [h] */);
  void tick_soil (const Geometry& geo, double ponding /* [mm] */,
                  double R_mixing /* [h/mm] */, 
                  const Soil&, const SoilWater&, const SoilHeat&, Movement&,
                  const OrganicMatter&, Chemistry&, const bool flux_below, 
		  double dt, const Scope&, Treelog&);
  void clear ();
  void output (Log&) const;

  // Create & Destroy.
  void initialize (const AttributeList&, const Geometry& geo,
                   const Soil&, const SoilWater&, const SoilHeat&, Treelog&);
  bool check (const Geometry&,
	      const Soil&, const SoilWater&, const SoilHeat&, const Chemistry&,
	      const Scope& scope, Treelog&) const;
  explicit ChemistryStandard (Block& al);
};

bool
ChemistryStandard::know (const symbol chem) const
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      return true;

  return false;
}

Chemical& 
ChemistryStandard::find (symbol chem)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      return *chemicals[c];

  daisy_notreached ();
}

const std::vector<Chemical*>& 
ChemistryStandard::all () const
{ return chemicals; }

void 
ChemistryStandard::deposit (const symbol chem, 
			    const double amount, const double dt, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      {
        chemicals[c]->deposit (amount, dt);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::spray (const symbol chem, 
                          const double amount, const double dt, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      {
        chemicals[c]->spray (amount, dt);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::dissipate (const symbol chem, const double amount,
			      const double dt, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      {
        chemicals[c]->dissipate (amount, dt);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void
ChemistryStandard::harvest (const double removed, const double surface, 
                            const double dt)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->harvest (removed, surface, dt);
}

void 
ChemistryStandard::mix (const Geometry& geo, const Soil& soil, 
                        const SoilWater& soil_water,
                        const double from, const double to, const double dt)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->mix (geo, soil, soil_water, from, to, dt); 
}

void 
ChemistryStandard::swap (const Geometry& geo,
                         const Soil& soil, const SoilWater& soil_water,
                         const double from, const double middle,
                         const double to, const double dt)
{ 
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->swap (geo, soil, soil_water, from, middle, to, dt); 
}

void 
ChemistryStandard::incorporate (const Geometry& geo,
				const symbol chem, const double amount,
				const double from, const double to, 
				const double dt, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      {
        chemicals[c]->incorporate (geo, amount, from, to, dt);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::tick_top (const double snow_leak_rate, // [h^-1]
                             const double cover, // [],
                             const double canopy_leak_rate, // [h^-1]
                             double surface_runoff_rate /* [h^-1] */,
                             const double dt, // [h]
			     Treelog& msg) 
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick_top (snow_leak_rate, cover, canopy_leak_rate, 
                            surface_runoff_rate, dt, msg);
}

void 
ChemistryStandard::infiltrate (const Geometry& geo,
                               const double ponding, const double infiltration,
                               const double R_mixing,
                               const double dt)
{ 
  const double old_pond = ponding + infiltration * dt;
  const double fraction = old_pond > 0.0
    ? infiltration * dt / old_pond : 0.0;
  const double rate = fraction / dt;
  for (size_t c = 0; c < chemicals.size (); c++)
    {
      chemicals[c]->infiltrate (std::max (0.0, rate), dt);
      chemicals[c]->mixture (geo, std::max (0.0, ponding), R_mixing, dt);
    }
}

void 
ChemistryStandard::tick_soil (const Geometry& geo, const double ponding,
                              const double R_mixing,
                              const Soil& soil, const SoilWater& soil_water,
                              const SoilHeat& soil_heat, Movement& movement,
                              const OrganicMatter& organic_matter,
			      Chemistry& chemistry,
			      const bool flux_below, 
                              const double dt, const Scope& scope, Treelog& msg)
{ 
  Treelog::Open nest (msg, "Chemistry: " + name + ": tick soil");
  infiltrate (geo, ponding, soil_water.infiltration (geo), R_mixing, dt);

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->uptake (soil, soil_water, dt); 

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->decompose (geo, soil, soil_water, soil_heat, organic_matter,
                             dt); 

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->tick (geo, soil, soil_water, soil_heat, organic_matter, 
			chemistry, dt, msg);

  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick_soil (cell_size, soil_water, dt, scope, msg);

  for (size_t c = 0; c < chemicals.size (); c++)
    {
      // [g/m^2/h down -> g/cm^2/h up]
      const double J_in = -chemicals[c]->down () / (100.0 * 100.0);
      movement.solute (soil, soil_water, J_in, *chemicals[c], flux_below, 
		       dt, scope, msg); 
    }
}

void
ChemistryStandard::clear ()
{ 
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->clear (); 
}

void 
ChemistryStandard::output (Log& log) const
{
  Chemistry::output (log);
  output_list (chemicals, "trace", log, Chemical::component);
  output_list (reactions, "reaction", log, Reaction::component);
}

void 
ChemistryStandard::initialize (const AttributeList& al,
                               const Geometry& geo,
                               const Soil& soil, 
                               const SoilWater& soil_water,
			       const SoilHeat& soil_heat,
			       Treelog& msg)
{
  const std::vector<const AttributeList*>& alists = al.alist_sequence ("trace");
  daisy_assert (alists.size () == chemicals.size ());
  
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->initialize (*alists[c], geo, soil, soil_water, soil_heat, 
			      msg);

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->initialize (soil, msg);
}

bool 
ChemistryStandard::check (const Geometry& geo,
			  const Soil& soil, const SoilWater& soil_water,
			  const SoilHeat& soil_heat, const Chemistry& chemistry,
			  const Scope& scope, Treelog& msg) const
{ 
  bool ok = true; 
  for (size_t c = 0; c < chemicals.size (); c++)
    {
      Treelog::Open nest (msg, "Chemical: '" + chemicals[c]->name  + "'");
      if (!chemicals[c]->check (geo, soil, soil_water, scope, msg))
	ok = false;
    }

  for (size_t r = 0; r < reactions.size (); r++)
    {
      Treelog::Open nest (msg, "Reaction: '" + reactions[r]->name  + "'");
      if (!reactions[r]->check (soil, soil_water, soil_heat, chemistry, msg))
	ok = false;
    }

  return ok;
}

ChemistryStandard::ChemistryStandard (Block& al)
  : Chemistry (al),
    chemicals (Librarian::build_vector<Chemical> (al, "trace")),
    reactions (Librarian::build_vector<Reaction> (al, "reaction"))
{ }

static struct ChemistryStandardSyntax
{
  static Model& make (Block& al)
  { return *new ChemistryStandard (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  static void load_N (Syntax& syntax, AttributeList& alist);
  static void build_default ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    load_syntax (syntax, alist);
    Librarian::add_type (Chemistry::component, "default", alist, syntax, &make);
  }
  static void build_N ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    load_N (syntax, alist);
    alist.add ("type", "default");
    Librarian::add_type (Chemistry::component, "N", alist, syntax, &make);
  }
  ChemistryStandardSyntax ()
  { 
    build_default ();
    build_N ();
  }
} ChemistryStandard_syntax;

void
ChemistryStandardSyntax::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Chemistry::load_syntax (syntax, alist);

  syntax.add_object ("trace", Chemical::component, 
                     Syntax::State, Syntax::Sequence, "\
List of chemicals you want to trace in the simulation.");
  syntax.add_check ("trace", VCheck::unique ());
  alist.add ("trace", std::vector<const AttributeList*> ());
  syntax.add_object ("reaction", Reaction::component, 
                     Syntax::State, Syntax::Sequence, "\
List of chemical reactions you want to simulate.");
  alist.add ("reaction", std::vector<const AttributeList*> ());
}

void
ChemistryStandardSyntax::load_N (Syntax& syntax, AttributeList& alist)
{ 
  load_syntax (syntax, alist);
  alist.add ("description", "Inorganic nitrogen.");
  std::vector<const AttributeList*> trace;
  trace.push_back (&Chemical::NO3_model ());
  trace.push_back (&Chemical::NH4_model ());
  alist.add ("trace", trace);
  std::vector<const AttributeList*> reaction;
  reaction.push_back (&Reaction::nitrification_model ());
  reaction.push_back (&Reaction::denitrification_model ());
  alist.add ("reaction", reaction);
}

const AttributeList& 
Chemistry::N_model ()
{ 
  static AttributeList alist;
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ChemistryStandardSyntax::load_N (dummy, alist);
      alist.add ("type", "N");
    }
  return alist;
}

// chemistry_std.C ends her.
