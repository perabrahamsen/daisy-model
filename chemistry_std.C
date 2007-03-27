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

struct ChemistryStandard : public Chemistry
{
  // Parameters.
  auto_vector<Chemical*> chemicals;
  auto_vector<Reaction*> reactions;

  // Query.
  bool know (symbol chem);
  Chemical& find (symbol chem);
  
  // Management.
  void spray (symbol chem, double amount, double dt);
  void harvest (double removed, double surface, double dt);
  void mix (const Geometry&, const Soil&, const SoilWater&, 
            double from, double to, double dt);
  void swap (const Geometry&, const Soil&, const SoilWater&,
	     double from, double middle, double to, double dt);
  
  // Simulation.
  void tick_top (const double snow_leak_rate, // [h^-1]
                 const double cover, // [],
                 const double canopy_leak_rate, // [h^-1]
                 double surface_runoff_rate /* [h^-1] */,
                 const double dt); // [h]
  void infiltrate (const Geometry&, 
                   double infiltration /* [mm/h] */, double ponding /* [mm] */,
                   double R_mixing /* [h/mm] */, const double dt /* [h] */);
  void tick_soil (const Geometry& geo, double ponding /* [mm] */,
                  double R_mixing /* [h/mm] */, 
                  const Soil&, const SoilWater&, const SoilHeat&, Movement&,
                  const OrganicMatter&, double dt, Treelog&);
  void clear ();
  void output (Log&) const;

  // Create & Destroy.
  void initialize (Block&, const AttributeList&, const Geometry& geo,
                   const Soil&, const SoilWater&);
  bool check (const Soil&, Treelog&) const;
  explicit ChemistryStandard (Block& al);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
};

bool
ChemistryStandard::know (const symbol chem)
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

void 
ChemistryStandard::spray (const symbol chem, 
                          const double amount, const double dt)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      chemicals[c]->spray (amount, dt);
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
ChemistryStandard::tick_top (const double snow_leak_rate, // [h^-1]
                             const double cover, // [],
                             const double canopy_leak_rate, // [h^-1]
                             double surface_runoff_rate /* [h^-1] */,
                             const double dt) // [h]
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick_top (snow_leak_rate, cover, canopy_leak_rate, 
                            surface_runoff_rate, dt);
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
                              const double dt, Treelog& msg)
{ 
  infiltrate (geo, ponding, soil_water.infiltration (geo), R_mixing, dt);

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->uptake (soil, soil_water, dt); 

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->decompose (geo, soil, soil_water, soil_heat, organic_matter,
                             dt); 

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->tick (soil, soil_water, *this, dt, msg);

  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick (cell_size, soil_water, dt);

  for (size_t c = 0; c < chemicals.size (); c++)
    {
      Treelog::Open nest (msg, name);
      // [g/m^2/h down -> g/cm^2/h up]
      const double J_in = -chemicals[c]->down () / (100.0 * 100.0);
      movement.solute (soil, soil_water, J_in, *chemicals[c], dt, msg); 
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
  output_list (chemicals, "trace", log, Librarian<Chemical>::library ());
  output_list (reactions, "reaction", log, Librarian<Reaction>::library ());
}

void 
ChemistryStandard::initialize (Block& block,
                               const AttributeList& al,
                               const Geometry& geo,
                               const Soil& soil, 
                               const SoilWater& soil_water)
{
  const std::vector<AttributeList*>& alists = al.alist_sequence ("trace");
  daisy_assert (alists.size () == chemicals.size ());
  
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->initialize (*alists[c], geo, soil, soil_water, block.msg ());

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->initialize (block, soil);
}

bool 
ChemistryStandard::check (const Soil& soil, Treelog& msg) const
{ 
  const size_t cell_size = soil.size ();

  bool ok = true; 
  for (size_t c = 0; c < chemicals.size (); c++)
    if (!chemicals[c]->check (cell_size, msg))
      ok = false;

  for (size_t r = 0; r < reactions.size (); r++)
    if (!reactions[r]->check (soil, msg))
      ok = false;

  return ok;
}

ChemistryStandard::ChemistryStandard (Block& al)
  : Chemistry (al),
    chemicals (Librarian<Chemical>::build_vector (al, "trace")),
    reactions (Librarian<Reaction>::build_vector (al, "reaction"))
{ }

void
ChemistryStandard::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("trace", Librarian<Chemical>::library (), 
              Syntax::State, Syntax::Sequence, "\
List of chemicals you want to trace in the simulation.");
  alist.add ("trace", std::vector<AttributeList*> ());
  syntax.add ("reaction", Librarian<Reaction>::library (), 
              Syntax::State, Syntax::Sequence, "\
List of chemical reactions you want to simulate.");
  alist.add ("reaction", std::vector<AttributeList*> ());
}

const AttributeList& 
Chemistry::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ChemistryStandard::load_syntax (dummy, alist);
      alist.add ("type", "default");
    }
  return alist;
}

static struct ChemistryStandardSyntax
{
  static Model& make (Block& al)
  { return *new ChemistryStandard (al); }
  ChemistryStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ChemistryStandard::load_syntax (syntax, alist);
    Librarian<Chemistry>::add_type ("default", alist, syntax, &make);
  }
} ChemistryStandard_syntax;
