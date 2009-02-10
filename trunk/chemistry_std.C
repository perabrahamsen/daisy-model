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
#include "treelog.h"
#include "frame.h"

struct ChemistryStandard : public Chemistry
{
  // Parameters.
  auto_vector<Chemical*> chemicals;
  auto_vector<Reaction*> reactions;

  // Query.
  bool know (symbol chem) const;
  bool ignored (symbol chem) const;
  Chemical& find (symbol chem); 
  const std::vector<Chemical*>& all () const;
 
  // Management.
  void deposit (symbol chem, double amount, double dt, Treelog&);
  void spray (symbol chem, double amount, double dt, Treelog&);
  void dissipate (symbol chem, double amount  /* [g/m^2] */,
		  double dt /* [h] */, Treelog&);
  void harvest (double removed, double surface, double dt);
  void mix (const Geometry&, const Soil&, const SoilWater&, 
            double from, double to, double penetration, double dt);
  void swap (const Geometry&, const Soil&, const SoilWater&,
	     double from, double middle, double to, double dt);
  void incorporate (const Geometry& geo,
		    const symbol chem, const double amount,
		    const double from, const double to, 
		    const double dt, Treelog& msg);
  void incorporate (const Geometry& geo,
		    const symbol chem, const double amount,
                    const Volume&, const double dt, Treelog& msg);
  
  // Simulation.
  void tick_top (const double snow_leak_rate, // [h^-1]
                 const double cover, // [],
                 const double canopy_leak_rate, // [h^-1]
                 const double surface_runoff_rate, // [h^-1]
                 const double direct_rain, // [mm/h]
                 const double dt, // [h]
		 Treelog&);
  void infiltrate (const Geometry&, 
                   double infiltration /* [mm/h] */, double ponding /* [mm] */,
                   double R_mixing /* [h/mm] */, const double dt /* [h] */);
  void tick_soil (const Scope&, const Geometry& geo, double ponding /* [mm] */,
                  double R_mixing /* [h/mm] */, 
                  const Soil&, const SoilWater&, const SoilHeat&, Movement&,
                  const OrganicMatter&, Chemistry&, 
		  double dt, Treelog&);
  void clear ();
  void output (Log&) const;

  // Create & Destroy.
  void initialize (const Scope& scope, const Geometry& geo,
                   const Soil&, const SoilWater&, const SoilHeat&, Treelog&);
  bool check (const Scope& scope, const Geometry&,
	      const Soil&, const SoilWater&, const SoilHeat&, const Chemistry&,
	      Treelog&) const;
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

bool 
ChemistryStandard::ignored (symbol chem) const
{ return false; }

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
                        const double from, const double to,
                        const double penetration, const double dt)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->mix (geo, soil, soil_water, from, to, penetration, dt); 
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
ChemistryStandard::incorporate (const Geometry& geo,
				const symbol chem, const double amount,
                                const Volume& volume,
				const double dt, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      {
        chemicals[c]->incorporate (geo, amount, volume, dt);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::tick_top (const double snow_leak_rate, // [h^-1]
                             const double cover, // [],
                             const double canopy_leak_rate, // [h^-1]
                             const double surface_runoff_rate, // [h^-1]
                             const double direct_rain, // [mm/h]
                             const double dt, // [h]
			     Treelog& msg) 
{
  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->tick_top  (direct_rain, *this, dt, msg);

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
ChemistryStandard::tick_soil (const Scope& scope, 
                              const Geometry& geo, const double ponding,
                              const double R_mixing,
                              const Soil& soil, const SoilWater& soil_water,
                              const SoilHeat& soil_heat, Movement& movement,
                              const OrganicMatter& organic_matter,
			      Chemistry& chemistry,
                              const double dt, Treelog& msg)
{ 
  Treelog::Open nest (msg, "Chemistry: " + name + ": tick soil");
  infiltrate (geo, ponding, soil_water.infiltration (geo), R_mixing, dt);

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->uptake (soil, soil_water, dt); 

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->decompose (geo, soil, soil_water, soil_heat, organic_matter,
                             chemistry, dt, msg); 

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->tick (units,
                        geo, soil, soil_water, soil_heat, organic_matter, 
			chemistry, dt, msg);

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick_soil (units, geo, soil, soil_water, dt, scope, msg);

  for (size_t c = 0; c < chemicals.size (); c++)
    {
      Treelog::Open nest (msg, "Chemical: " 
                          + chemicals[c]->name + ": transport");
      // [g/m^2/h down -> g/cm^2/h up]
      const double J_above = -chemicals[c]->down () / (100.0 * 100.0);
      movement.solute (soil, soil_water, J_above, *chemicals[c], 
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
ChemistryStandard::initialize (const Scope& scope, 
                               const Geometry& geo,
                               const Soil& soil, 
                               const SoilWater& soil_water,
			       const SoilHeat& soil_heat,
			       Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->initialize (units, scope, geo, soil, soil_water, soil_heat, 
			      msg);

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->initialize (units, geo, soil, soil_water, soil_heat, msg);
}

bool 
ChemistryStandard::check (const Scope& scope,
                          const Geometry& geo,
			  const Soil& soil, const SoilWater& soil_water,
			  const SoilHeat& soil_heat, const Chemistry& chemistry,
			  Treelog& msg) const
{ 
  bool ok = true; 
  for (size_t c = 0; c < chemicals.size (); c++)
    {
      Treelog::Open nest (msg, "Chemical: '" + chemicals[c]->name  + "'");
      if (!chemicals[c]->check (units, scope, 
                                geo, soil, soil_water, chemistry, msg))
	ok = false;
    }

  for (size_t r = 0; r < reactions.size (); r++)
    {
      Treelog::Open nest (msg, "Reaction: '" + reactions[r]->name  + "'");
      if (!reactions[r]->check (units, geo, soil, soil_water, soil_heat,
                                chemistry, msg))
	ok = false;
    }

  return ok;
}

ChemistryStandard::ChemistryStandard (Block& al)
  : Chemistry (al),
    chemicals (Librarian::build_vector<Chemical> (al, "trace")),
    reactions (Librarian::build_vector<Reaction> (al, "reaction"))
{ }

static struct ChemistryStandardSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ChemistryStandard (al); }
  ChemistryStandardSyntax ()
    : DeclareModel (Chemistry::component, "default", "\
Handle chemicals and reactions.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add_object ("trace", Chemical::component, 
                      Value::State, Value::Sequence, "\
List of chemicals you want to trace in the simulation.");
    frame.add_check ("trace", VCheck::unique ());
    frame.add_empty ("trace");
    frame.add_object ("reaction", Reaction::component, 
                      Value::State, Value::Sequence, "\
List of chemical reactions you want to simulate.");
    frame.add_empty ("reaction");
  }
} ChemistryStandard_syntax;

static struct ChemistryNitrogenSyntax : public DeclareParam
{
  ChemistryNitrogenSyntax ()
    : DeclareParam (Chemistry::component, "N", "default", "\
Inorganic nitrogen.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_strings ("trace", "NO3", "NH4");
    frame.add_strings ("reaction", "nitrification", "denitrification");
  }
} ChemistryNitrogen_syntax;

// chemistry_std.C ends her.
