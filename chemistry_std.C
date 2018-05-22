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
#include "surface.h"
#include "block_model.h"
#include "log.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "vcheck.h"
#include "treelog.h"
#include "frame.h"
#include "mathlib.h"

struct ChemistryStandard : public Chemistry
{
  // Parameters.
  auto_vector<Chemical*> chemicals;
  auto_vector<Reaction*> reactions;

  // Query.
  bool know (symbol chem) const;
  bool ignored (symbol chem) const;
  Chemical& find (symbol chem); 
  const Chemical& find (symbol chem) const; 
  const std::vector<Chemical*>& all () const;
  void sorption_table (const Soil& soil, const size_t cell, 
                       const double Theta, const double start,
                       const double factor, const int intervals,
                       Treelog& msg) const;
 
  // Management.
  void update_C (const Soil& soil, const SoilWater& soil_water);
  void mass_balance (const Geometry& geo, const SoilWater& soil_water) const;
  void deposit (symbol chem, double flux, Treelog&);
  void spray_overhead (symbol chem, double amount, Treelog&);
  void spray_surface (symbol chem, double amount, Treelog&);
  void dissipate_surface (symbol chem, double amount  /* [g/m^2] */, Treelog&);
  void harvest (double removed, double surface);
  void mix (const Geometry&, const Soil&, const SoilWater&, 
            double from, double to, double penetration);
  void swap (const Geometry&, const Soil&, const SoilWater&,
	     double from, double middle, double to);
  void incorporate (const Geometry& geo,
		    const symbol chem, const double amount,
		    const double from, const double to, 
		    Treelog& msg);
  void incorporate (const Geometry& geo,
		    const symbol chem, const double amount,
                    const Volume&, Treelog& msg);
  void remove_solute (const symbol chem);
  double total_content (const Geometry&, const symbol chem) const; //[g/m^2]

  // Simulation.
  void tick_source (const Scope&, 
                    const Geometry&, const Soil&, const SoilWater&, 
                    const SoilHeat&, const OrganicMatter&, const Chemistry&, 
                    Treelog&);
  double find_dt (double, double, double, double, double) const
  { return 0.0; }
  double suggest_dt () const;
  void tick_top (const Units&, const Geometry&, const Soil&, 
                 const SoilWater&, const SoilHeat&, 
                 const double tillage_age /* [d] */,
		 const Surface&,
		 const Vegetation& vegetation,
		 const Bioclimate& bioclimate,
		 const double litter_cover, // [],
		 const double surface_runoff_rate, // [h^-1]
		 const double surface_water /* [mm] */,
		 const double total_rain /* [mm/h] */,
                 Chemistry& chemistry, 
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
                   const Soil&, const SoilWater&, const SoilHeat&, 
                   const Surface&, Treelog&);
  bool check (const Scope& scope, const Geometry&,
	      const Soil&, const SoilWater&, const SoilHeat&, const Chemistry&,
	      Treelog&) const;
  explicit ChemistryStandard (const BlockModel& al);
};

bool
ChemistryStandard::know (const symbol chem) const
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
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
    if (chemicals[c]->objid == chem)
      return *chemicals[c];

  daisy_panic ("Can't find chemical '" + chem + "'");
}

const Chemical& 
ChemistryStandard::find (symbol chem) const
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      return *chemicals[c];

  daisy_panic ("Can't find chemical '" + chem + "'");
}

const std::vector<Chemical*>& 
ChemistryStandard::all () const
{ return chemicals; }

void 
ChemistryStandard::sorption_table (const Soil& soil, const size_t cell, 
                                   const double Theta, const double start,
                                   const double factor, const int intervals,
                                   Treelog& msg) const
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->sorption_table (soil, cell, Theta, start, factor, intervals,
                                  msg); 
}

void 
ChemistryStandard::update_C (const Soil& soil, const SoilWater& soil_water)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->update_C (soil, soil_water); 
}

void 
ChemistryStandard::mass_balance (const Geometry& geo, 
                                 const SoilWater& soil_water) const
{
  const size_t cell_size = geo.cell_size ();

  for (size_t i = 0; i < chemicals.size (); i++)
    {
      const Chemical& sol = *chemicals[i];
      for (size_t c = 0; c < cell_size; c++)
        {
          const double Theta = soil_water.Theta_primary (c);
          const double C = sol.C_primary (c);
          const double M = sol.M_primary (c);
          const double A = M - Theta * C;
          if (A < 0.0)
            daisy_approximate (M,  C * Theta);
        }
    }
}

void 
ChemistryStandard::deposit (const symbol chem, const double flux, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      {
        chemicals[c]->deposit (flux);
        return;
      }
  msg.warning ("Unknown chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::spray_overhead (const symbol chem, 
                                   const double amount, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      {
        chemicals[c]->spray_overhead (amount);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::spray_surface (const symbol chem, 
                                  const double amount, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      {
        chemicals[c]->spray_surface (amount);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::dissipate_surface (const symbol chem, const double amount,
                                      Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      {
        chemicals[c]->dissipate_surface (amount);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void
ChemistryStandard::harvest (const double removed, const double surface)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->harvest (removed, surface);
}

void 
ChemistryStandard::mix (const Geometry& geo, const Soil& soil, 
                        const SoilWater& soil_water,
                        const double from, const double to,
                        const double penetration)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->mix (geo, soil, soil_water, from, to, penetration); 
}

void 
ChemistryStandard::swap (const Geometry& geo,
                         const Soil& soil, const SoilWater& soil_water,
                         const double from, const double middle, 
                         const double to)
{ 
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->swap (geo, soil, soil_water, from, middle, to); 
}

void 
ChemistryStandard::incorporate (const Geometry& geo,
				const symbol chem, const double amount,
				const double from, const double to, 
				Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      {
        chemicals[c]->incorporate (geo, amount, from, to);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::incorporate (const Geometry& geo,
				const symbol chem, const double amount,
                                const Volume& volume, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      {
        chemicals[c]->incorporate (geo, amount, volume);
        return;
      }
  msg.warning ("Unknwon chemical '" + chem + "' ignored");
}

void 
ChemistryStandard::remove_solute (const symbol chem)
{
  for (auto c : chemicals)
    if (c->objid == chem)
      {
	c->remove_all ();
	return;
      }
}

double				// [g/m^2]
ChemistryStandard::total_content (const Geometry& geo, const symbol chem) const
{
  double total = 0.0;
  for (auto c : chemicals)
    if (c->objid == chem)
      total += c->total_content (geo);
  return total;
}

void 
ChemistryStandard::tick_source (const Scope& scope, const Geometry& geo,
                                const Soil& soil, const SoilWater& soil_water, 
                                const SoilHeat& soil_heat, 
                                const OrganicMatter& organic, 
                                const Chemistry& chemistry, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick_source (scope, geo, soil, soil_water, soil_heat, 
                               organic, chemistry, msg);
}

double 
ChemistryStandard::suggest_dt () const
{
  double dt = 0.0;
  for (size_t c = 0; c < chemicals.size (); c++)
    {
      const double chem_dt = chemicals[c]->suggest_dt ();
      if (std::isnormal (chem_dt)
          && (!std::isnormal (dt) || chem_dt < dt))
        dt = chem_dt;
    }
  return dt;
}

void 
ChemistryStandard::tick_top (const Units& units, const Geometry& geo, 
                             const Soil& soil, const SoilWater& soil_water, 
                             const SoilHeat& soil_heat, 
                             const double tillage_age /* [d] */,
                             const Surface& surface,
			     const Vegetation& vegetation,
			     const Bioclimate& bioclimate,
                             const double litter_cover, // [],
                             const double surface_runoff_rate, // [h^-1]
                             const double surface_water /* [mm] */,
                             const double total_rain /* [mm/h] */,
                             Chemistry& chemistry,
                             const double dt, // [h]
			     Treelog& msg) 
{
  for (size_t r = 0; r < reactions.size (); r++)
    {
      reactions[r]->tick_top  (vegetation, bioclimate, tillage_age, 
                               total_rain, surface_water, chemistry, 
                               dt, msg);
      reactions[r]->tick_surface  (units, geo, soil, soil_water, soil_heat,
                                   surface, chemistry,  dt, msg);
    }
      

  const double z_mixing = surface.mixing_depth ();
  const double pond_rain = std::max (surface.ponding_average (), 0.0);
  for (size_t c = 0; c < chemicals.size (); c++)
    {
      chemicals[c]->tick_top (vegetation, bioclimate, 
                              litter_cover, surface_runoff_rate, dt, msg);
      chemicals[c]->tick_surface (pond_rain,
                                  geo, soil, soil_water, z_mixing, msg);
    }
}

void 
ChemistryStandard::infiltrate (const Geometry& geo,
                               const double ponding, const double infiltration,
                               const double R_mixing,
                               const double dt)
{ 
  const double old_pond = ponding + infiltration * dt;
  const double fraction = old_pond > 0.0
    ? infiltration * dt / old_pond 
    : 0.0;
  const double rate = infiltration * dt > 0.01 /* [mm] */
    ? fraction / dt
    : 0.0;
  for (size_t c = 0; c < chemicals.size (); c++)
    {
      chemicals[c]->infiltrate (std::max (0.0, rate), 
                                std::max (0.0, infiltration * dt),
                                dt);
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
  Treelog::Open nest (msg, "Chemistry: " + objid + ": tick soil");
  infiltrate (geo, ponding, soil_water.infiltration (geo), R_mixing, dt);

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->uptake (soil, soil_water, dt); 

  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->decompose (geo, soil, soil_water, soil_heat, organic_matter,
                             chemistry, dt, msg); 

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->tick_soil (units,
                             geo, soil, soil_water, soil_heat, organic_matter, 
                             chemistry, dt, msg);
  
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick_soil (units, geo, soil, soil_water, dt, scope, msg);

  for (size_t c = 0; c < chemicals.size (); c++)
    {
      Treelog::Open nest (msg, "Chemical: " 
                          + chemicals[c]->objid + ": transport");
      // [g/m^2/h down -> g/cm^2/h up]
      const double J_above = -chemicals[c]->down () / (100.0 * 100.0);
      movement.solute (soil, soil_water, J_above, *chemicals[c], 
		       dt, scope, msg); 
    }
  
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->tick_after (geo, msg);
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
  static const symbol chemical_lib (Chemical::component);
  static const symbol reaction_lib (Reaction::component);
  Chemistry::output (log);
  output_list (chemicals, "trace", log, chemical_lib);
  output_list (reactions, "reaction", log, reaction_lib);
}

void 
ChemistryStandard::initialize (const Scope& scope, 
                               const Geometry& geo,
                               const Soil& soil, 
                               const SoilWater& soil_water,
			       const SoilHeat& soil_heat,
			       const Surface& surface, Treelog& msg)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    chemicals[c]->initialize (units, scope, geo, soil, soil_water, soil_heat, 
			      msg);

  for (size_t r = 0; r < reactions.size (); r++)
    reactions[r]->initialize (units, geo, soil, soil_water, soil_heat, surface, 
                              msg);
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
      Treelog::Open nest (msg, "Chemical: '" + chemicals[c]->objid  + "'");
      if (!chemicals[c]->check (units, scope, 
                                geo, soil, soil_water, chemistry, msg))
	ok = false;
    }

  for (size_t r = 0; r < reactions.size (); r++)
    {
      Treelog::Open nest (msg, "Reaction: '" + reactions[r]->objid  + "'");
      if (!reactions[r]->check (units, geo, soil, soil_water, soil_heat,
                                chemistry, msg))
	ok = false;
    }

  return ok;
}

ChemistryStandard::ChemistryStandard (const BlockModel& al)
  : Chemistry (al),
    chemicals (Librarian::build_vector<Chemical> (al, "trace")),
    reactions (Librarian::build_vector<Reaction> (al, "reaction"))
{ }

static struct ChemistryStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ChemistryStandard (al); }
  ChemistryStandardSyntax ()
    : DeclareModel (Chemistry::component, "default", "\
Handle chemicals and reactions.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("trace", Chemical::component, 
                      Attribute::State, Attribute::Variable, "\
List of chemicals you want to trace in the simulation.");
    frame.set_check ("trace", VCheck::unique ());
    frame.set_empty ("trace");
    frame.declare_object ("reaction", Reaction::component, 
                      Attribute::State, Attribute::Variable, "\
List of chemical reactions you want to simulate.");
    frame.set_empty ("reaction");
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
    frame.set_strings ("trace", "NO3", "NH4");
    frame.set_strings ("reaction", "nitrification", "denitrification");
  }
} ChemistryNitrogen_syntax;

// chemistry_std.C ends her.
