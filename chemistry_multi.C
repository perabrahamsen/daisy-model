// chemistry_multi.C -- Default model for pesticides and other chemicals.
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
#include "log.h"
#include "block_model.h"
#include "treelog.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "vcheck.h"
#include "check.h"
#include "frame.h"
#include "mathlib.h"
#include <map>
#include <sstream>
 
struct ChemistryMulti : public Chemistry
{
  // Parameters.
  auto_vector<Chemistry*> combine;
  std::vector<symbol> ignore;
  const double max_sink_total;     // []
  const double max_sink_solute;    // []
  const double max_sink_secondary; // []
  const double min_sink_total;     // []

  // Cache.
  const std::vector<Chemical*> chemicals;

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
  void check_ignore (const symbol chem, Treelog& msg);
  void update_C (const Soil&, const SoilWater&);
  void mass_balance (const Geometry& geo, 
                     const SoilWater& soil_water) const;
  void deposit (symbol chem, double flux, Treelog&);
  void spray_overhead (symbol chem, double amount, Treelog&);
  void spray_surface (symbol chem, double amount, Treelog&);
  void dissipate_surface (symbol chem, double amount, Treelog&);
  void harvest (double removed, double surface);
  void mix (const Geometry&, const Soil&, const SoilWater&, 
            double from, double to, double penetration);
  void swap (const Geometry&, const Soil&, const SoilWater&,
	     double from, double middle, double to);
  void incorporate (const Geometry& geo,
		    const symbol chem, const double amount,
		    const double from, const double to, Treelog& msg);
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
  double find_dt (double S, double C, 
                  double M_secondary, double M_solute, double M_total) const;
  double suggest_dt () const;
  void tick_top (const Geometry&, const Soil&, 
                 const SoilWater&, const SoilHeat&, 
                 const double tillage_age /* [d] */,
                 const Surface&,
		 const Vegetation& vegetation,
		 const Bioclimate& bioclimate,
		 const Litter& litter, 
		 const double surface_runoff_rate, // [h^-1]
		 const double surface_water /* [mm] */,
		 const double total_rain /* [mm/h] */,
                 OrganicMatter&, Chemistry& chemistry, 
                 const double dt, // [h]
		 Treelog&);
  void infiltrate (const Geometry&, 
                   double infiltration /* [mm/h] */, double ponding /* [mm] */,
                   double R_mixing /* [h/mm] */, const double dt /* [h] */);
  void tick_soil (const Scope&, const Geometry& geo, double ponding /* [mm] */,
                  double R_mixing /* [h/mm] */, 
                  const Soil&, const SoilWater&, const SoilHeat&, Movement&,
                  OrganicMatter&, Chemistry&, 
		  double dt, Treelog&);
  void clear ();
  void output (Log&) const;

  // Create & Destroy.
  void initialize (const Scope&, const Geometry&,
                   const Soil&, const SoilWater&, const SoilHeat&, 
                   const OrganicMatter&, const Surface&, Treelog&);
  bool check (const Scope&, const Geometry&, 
	      const Soil&, const SoilWater&, const SoilHeat&,
	      const OrganicMatter&, const Chemistry&,
	      Treelog&) const;
  static const std::vector<Chemical*> 
  /**/ find_chemicals (const std::vector<Chemistry*>& combine);
  explicit ChemistryMulti (const BlockModel& al);
};

bool 
ChemistryMulti::ignored (symbol chem) const
{
  for (size_t i = 0; i < ignore.size (); i++)
    if (ignore[i] == chem)
      return true;

  return false;
}

bool
ChemistryMulti::know (const symbol chem) const
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      return true;

  return false;
}

Chemical& 
ChemistryMulti::find (symbol chem)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      return *chemicals[c];

  daisy_notreached ();
}

const Chemical& 
ChemistryMulti::find (symbol chem) const
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->objid == chem)
      return *chemicals[c];

  daisy_notreached ();
}

const std::vector<Chemical*>& 
ChemistryMulti::all () const
{ return chemicals; }

void 
ChemistryMulti::sorption_table (const Soil& soil, const size_t cell, 
                                const double Theta, const double start,
                                const double factor, const int intervals,
                                Treelog& msg) const
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->sorption_table (soil, cell, Theta, start, factor, intervals,
                                msg); 
}

void 
ChemistryMulti::check_ignore (const symbol chem, Treelog& msg)
{
  if (ignored (chem))
    return;
  
  msg.message ("Fate of '" + chem.name () + "' will not be traced");
  ignore.push_back (chem);
}

void 
ChemistryMulti::update_C (const Soil& soil, const SoilWater& soil_water)
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->update_C (soil, soil_water); 
}

void 
ChemistryMulti::mass_balance (const Geometry& geo, 
                              const SoilWater& soil_water) const
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->mass_balance (geo, soil_water); 
}

void 
ChemistryMulti::deposit (const symbol chem, const double flux, Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.deposit (flux);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::spray_overhead (const symbol chem, const double amount,
                                Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.spray_overhead (amount);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::spray_surface (const symbol chem, const double amount,
                               Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.spray_surface (amount);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::dissipate_surface (const symbol chem, const double amount, 
                                   Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.dissipate_surface (amount);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void
ChemistryMulti::harvest (const double removed, const double surface)
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->harvest (removed, surface);
}

void 
ChemistryMulti::mix (const Geometry& geo, const Soil& soil, 
                     const SoilWater& soil_water,
                     const double from, const double to, 
                     const double penetration)
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->mix (geo, soil, soil_water, from, to, penetration); 
}

void 
ChemistryMulti::swap (const Geometry& geo,
                      const Soil& soil, const SoilWater& soil_water,
                      const double from, const double middle,
                      const double to)
{ 
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->swap (geo, soil, soil_water, from, middle, to); 
}

void 
ChemistryMulti::incorporate (const Geometry& geo,
			     const symbol chem, const double amount,
			     const double from, const double to, 
			     Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.incorporate (geo, amount, from, to);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::incorporate (const Geometry& geo,
			     const symbol chem, const double amount,
                             const Volume& volume,
			     Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.incorporate (geo, amount, volume);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::remove_solute (const symbol chem)
{
  for (auto c : combine)
    c->remove_solute (chem);
}

double				// [g/m^2]
ChemistryMulti::total_content (const Geometry& geo, const symbol chem) const
{
  double total = 0.0;
  for (auto c : combine)
    total += c->total_content (geo, chem);
  return total;
}

void 
ChemistryMulti::tick_source (const Scope& scope, const Geometry& geo,
                             const Soil& soil, const SoilWater& soil_water, 
                             const SoilHeat& soil_heat, 
                             const OrganicMatter& organic, 
                             const Chemistry& chemistry, Treelog& msg)
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->tick_source (scope, geo, soil, soil_water, soil_heat, 
                             organic, chemistry, msg);
}

double 
ChemistryMulti::find_dt (const double S, const double C, 
                         const double M_secondary, const double M_solute,
                         const double M_total) const
{
  double min_M = std::min (max_sink_solute * M_solute,
                           max_sink_total * M_total);

  if (M_secondary > 0.0)
    min_M = std::min (min_M, max_sink_secondary * M_secondary);

  if (!std::isnormal (S) || !std::isnormal (C))
    return 0.0;
  
  const double dt = std::max (min_sink_total * M_total, min_M) / (S * C);
  daisy_assert (std::isnormal (dt));
  if (dt < 0.0)
    return 0.0;

  return dt;
}

double 
ChemistryMulti::suggest_dt () const
{
  double dt = 0.0;
  for (size_t c = 0; c < combine.size (); c++)
    {
      const double chem_dt = combine[c]->suggest_dt ();
      if (std::isnormal (chem_dt)
          && (!std::isnormal (dt) || chem_dt < dt))
        dt = chem_dt;
    }
  return dt;
}


void 
ChemistryMulti::tick_top (const Geometry& geo, 
                          const Soil& soil, const SoilWater& soil_water, 
                          const SoilHeat& soil_heat, 
                          const double tillage_age /* [d] */,
                          const Surface& surface,
			  const Vegetation& vegetation,
			  const Bioclimate& bioclimate,
			  const Litter& litter, 
			  const double surface_runoff_rate, // [h^-1]
			  const double surface_water /* [mm] */,
			  const double total_rain /* [mm/h] */,
                          OrganicMatter& organic, Chemistry& chemistry, 
                          const double dt, // [h]
                          Treelog& msg) 
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->tick_top (geo, soil, soil_water, soil_heat,
                          tillage_age, surface, vegetation, bioclimate,
                          litter, surface_runoff_rate, surface_water,
                          total_rain, 
                          organic, chemistry, dt, msg);
}

void 
ChemistryMulti::tick_soil (const Scope& scope, 
                           const Geometry& geo, const double ponding,
			   const double R_mixing,
			   const Soil& soil, const SoilWater& soil_water,
			   const SoilHeat& soil_heat, Movement& movement,
			   OrganicMatter& organic_matter,
			   Chemistry& chemistry, 
			   const double dt, Treelog& msg)
{ 
  TREELOG_MODEL (msg);

  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->tick_soil (scope, geo, ponding, R_mixing, 
			   soil, soil_water, soil_heat, movement, 
			   organic_matter, chemistry, dt, msg);
}

void
ChemistryMulti::clear ()
{ 
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->clear (); 
}

void 
ChemistryMulti::output (Log& log) const
{
  static const symbol chemistry_lib (Chemistry::component);
  static const symbol chemical_lib (Chemical::component);
  Chemistry::output (log);
  output_list (combine, "combine", log, chemistry_lib);
  output_list (chemicals, "trace", log, chemical_lib);

  // We can't log identifier_sequence yet.
#if 0
  output_variable (ignore, log);
#endif
}

void 
ChemistryMulti::initialize (const Scope& scope, 
			    const Geometry& geo,
			    const Soil& soil, 
			    const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    const OrganicMatter& organic,
			    const Surface& surface, Treelog& msg)
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->initialize (scope, geo, soil, soil_water, soil_heat,
			    organic, surface, msg);
}

bool 
ChemistryMulti::check (const Scope& scope, const Geometry& geo,
		       const Soil& soil, const SoilWater& soil_water,
		       const SoilHeat& soil_heat,
		       const OrganicMatter& organic, const Chemistry& chemistry,
		       Treelog& msg) const
{ 
  bool ok = true; 
  for (size_t c = 0; c < combine.size (); c++)
    {
      Treelog::Open nest (msg, "Chemistry: '" + combine[c]->objid  + "'");
      if (!combine[c]->check (scope, geo, soil, soil_water, soil_heat,
                              organic, chemistry, msg))
	ok = false;
    }

  // Check for duplicate chemicals.
  std::map<symbol, size_t> found;
  for (size_t i = 0; i < chemicals.size (); i++)
    {
      const symbol type = chemicals[i]->objid;
      std::map<symbol, size_t>::const_iterator f = found.find (type);
      if (f != found.end ())
	{
	  std::ostringstream tmp;
	  tmp << "Chemical '" << type << "' definded in multiple chemistries:";
	  for (size_t j = 0; j < combine.size (); j++)
	    if (combine[j]->know (type))
	      tmp << " '" << combine[j]->objid << "'";
	  msg.error (tmp.str ());
	  ok = false;
	}
      found[type] = i;
    }
  return ok;
}

const std::vector<Chemical*> 
ChemistryMulti::find_chemicals (const std::vector<Chemistry*>& combine)
{
  std::vector<Chemical*> result;
  for (size_t i = 0; i < combine.size (); i++)
    {
      const std::vector<Chemical*>& all = combine[i]->all ();
      for (size_t j =  0; j < all.size (); j++)
	result.push_back (all[j]);
    }
  return result;
}

ChemistryMulti::ChemistryMulti (const BlockModel& al)
  : Chemistry (al),
    combine (Librarian::build_vector<Chemistry> (al, "combine")),
    ignore (al.name_sequence ("ignore")),
    max_sink_total (al.number ("max_sink_total")),
    max_sink_solute (al.number ("max_sink_solute")),
    max_sink_secondary (al.number ("max_sink_secondary")),
    min_sink_total (al.number ("min_sink_total")),
    chemicals (find_chemicals (combine))
{ }

static struct ChemistryMultiSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ChemistryMulti (al); }
  ChemistryMultiSyntax ()
    : DeclareModel (Chemistry::component, "multi", "Handle multile chemistries.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("combine", Chemistry::component, 
                          Attribute::State, Attribute::Variable, "\
List of chemistry parameterizations you want to combine.");
    frame.declare_string ("ignore", Attribute::State, Attribute::Variable,
                          "Don't warn when spraying one of these chemicals.\n\
The first time an untraced chemical not on the list is sprayed on the\n\
field, Daisy will issue a warning and add the chemical to this list.");
    frame.set_check ("ignore", VCheck::unique ());
    frame.set_empty ("ignore");
    frame.declare_object ("trace", Chemical::component, 
                          Attribute::LogOnly, Attribute::Variable, "\
List of chemicals in nested chemistries.");
    frame.declare ("max_sink_total", Attribute::None (), Check::positive (), 
                   Attribute::Const, "\
Maximum allowed sink term as a fraction of total content.\n\
\n\
If variable timesteps are enabled, Daisy will try to scale down the\n\
timestep in order to ensure that no more than this fraction of the\n\
total content is removed by drains or biopores within the timestep.");
    frame.set ("max_sink_total", 0.5);
    frame.declare ("max_sink_solute", Attribute::None (), Check::positive (), 
                   Attribute::Const, "\
Maximum allowed sink term as a fraction of solute content.\n\
\n\
If variable timesteps are enabled, Daisy will try to scale down the\n\
timestep in order to ensure that no more than this fraction of the\n\
solute content is removed by drains or biopores within the timestep.");
    frame.set ("max_sink_solute", 0.9);
    frame.declare ("max_sink_secondary", Attribute::None (), Check::positive (), 
                   Attribute::Const, "\
Maximum allowed sink term as a fraction of secondary domain content.\n\
\n                                                                    \
If variable timesteps are enabled, Daisy will try to scale down the\n\
timestep in order to ensure that no more than this fraction of the\n\
secondary domain content is removed by drains or biopores within the\n\
timestep.  This should usually be above 1 to allow for the case where\n\
the secondary domain is emptied within a timestep.");
    frame.set ("max_sink_secondary", 1.5);
    
    frame.declare ("min_sink_total", Attribute::None (), Check::positive (), 
                   Attribute::Const, "\n\
Always allow this fraction of total content to be removed by sink term.\n\
\n\
This overwrites all the 'max_sink' parameters.");
    frame.set ("min_sink_total", 0.01);
  }
} ChemistryMulti_syntax;

static struct ChemistryNutrientSyntax : public DeclareParam
{ 
  ChemistryNutrientSyntax ()
    : DeclareParam (Chemistry::component, "nutrient", "multi", "\
Include 'N' chemistry so organic matter and plants will work.")
  { }
  void load_frame (Frame& frame) const
  { frame.set_strings ("combine", "N"); }
} ChemistryNutrient_syntax;

static struct ChemistryNoneSyntax : public DeclareParam
{ 
  ChemistryNoneSyntax ()
    : DeclareParam (Chemistry::component, "none", "multi", "\
No active chemistries.")
  { }
  void load_frame (Frame& frame) const
  { frame.set_empty ("combine"); }
} ChemistryNone_syntax;

// chemistry_multi.C ends here
