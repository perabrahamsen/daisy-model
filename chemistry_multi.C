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
#include "block.h"
#include "syntax.h"
#include "treelog.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "vcheck.h"
#include <map>
#include <sstream>
 
struct ChemistryMulti : public Chemistry
{
  // Parameters.
  auto_vector<Chemistry*> combine;
  std::vector<symbol> ignore;
  
  // Cache.
  const std::vector<Chemical*> chemicals;

  // Query.
  bool know (symbol chem) const;
  bool ignored (symbol chem) const;
  Chemical& find (symbol chem);
  const std::vector<Chemical*>& all () const;
  
  // Management.
  void check_ignore (const symbol chem, Treelog& msg);
  void deposit (symbol chem, double amount, double dt, Treelog&);
  void spray (symbol chem, double amount, double dt, Treelog&);
  void dissipate (symbol chem, double amount, double dt, Treelog&);
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
  static const std::vector<Chemical*> 
  /**/ find_chemicals (const std::vector<Chemistry*>& combine);
  explicit ChemistryMulti (Block& al);
  static bool check_alist (const AttributeList& al, Treelog& msg);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
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
    if (chemicals[c]->name == chem)
      return true;

  return false;
}

Chemical& 
ChemistryMulti::find (symbol chem)
{
  for (size_t c = 0; c < chemicals.size (); c++)
    if (chemicals[c]->name == chem)
      return *chemicals[c];

  daisy_notreached ();
}

const std::vector<Chemical*>& 
ChemistryMulti::all () const
{ return chemicals; }

void 
ChemistryMulti::check_ignore (const symbol chem, Treelog& msg)
{
  if (ignored (chem))
    return;
  
  msg.message ("Fate of '" + chem.name () + "' will not be traced");
  ignore.push_back (chem);
}

void 
ChemistryMulti::deposit (const symbol chem, 
			 const double amount, const double dt, Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.deposit (amount, dt);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::spray (const symbol chem, 
		       const double amount, const double dt, Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.spray (amount, dt);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::dissipate (const symbol chem, 
			   const double amount, const double dt, Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.dissipate (amount, dt);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void
ChemistryMulti::harvest (const double removed, const double surface, 
			 const double dt)
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->harvest (removed, surface, dt);
}

void 
ChemistryMulti::mix (const Geometry& geo, const Soil& soil, 
                        const SoilWater& soil_water,
                        const double from, const double to, const double dt)
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->mix (geo, soil, soil_water, from, to, dt); 
}

void 
ChemistryMulti::swap (const Geometry& geo,
                         const Soil& soil, const SoilWater& soil_water,
                         const double from, const double middle,
                         const double to, const double dt)
{ 
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->swap (geo, soil, soil_water, from, middle, to, dt); 
}

void 
ChemistryMulti::incorporate (const Geometry& geo,
			     const symbol chem, const double amount,
			     const double from, const double to, 
			     const double dt, Treelog& msg)
{
  bool found = false;

  for (size_t c = 0; c < combine.size (); c++)
    if (combine[c]->know (chem))
      {
	if (found)
	  msg.error ("Duplicate chemical '" + chem + "' detected");

	Chemical& chemical = combine[c]->find (chem);
        chemical.incorporate (geo, amount, from, to, dt);
	found = true;
      }
  
  if (found)
    return;

  check_ignore (chem, msg);
}

void 
ChemistryMulti::tick_top (const double snow_leak_rate, // [h^-1]
                             const double cover, // [],
                             const double canopy_leak_rate, // [h^-1]
                             double surface_runoff_rate /* [h^-1] */,
                             const double dt, // [h]
			     Treelog& msg) 
{
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->tick_top (snow_leak_rate, cover, canopy_leak_rate, 
			  surface_runoff_rate, dt, msg);
}

void 
ChemistryMulti::tick_soil (const Geometry& geo, const double ponding,
			   const double R_mixing,
			   const Soil& soil, const SoilWater& soil_water,
			   const SoilHeat& soil_heat, Movement& movement,
			   const OrganicMatter& organic_matter,
			   Chemistry& chemistry, const bool flux_below, 
			   const double dt, const Scope& scope, Treelog& msg)
{ 
  Treelog::Open nest (msg, "Chemistry: " + name + ": tick soil");

  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->tick_soil (geo, ponding, R_mixing, 
			   soil, soil_water, soil_heat, movement, 
			   organic_matter, chemistry, 
			   flux_below, dt, scope, msg);
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
  Chemistry::output (log);
  output_list (combine, "combine", log, Chemistry::component);
  output_list (chemicals, "trace", log, Chemical::component);

  // We can't log identifier_sequence yet.
#if 0
  output_variable (ignore, log);
#endif
}

void 
ChemistryMulti::initialize (const AttributeList& al,
			    const Geometry& geo,
			    const Soil& soil, 
			    const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    Treelog& msg)
{
  const std::vector<const AttributeList*>& alists
    = al.alist_sequence ("combine");
  daisy_assert (alists.size () == combine.size ());
  
  for (size_t c = 0; c < combine.size (); c++)
    combine[c]->initialize (*alists[c], geo, soil, soil_water, soil_heat, msg);
}

bool 
ChemistryMulti::check (const Geometry& geo,
		       const Soil& soil, const SoilWater& soil_water,
		       const SoilHeat& soil_heat, const Chemistry& chemistry,
		       const Scope& scope, Treelog& msg) const
{ 
  bool ok = true; 
  for (size_t c = 0; c < combine.size (); c++)
    {
      Treelog::Open nest (msg, "Chemistry: '" + combine[c]->name  + "'");
      if (!combine[c]->check (geo, soil, soil_water, soil_heat, chemistry, 
			      scope, msg))
	ok = false;
    }

  // Check for duplicate chemicals.
  std::map<symbol, size_t> found;
  for (size_t i = 0; i < chemicals.size (); i++)
    {
      const symbol type = chemicals[i]->name;
      std::map<symbol, size_t>::const_iterator f = found.find (type);
      if (f != found.end ())
	{
	  std::ostringstream tmp;
	  tmp << "Chemical '" << type << "' definded in multiple chemistries:";
	  for (size_t j = 0; j < combine.size (); j++)
	    if (combine[j]->know (type))
	      tmp << " '" << combine[j]->name << "'";
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

ChemistryMulti::ChemistryMulti (Block& al)
  : Chemistry (al),
    combine (Librarian::build_vector<Chemistry> (al, "combine")),
    ignore (al.identifier_sequence ("ignore")),
    chemicals (find_chemicals (combine))
{ }

void
ChemistryMulti::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Chemistry::load_syntax (syntax, alist);
  syntax.add_object ("combine", Chemistry::component, 
                     Syntax::State, Syntax::Sequence, "\
List of chemistry parameterizations you want to combine.");
  syntax.add ("ignore", Syntax::String, Syntax::State, Syntax::Sequence,
              "Don't warn when spraying one of these chemicals.\n\
The first time an untraced chemical not on the list is sprayed on the\n\
field, Daisy will issue a warning and add the chemical to this list.");
  syntax.add_check ("ignore", VCheck::unique ());
  alist.add ("ignore", std::vector<symbol> ());
  syntax.add_object ("trace", Chemical::component, 
                     Syntax::LogOnly, Syntax::Sequence, "\
List of chemicals in nested chemistries.");
}

const AttributeList& 
Chemistry::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ChemistryMulti::load_syntax (dummy, alist);
      std::vector<const AttributeList*> combine;
      combine.push_back (&N_model ());
      alist.add ("combine", combine);
      alist.add ("type", "multi");
    }
  return alist;
}

static struct ChemistryMultiSyntax
{
  static Model& make (Block& al)
  { return *new ChemistryMulti (al); }
  ChemistryMultiSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ChemistryMulti::load_syntax (syntax, alist);
    Librarian::add_type (Chemistry::component, "multi", alist, syntax, &make);
  }
} ChemistryMulti_syntax;

// chemistry_multi.C ends here
