// soil_chemicals.C --- chemical solutes in the soil.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "soil_chemicals.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic_matter.h"
#include "chemical.h"
#include "chemicals.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "soil_chemical.h"
#include "submodel.h"
#include <map>
#include <set>

struct SoilChemicals::Implementation
{
  // Content
  typedef map<string, SoilChemical*, less<string>/**/> SoluteMap;
  SoluteMap solutes;
  typedef set<string, less<string>/**/> string_set;
  string_set all;

  // Utilities
  void add_missing (const Soil& soil, 
		    const SoilWater& soil_water,
		    const Chemicals& chemicals, Treelog&);
  SoilChemical& find (const Soil& soil, 
		      const SoilWater& soil_water,
		      const string& name, Treelog&);

  // Simulation
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     const OrganicMatter*, const Chemicals& flux_in, Treelog&);
  void mixture (Chemicals& storage, Chemicals& down, 
		double pond, double rate) const;
  void output (Log&) const;
  void mix (const Soil&, const SoilWater&, double from, double to);
  void swap (const Soil&, const SoilWater&,
	     double from, double middle, double to);

  // Create & Destroy
  void clear ();
  void initialize (const vector<AttributeList*>,
		   const Soil&, const SoilWater&, Treelog&);
  bool check (unsigned n, Treelog&) const;
  Implementation (const vector<AttributeList*>&);
  ~Implementation ();
};

void
SoilChemicals::Implementation::add_missing (const Soil& soil, 
					    const SoilWater& soil_water,
					    const Chemicals& chemicals,
					    Treelog& out)
{
  string_set missing;
  chemicals.find_missing (all, missing);

  for (string_set::const_iterator i = missing.begin (); 
       i != missing.end ();
       i++)
    {
      const string& name = *i;
      assert (solutes.find (name) == solutes.end ());
      const Chemical& chemical = Chemicals::lookup (name);
      solutes[name] = new SoilChemical (chemical);
      solutes[name]->initialize (chemical.solute_alist (), soil, soil_water,
				 out);
      all.insert (name);
    }
}

SoilChemical& 
SoilChemicals::Implementation::find (const Soil& soil, 
				     const SoilWater& soil_water,
				     const string& name, Treelog& out)
{
  if (solutes.find (name) == solutes.end ())
    {
      const Chemical& chemical = Chemicals::lookup (name);
      solutes[name] = new SoilChemical (chemical);
      solutes[name]->initialize (chemical.solute_alist (), soil, soil_water, 
				 out);
      all.insert (name);
    }
  return *solutes[name];
}

void 
SoilChemicals::Implementation::tick (const Soil& soil, 
				     const SoilWater& soil_water,
				     const SoilHeat& soil_heat,
				     const OrganicMatter* organic_matter,
				     const Chemicals& flux_in,
				     Treelog& out)
{ 
  // Allow 'flux_in' to create new solutes.
  add_missing (soil, soil_water, flux_in, out);

  // Crop Uptake.
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    (*i).second->uptake (soil, soil_water); 

  // Decompose.
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    (*i).second->decompose (soil, soil_water, soil_heat, organic_matter); 

  // Transport.
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      const string& name = (*i).first;
      SoilChemical& solute = *(*i).second;
      // [g/m^2/h ned -> g/cm^2/h op]
      const double J_in = -flux_in.amount (name) / (100.0 * 100.0);
      solute.tick (soil, soil_water, J_in, out); 
    }
}

void 
SoilChemicals::Implementation::mixture (Chemicals& storage, // [g/m^2]
					Chemicals& up, // [g/m^2/h]
					const double pond, // [mm]
					const double rate) const // [h/mm]
{
  // Make sure we have something to mix.
  if (pond < 1e-6 || rate < 1e-99)
    return;

  // BUG: Handle chemicals in storage but not in soil.

  // Mix them.
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      const string& name = (*i).first;
      SoilChemical& solute = *(*i).second;

      const double soil_conc = solute.C (0)
	* (100.0 * 100.0) / 10.0; // [g/cm^3/] -> [g/m^2/mm]
      const double storage_amount = storage.amount (name); // [g/cm^2]
      const double storage_conc = storage_amount / pond;// [g/cm^2/mm]
      const double up_amount  // [g/cm^2/h]
	= max (-storage_amount / dt, (soil_conc - storage_conc) / rate);
      up.set_to (name, up_amount);
      storage.add (name, up_amount * dt);
    }
}

void 
SoilChemicals::Implementation::output (Log& log) const
{
  if (log.check ("solutes"))
    {
      log.open ("solutes");
      for (SoluteMap::const_iterator i = solutes.begin ();
	   i != solutes.end ();
	   i++)
	{
	  const string& name = (*i).first;
	  const SoilChemical& solute = *(*i).second;

	  Log::Unnamed unnamed (log);
	  Log::Maybe maybe (log, name);
	  log.output ("chemical", name);
	  if (log.check ("solute"))
	    {
	      log.open_alist ("solute", solute.chemical.solute_alist ());
	      solute.output (log);
	      log.close_alist ();
	    }
	}
      log.close ();
    }
}
void 
SoilChemicals::Implementation::mix (const Soil& soil,
				    const SoilWater& soil_water,
				    double from, double to)
{
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      SoilChemical& solute = *(*i).second;
      solute.mix (soil, soil_water, from, to); 
    }
}

void 
SoilChemicals::Implementation::swap (const Soil& soil,
				     const SoilWater& soil_water,
				     double from, double middle, double to)
{ 
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      SoilChemical& solute = *(*i).second;
      solute.swap (soil, soil_water, from, middle, to); 
    }
}


void
SoilChemicals::Implementation::clear ()
{ 
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      SoilChemical& solute = *(*i).second;
      solute.clear (); 
    }
}

void 
SoilChemicals::Implementation::initialize (const vector<AttributeList*> al,
					   const Soil& soil, 
					   const SoilWater& soil_water, 
					   Treelog& out)
{
  for (unsigned int i = 0; i < al.size (); i++)
    {
      const string& name = al[i]->name ("chemical");
      const AttributeList& alist = al[i]->alist ("solute");
      solutes[name]->initialize (alist, soil, soil_water, out);
    }
}

bool 
SoilChemicals::Implementation::check (unsigned n, Treelog& err) const
{ 
  bool ok = true; 
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      Treelog::Open nest (err, (*i).first);
      const SoilChemical& solute = *(*i).second;
      if (!solute.check (n, err))
	ok = false;
    }
  return ok;
}

SoilChemicals::Implementation::Implementation (const
					       vector<AttributeList*>& al)
{
  for (unsigned int i = 0; i < al.size (); i++)
    {
      const string& name = al[i]->name ("chemical");
      const AttributeList& alist = al[i]->alist ("solute");
      const Chemical& chemical = Chemicals::lookup (name);
      solutes[name] = new SoilChemical (chemical, alist);
    }
}
  
SoilChemicals::Implementation::~Implementation ()
{ map_delete (solutes.begin (), solutes.end ()); }

SoilChemical& 
SoilChemicals::find (const Soil& soil, 
		     const SoilWater& soil_water,
		     const string& name, Treelog& out)
{ return impl.find (soil, soil_water, name, out); }

void 
SoilChemicals::tick (const Soil& soil, const SoilWater& soil_water,
		     const SoilHeat& soil_heat, 
		     const OrganicMatter* organic_matter,
		     const Chemicals& flux_in, Treelog& out)
{ impl.tick (soil, soil_water, soil_heat, organic_matter, flux_in, out); }

void 
SoilChemicals::mixture (Chemicals& storage, // [g/m^2]
			Chemicals& up, // [g/m^2/h]
			const double pond, // [mm]
			const double rate) const // [h/mm]
{ impl.mixture (storage, up, pond, rate); }

void 
SoilChemicals::output (Log& log) const
{ impl.output (log); }

void 
SoilChemicals::mix (const Soil& soil, const SoilWater& soil_water,
		    double from, double to)
{ impl.mix (soil, soil_water, from, to); }

void 
SoilChemicals::swap (const Soil& soil, const SoilWater& soil_water,
		     double from, double middle, double to)
{ impl.swap (soil, soil_water, from, middle, to); }

void
SoilChemicals::clear ()
{ impl.clear (); }

void
SoilChemicals::initialize (const AttributeList& al, 
			   const Soil& soil, const SoilWater& soil_water,
			   Treelog& out)
{ impl.initialize (al.alist_sequence ("solutes"), soil, soil_water, out); }

bool 
SoilChemicals::check (unsigned n, Treelog& err) const
{ return impl.check (n, err); }

void 
SoilChemicals::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilChemicals");
  alist.add ("description", "List of chemicals in the soil.");
  Syntax& entry_syntax = *new Syntax ();
  AttributeList entry_alist;
  entry_syntax.add ("chemical", Syntax::String, Syntax::State,
		    "Name of chemical in solute.");
  entry_syntax.add_submodule ("solute", entry_alist, Syntax::State,
			      "State of chemical solute.",
			      SoilChemical::load_syntax);
  syntax.add ("solutes", entry_syntax, entry_alist, Syntax::State,
	      Syntax::Sequence, "List of chemical solutes in the soil.");
  alist.add ("solutes", vector<AttributeList*> ());
}
  
SoilChemicals::SoilChemicals (const AttributeList& al)
  : impl (*new Implementation (al.alist_sequence ("solutes")))
{ }
  
SoilChemicals::~SoilChemicals ()
{ delete &impl; }

static Submodel::Register 
soil_chemicals_submodel ("SoilChemicals", SoilChemicals::load_syntax);
