// soil_chemicals.C --- chemical solutes in the soil.

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
#include <map>
#include <set>

struct SoilChemicals::Implementation
{
  // Content
  typedef map<string, SoilChemical*, less<string>/**/> SoluteMap;
  SoluteMap solutes;
  typedef set<string, less<string>/**/> string_set;
  string_set all;

  // Simulation
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     const OrganicMatter&, const Chemicals& flux_in);
  void output (Log&, Filter&) const;
  void add (const Soil&, const SoilWater&,
	    double amount, double from, double to);
  void mix (const Soil&, const SoilWater&, double from, double to);
  void swap (const Soil&, const SoilWater&,
	     double from, double middle, double to);

  // Create & Destroy
  void clear ();
  void initialize (const vector<AttributeList*>,
		   const Soil&, const SoilWater&);
  bool check (unsigned n) const;
  Implementation (const vector<AttributeList*>&);
  ~Implementation ();
};

void 
SoilChemicals::Implementation::tick (const Soil& soil, 
				     const SoilWater& soil_water,
				     const SoilHeat& soil_heat,
				     const OrganicMatter& organic_matter,
				     const Chemicals& flux_in)
{ 
  // Allow `flux_in' to create new solutes.
  string_set missing;
  flux_in.find_missing (all, missing);

  for (string_set::const_iterator i = missing.begin (); 
       i != missing.end ();
       i++)
    {
      const string& name = *i;
      const Chemical& chemical = Chemicals::lookup (name);
      solutes[name] = new SoilChemical (chemical);
      solutes[name]->initialize (chemical.solute_alist (), soil, soil_water);
      all.insert (name);
    }

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
      solute.tick (soil, soil_water, J_in); 
    }
}

void 
SoilChemicals::Implementation::output (Log& log, Filter& filter) const
{
  if (filter.check ("solutes"))
    {
      log.open ("solutes");
      for (SoluteMap::const_iterator i = solutes.begin ();
	   i != solutes.end ();
	   i++)
	{
	  const string& name = (*i).first;
	  const SoilChemical& solute = *(*i).second;

	  log.open_unnamed ();
	  log.output ("chemical", filter, name);
	  if (filter.check ("solute", false))
	    {
	      log.open_alist ("solute", solute.chemical.solute_alist ());
	      solute.output (log, filter.lookup ("solute"));
	      log.close_alist ();
	    }
	  log.close_unnamed ();
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
					   const SoilWater& soil_water)
{
  for (unsigned int i = 0; i < al.size (); i++)
    {
      const string& name = al[i]->name ("chemical");
      const AttributeList& alist = al[i]->alist ("solute");
      solutes[name]->initialize (alist, soil, soil_water);
    }
}

bool 
SoilChemicals::Implementation::check (unsigned n) const
{ 
  bool ok = true; 
  for (SoluteMap::const_iterator i = solutes.begin ();
       i != solutes.end ();
       i++)
    {
      const SoilChemical& solute = *(*i).second;
      if (!solute.check (n))
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
{ }

void 
SoilChemicals::tick (const Soil& soil, const SoilWater& soil_water,
		     const SoilHeat& soil_heat, 
		     const OrganicMatter& organic_matter,
		     const Chemicals& flux_in)
{ impl.tick (soil, soil_water, soil_heat, organic_matter, flux_in); }

void 
SoilChemicals::output (Log& log, Filter& filter) const
{ impl.output (log, filter); }

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
			   const Soil& soil, const SoilWater& soil_water)
{ impl.initialize (al.alist_sequence ("solutes"), soil, soil_water); }

bool 
SoilChemicals::check (unsigned n) const
{ return impl.check (n); }

#ifdef BORLAND_TEMPLATES
template class add_submodule<SoilChemical>;
#endif

void 
SoilChemicals::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Syntax& entry_syntax = *new Syntax ();
  AttributeList entry_alist;

  entry_syntax.add ("chemical", Syntax::String, Syntax::State,
		    "Name of chemical in solute");
  add_submodule<SoilChemical> ("solute", entry_syntax, entry_alist, 
			       Syntax::State, "State of chemical solute");
  syntax.add ("solutes", entry_syntax, entry_alist, Syntax::State,
	      "List of chemical solutes in the soil");
  alist.add ("solutes", vector<AttributeList*> ());
}
  
SoilChemicals::SoilChemicals (const AttributeList& al)
  : impl (*new Implementation (al.alist_sequence ("solutes")))
{ }
  
SoilChemicals::~SoilChemicals ()
{ delete &impl; }
