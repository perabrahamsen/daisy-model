// chemicals.C
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


#include "chemicals.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "chemical.h"
#include "submodel.h"
#include "treelog.h"
#include "mathlib.h"
#include <map>

struct Chemicals::Implementation
{ 
  // Types.
  typedef map<const Chemical*, double, less<const Chemical*>/**/> chemical_map;
  typedef map<string, const Chemical*, less<string>/**/> chemistry_map;
  
  // Class variables.
  static chemistry_map* chemistry;
  static const Chemical* lookup (const string& name);

  // Instance variables.
  chemical_map chemicals;

  // Utilities.
  static void move_fraction (Implementation& from, Implementation& to,
			     double fraction);
  static void copy_fraction (const Implementation& from, Implementation& to,
			     double fraction);
  
  // Canopy functions.
  void canopy_update (const Implementation& canopy_chemicals_in, 
		      double canopy_water_storage,
		      double canopy_water_out,
		      Implementation& canopy_chemicals_dissipate,
		      Implementation& canopy_chemicals_out);

  // Simulation
  void output (Log&) const;
  void add (const string& name, double amount) // [g/m^2]
    { add (lookup (name), amount); }
  void add (const Chemical* chemical, double amount) // [g/m^2]
    { chemicals[chemical] += amount; }
  void set_to (const string& name, double amount) // [g/m^2]
    { set_to (lookup (name), amount); }
  void set_to (const Chemical* chemical, double amount) // [g/m^2]
    { chemicals[chemical] = amount; }
  double amount (const string& name) const; // [g/m^2]
  typedef set<string, less<string>/**/> string_set;
  void find_missing (const string_set& all, string_set& missing) const;

  // Create and Destroy.
  void clear ();
  void operator += (const Implementation&);
  Implementation ()
    : chemicals ()
    { }
  Implementation (const vector<AttributeList*>&);
  Implementation (const Implementation& impl)
    : chemicals (impl.chemicals)
    { }
  ~Implementation ()
    { }
};


Chemicals::Implementation::chemistry_map* 
Chemicals::Implementation::chemistry = NULL;

const Chemical* 
Chemicals::Implementation::lookup (const string& name)
{
  // Create map first time.
  if (chemistry == NULL)
    chemistry = new chemistry_map ();

  // Look if name is already in map.
  chemistry_map::const_iterator entry = chemistry->find (name);
  if (entry != chemistry->end ())
    return (*entry).second;

  // Otherwise, check that it exists and is complate.
  const Library& library = Librarian<Chemical>::library ();
  assert (library.check (name));
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);
  assert (syntax.check (alist, Treelog::null ()));
  AttributeList child (alist);
  child.add ("type", name);

  // Then add it.
  const Chemical* chemical = &Librarian<Chemical>::create (child);
  (*chemistry)[name] = chemical;
  return chemical;
}

void 
Chemicals::Implementation::move_fraction (Implementation& from,
					  Implementation& to,
					  double fraction)
{ 
  for (chemical_map::iterator i = from.chemicals.begin (); 
       i != from.chemicals.end (); 
       i++)
    {
      const Chemical* chemical = (*i).first;
      const double amount = (*i).second * fraction;
      
      to.add (chemical, amount);
      (*i).second -= amount;
    }
}

void 
Chemicals::Implementation::copy_fraction (const Implementation& from, 
					  Implementation& to,
					  double fraction)
{ 
  for (chemical_map::const_iterator i = from.chemicals.begin ();
       i != from.chemicals.end ();
       i++)
    {
      const Chemical* chemical = (*i).first;
      const double amount = (*i).second * fraction;
      
      to.add (chemical, amount);
    }
}

void
Chemicals::Implementation::canopy_update (const Implementation& in, 
					  double water_storage,
					  double water_out,
					  Implementation& dissipate,
					  Implementation& out)
{ 
  // new = (old + in * dt) / (1 + dt (K + water_out / (Kd + new)))

  // Calculate the factor
  for (chemical_map::const_iterator i = in.chemicals.begin ();
       i != in.chemicals.end ();
       i++)
    add ((*i).first, (*i).second * dt);


  // Dissipitaion and washoff.
  dissipate.clear ();
  out.clear ();
  for (chemical_map::iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    {
      const Chemical* chemical = (*i).first;
      const double old_amount = (*i).second;
      const double k_1 = chemical->canopy_dissipation_rate ();
      const double f_w = chemical->canopy_washoff_coefficient ();
      const double washoff_fraction = f_w * 
	(water_storage > 0.0) ? water_out / water_storage : 0.0;
      const double new_amount 
	= old_amount / (1.0 + dt * (k_1 + washoff_fraction));
      const double dissipated = k_1 * new_amount * dt;
      const double washedoff = new_amount * washoff_fraction * dt;
      assert (approximate (new_amount, old_amount - washedoff - dissipated));

      dissipate.add (chemical, dissipated);
      if (new_amount > 0.0 && new_amount < 1.e-18) // Less than one molecule...
	{
	  (*i).second = 0.0;
	  out.add (chemical, new_amount);
	}
      else
	{
	  (*i).second = new_amount;
	  out.add (chemical, washedoff);
	}
    }
}

void
Chemicals::Implementation::output (Log& log) const
{
  for (chemical_map::const_iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    { 
      const string& name = (*i).first->name;
      const double amount = (*i).second;
      Log::Named named (log, name);
      log.output ("chemical", name);
      log.output ("amount", amount);
    }
}

double
Chemicals::Implementation::amount (const string& name) const
{
  const Chemical* chemical = lookup (name);

  chemical_map::const_iterator i = chemicals.find (chemical);
  
  if (i == chemicals.end ())
    return 0.0;

  return  (*i).second;
}

void 
Chemicals::Implementation::find_missing (const string_set& all,
					 string_set& missing) const
{
  for (chemical_map::const_iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    {
      const string name = (*i).first->name;
      string_set::const_iterator found = all.find (name);
      if (found == all.end ())
	missing.insert (name);
    }
}

void
Chemicals::Implementation::clear ()
{ 
  for (chemical_map::iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    (*i).second = 0.0;
}

void
Chemicals::Implementation::operator += (const Implementation& other)
{ 
  for (chemical_map::const_iterator i = other.chemicals.begin ();
       i != other.chemicals.end ();
       i++)
    add ((*i).first, (*i).second);
}

Chemicals::Implementation::Implementation (const vector<AttributeList*>& al)
{
  for (unsigned int i = 0; i < al.size (); i++)
    add (al[i]->name ("chemical"), al[i]->number ("amount"));
}

const Chemical&
Chemicals::lookup (const string& name)
{ return *Implementation::lookup (name); }

void
Chemicals::move_fraction (Chemicals& from, Chemicals& to, double fraction)
{ Implementation::move_fraction (from.impl, to.impl, fraction); }

void
Chemicals::copy_fraction (const Chemicals& from, Chemicals& to,
			  double fraction)
{ Implementation::copy_fraction (from.impl, to.impl, fraction); }

void
Chemicals::canopy_update (const Chemicals& canopy_chemicals_in, 
			  double canopy_water_storage,
			  double canopy_water_out,
			  Chemicals& canopy_chemicals_dissipate,
			  Chemicals& canopy_chemicals_out)
{ impl.canopy_update (canopy_chemicals_in.impl, 
		      canopy_water_storage, canopy_water_out,
		      canopy_chemicals_dissipate.impl,
		      canopy_chemicals_out.impl); }

void
Chemicals::output (Log& log) const
{ impl.output (log); }

void 
Chemicals::add (const string& chemical, double amount)
{ impl.add (chemical, amount); }

void 
Chemicals::set_to (const string& chemical, double amount)
{ impl.set_to (chemical, amount); }

double
Chemicals::amount (const string& chemical) const
{ return impl.amount (chemical); }

void 
Chemicals::find_missing (const string_set& all, string_set& missing) const
{ impl.find_missing (all, missing); }

void 
Chemicals::clear ()
{ impl.clear (); }

void 
Chemicals::operator += (const Chemicals& other)
{ impl += other.impl; }

void 
Chemicals::operator = (const Chemicals& other)
{ impl.chemicals = other.impl.chemicals; }
  
Chemicals::Chemicals (const Chemicals& other)
  : impl (*new Implementation (other.impl))
{ }

static bool
check_alist_entry (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  
  const Library& library = Librarian<Chemical>::library ();
  const string chemical = al.name ("chemical");
  const double amount = al.number ("amount");

  if (!library.check (chemical))
    {
      err.entry (string ("Unknown chemical '") + chemical + "'");
      ok = false;
    }
  else
    {
      const Syntax& syntax = library.syntax (chemical);
      const AttributeList& alist = library.lookup (chemical);
      if (!syntax.check (alist, err))
	ok = false;
    }
  if (!(amount >= 0.0))
    {
      err.entry ("Amount must be non-negative");
      ok = false;
    }
  return ok;
}

// KLUDGE: Ugly hack to be able to use the standard 'load_syntax' form.
static Syntax::category chemicals_default_category = Syntax::State;

static void chemicals_load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist_entry);
  alist.add ("description", "Name and amount of chemical compounds.");
  alist.add ("submodel", "Chemicals");
  syntax.add ("chemical", Syntax::String, Syntax::State, 
	      "Name of chemical.");
  syntax.add ("amount", "g/m^2", chemicals_default_category,
	      "Amount of chemical compound");
  syntax.order ("chemical", "amount");
}

void 
Chemicals::add_syntax (const char* name,
		       Syntax& syntax, AttributeList& alist,
		       Syntax::category cat, 
		       const string& description)
{
  // KLUDGE: Ugly hack to be able to use the standard 'load_syntax' form.
  chemicals_default_category = cat;
  Syntax& entry_syntax = *new Syntax ();
  AttributeList entry_alist;
  chemicals_load_syntax (entry_syntax, entry_alist);
  syntax.add (name, entry_syntax, entry_alist,
	      cat, Syntax::Sequence, description);
  alist.add (name, vector<AttributeList*> ());
  // KLUDGE: Ugly hack to be able to use the standard 'load_syntax' form.
  chemicals_default_category = Syntax::State;
}
  
Chemicals::Chemicals (const vector<AttributeList*>& al)
  : impl (*new Implementation (al))
{ }
  
Chemicals::Chemicals ()
  : impl (*new Implementation ())
{ }
  
Chemicals::~Chemicals ()
{ delete &impl; }

static Submodel::Register chemicals_submodel ("Chemicals", 
					      chemicals_load_syntax);
