// chemicals.C

#include "chemicals.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "chemical.h"
#include "submodel.h"
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
		      double canopy_water_out);
  void canopy_dissipate (Implementation& canopy_chemicals_dissipate) const;
  void canopy_out (Implementation& canopy_chemicals_out,
		   double canopy_water_storage,
		   double canopy_water_out) const;

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
  void cleanup (Implementation& out);
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
  ostrstream dummy_stream;
  assert (syntax.check (alist, dummy_stream));
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
					  double water_out)
{ 
  // new = (old + in * dt) / (1 + dt (K + water_out / (Kd + new)))

  // Calculate the factor
  for (chemical_map::const_iterator i = in.chemicals.begin ();
       i != in.chemicals.end ();
       i++)
    add ((*i).first, (*i).second * dt);

  // Divide with the divisor.
  for (chemical_map::iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    {
      const Chemical* chemical = (*i).first;
      const double K = chemical->canopy_dissipation_rate_coefficient ();
      const double Kd = chemical->canopy_washoff_coefficient ();
      const double divisor = 1.0 + dt * (K + water_out / (Kd + water_storage));
      assert (divisor > 0.0);
      (*i).second /= divisor;
    }
}

void
Chemicals::Implementation::canopy_dissipate (Implementation& dissipate) const
{
  dissipate.clear ();
  for (chemical_map::const_iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    {
      const Chemical* chemical = (*i).first;
      const double Sm = (*i).second;
      const double K = chemical->canopy_dissipation_rate_coefficient ();
      dissipate.add (chemical, K * Sm);
    }
}

void
Chemicals::Implementation::canopy_out (Implementation& out,
				       double water_storage,
				       double water_out) const
{
  out.clear ();
  for (chemical_map::const_iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    {
      const Chemical* chemical = (*i).first;
      const double Sm = (*i).second;
      const double Kd = chemical->canopy_washoff_coefficient ();
      out.add (chemical, water_out * Sm / (Kd + water_storage));
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
      if (amount > 0.0)
	{
	  log.open_unnamed ();
	  Log::Maybe maybe (log, name);
	  log.output ("chemical", name);
	  log.output ("amount", amount);
	  log.close_unnamed ();
	}
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
      if (all.find (name) == all.end ())
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
Chemicals::Implementation::cleanup (Implementation& out)
{ 
  for (chemical_map::iterator i = chemicals.begin ();
       i != chemicals.end ();
       i++)
    {
      const Chemical* chemical = (*i).first;
      const double amount = (*i).second;
      
      if (amount > 0.0 && amount < 1.e-18) // Less than one molecule...
	{
	  out.add (chemical, amount);
	  (*i).second = 0.0;
	}
    }
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
			  double canopy_water_out)
{ impl.canopy_update (canopy_chemicals_in.impl, 
		      canopy_water_storage, canopy_water_out); }

void
Chemicals::canopy_dissipate (Chemicals& canopy_chemicals_dissipate) const
{ impl.canopy_dissipate (canopy_chemicals_dissipate.impl); }

void
Chemicals::canopy_out (Chemicals& canopy_chemicals_out,
		       double canopy_water_storage,
		       double canopy_water_out) const
{ impl.canopy_out (canopy_chemicals_out.impl, 
		   canopy_water_storage, canopy_water_out); }

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
Chemicals::cleanup (Chemicals& out)
{ impl.cleanup (out.impl); }

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
check_alist_entry (const AttributeList& al, ostream& err)
{
  bool ok = true;
  
  const Library& library = Librarian<Chemical>::library ();
  const string chemical = al.name ("chemical");
  const double amount = al.number ("amount");

  if (!library.check (chemical))
    {
      err << "Unknown chemical `" << chemical << "'\n";
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
      err << "Amount must be non-negative\n";
      ok = false;
    }
  return ok;
}

// KLUDGE: Ugly hack to be able to use the standard `load_syntax' form.
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
  // KLUDGE: Ugly hack to be able to use the standard `load_syntax' form.
  chemicals_default_category = cat;
  Syntax& entry_syntax = *new Syntax ();
  AttributeList& entry_alist = *new AttributeList ();
  chemicals_load_syntax (entry_syntax, entry_alist);
  syntax.add (name, entry_syntax, entry_alist,
	      cat, Syntax::Sequence, description);
  vector<AttributeList*> alist_sequence;
  alist.add (name, alist_sequence);
// KLUDGE: Ugly hack to be able to use the standard `load_syntax' form.
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
