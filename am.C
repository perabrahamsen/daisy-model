// am.C

#include "am.h"
#include "om.h"
#include "im.h"
#include "syntax.h"
#include "alist.h"
#include "time.h"
#include "log.h"
#include "geometry.h"
#include "mathlib.h"
#include <numeric>

static Library* AM_library = NULL;

struct AM::Implementation
{
  // Content.
  const Time creation;		// When it was created.
  const string name;		// What is was.
  vector<OM*> om;		// Organic matter pool.

  // Use this if a living crop is adding to this AM.
  struct Lock;
  const Lock* lock;
  void unlock ();		// Crop died.
  bool locked () const;		// Test if this AM can be safely removed.
  const string crop_name () const; // Name of locked crop.
  const string crop_part_name () const; // Name of locked crop part.

  // Simulation.
  void output (Log&, Filter&) const;
  bool check () const;
  void mix (const Geometry&, double from, double to, double penetration = 1.0);
  void swap (const Geometry&, double from, double middle, double to);
  double total_C (const Geometry& geometry) const;
  double total_N (const Geometry& geometry) const;
  void pour (vector<double>& cc, vector<double>& nn); // Move content to cc&nn.
  void append_to (vector<OM*>& added); // Add OM's to added.
  void distribute (double C, vector<double>& om_C, // Helper for `add' fns.
		   double N, vector<double>& om_N);
  void add (double C, double N);// Add dead leafs.
  void add (const Geometry&,	// Add dead roots.
	    double C, double N, 
	    const vector<double>& density);
  void add (const Geometry&,	// Add initial dead roots.
	    double C, /* Fixed C/N */
	    const vector<double>& density);


  // Create and Destroy.
  Implementation (const Time& c, const string& n, vector<OM*>& o)
    : creation (c),
      name (n),
      om (o),
      lock (NULL)
  { }
  ~Implementation ()
  { sequence_delete (om.begin (), om.end ()); }
};

struct AM::Implementation::Lock
{ 
  // Content.
  string crop;
  string part;

  // Simulation.
  void output (Log&, Filter&) const;
    
  // Create and Destroy.
  Lock (string c, string p);
  Lock (const AttributeList& al);
};

void 
AM::Implementation::Lock::output (Log& log, Filter& filter) const
{
  log.output ("crop", filter, crop);
  log.output ("part", filter, part);
}  
  
AM::Implementation::Lock::Lock (string c, string p)
  : crop (c),
    part (p)
{ }
  
AM::Implementation::Lock::Lock (const AttributeList& al)
  : crop (al.name ("crop")),
    part (al.name ("part"))
{ }

void 
AM::Implementation::unlock ()
{
  assert (lock != NULL);
  delete lock;
  lock = NULL;
};		

bool 
AM::Implementation::locked () const
{ return lock != NULL; }

const string 
AM::Implementation::crop_name () const
{ 
  assert (lock);
  return lock->crop;
}

const string 
AM::Implementation::crop_part_name () const
{
  assert (lock);
  return lock->part;
}

void
AM::Implementation::distribute (double C, vector<double>& om_C, 
				double N, vector<double>& om_N)
{
  // Fill out the blanks.
  int missing_fraction = -1;
  int missing_C_per_N = -1;
  
  for (unsigned int i = 0; i < om.size (); i++)
    {
      const double fraction = om[i]->initial_fraction;
      const double C_per_N = om[i]->initial_C_per_N;

      if (fraction != OM::Unspecified)
	{
	  om_C[i] = C * fraction;
      
	  if (C_per_N != OM::Unspecified)
	    {
	      om_N[i] = om_C[i] / C_per_N;
	      assert (om_N[i] >= 0.0);
	    }
	  else
	    {
	      assert (missing_C_per_N < 0);
	      missing_C_per_N = i;
	    }
	}
      else
	{
	  assert (missing_fraction < 0);
	  missing_fraction = i;
	  if (om[i]->initial_C_per_N == OM::Unspecified)
	    {
	      assert (missing_C_per_N < 0);
	      missing_C_per_N = i;
	    }
	}
    }
  assert (missing_C_per_N > -1);
  assert (missing_fraction > -1);

  // Calculate C in missing fraction.
  om_C[missing_fraction] = C - accumulate (om_C.begin (), om_C.end (), 0.0);
  
  // Calculate N in missing C/N.
  if (missing_fraction != missing_C_per_N)
    {
      const double C_per_N = om[missing_fraction]->C_per_N[0];
      assert (C_per_N >= 0.0);
      om_N[missing_fraction] = om_C[missing_fraction] / C_per_N;
      assert (om_N[missing_fraction] >= 0.0);
    }
  om_N[missing_C_per_N] = N - accumulate (om_N.begin (), om_N.end (), 0.0);

  assert (approximate (C, accumulate (om_C.begin (), om_C.end (), 0.0)));
  assert (approximate (N, accumulate (om_N.begin (), om_N.end (), 0.0)));
}

void
AM::Implementation::add (double C, double N)
{
  vector<double> om_C (om.size (), 0.0);
  vector<double> om_N (om.size (), 0.0);

  distribute (C, om_C, N, om_N);

  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->add (om_C[i], om_N[i]);
}

void
AM::Implementation::add (const Geometry& geometry, 
			 double C, double N,
			 const vector<double>& density)
{
  const double old_C = total_C (geometry);
  const double old_N = total_N (geometry);

  vector<double> om_C (om.size (), 0.0);
  vector<double> om_N (om.size (), 0.0);

  distribute (C, om_C, N, om_N);

  for (unsigned int i = 0; i < om.size (); i++)
    {
      const double C_per_N = om[i]->initial_C_per_N;
      if (C_per_N == OM::Unspecified)
	om[i]->add (geometry, om_C[i], om_N[i], density);
      else
	{
	  assert (approximate (om_C[i], C_per_N * om_N[i]));
	  om[i]->add (geometry, om_C[i], density);
	}
    }

  const double new_C = total_C (geometry);
  const double new_N = total_N (geometry);
  assert (approximate (new_C, old_C + C));
  assert (approximate (new_N, old_N + N));
}

void
AM::Implementation::add (const Geometry& geometry, 
			 double C, /* fixed C/N */
			 const vector<double>& density)
{
  const double old_C = total_C (geometry);

  // Find the missing fraction.
  vector<double> om_C (om.size (), 0.0);
  int missing_fraction = -1;
  for (unsigned int i = 0; i < om.size (); i++)
    {
      const double fraction = om[i]->initial_fraction;

      if (fraction != OM::Unspecified)
	om_C[i] = C * fraction;
      else
	{
	  assert (missing_fraction < 0);
	  missing_fraction = i;
	}
    }
  assert (missing_fraction > -1);

  // Calculate C in missing fraction.
  om_C[missing_fraction] = C - accumulate (om_C.begin (), om_C.end (), 0.0);
  assert (approximate (C, accumulate (om_C.begin (), om_C.end (), 0.0)));
  
  // Distribute to OMs.
  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->add (geometry, om_C[i], density);

  const double new_C = total_C (geometry);
  assert (approximate (new_C, old_C + C));
}

void 
AM::Implementation::append_to (vector<OM*>& added)
{
  for (unsigned i = 0; i < om.size (); i++)
    added.push_back (om[i]);
}

void
AM::Implementation::output (Log& log, Filter& filter) const
{ 
  log.output ("creation", filter, creation);
  log.output ("name", filter, name);
  if (lock)
    output_submodule (*lock, "lock", log, filter);
  output_vector (om, "om", log, filter);
}

bool 
AM::Implementation::check () const
{ 
  bool ok = true;

  for (unsigned int i = 0; i < om.size (); i++)
    {
      bool om_ok = true;
      
      non_negative (om[i]->top_C, "top_C", om_ok);

      for (unsigned int j = 0; j < om[i]->C_per_N.size (); j++)
	non_negative (om[i]->C_per_N[j], "C_per_N", om_ok, j);

      non_negative (om[i]->turnover_rate, "turnover_rate", om_ok);

      for (unsigned int j = 0; j < om[i]->efficiency.size (); j++)
	non_negative (om[i]->efficiency[j], "efficiency", om_ok, j);

      non_negative (om[i]->maintenance, "maintenance", om_ok);

      if (!om_ok)
	{
	  cerr << "in om[" << i << "]\n";
	  ok = false;
	}
      }
  if (!ok)
    cerr << "in am\n";
  
  return ok;
}

void 
AM::Implementation::mix (const Geometry& geometry,
			 double from, double to, double penetration)
{
  const double old_C = total_C (geometry);
  const double old_N = total_N (geometry);

  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->mix (geometry, from, to, penetration);

  const double new_C = total_C (geometry);
  const double new_N = total_N (geometry);
  
  assert (approximate (new_C, old_C));
  assert (approximate (new_N, old_N));
}

void
AM::Implementation::swap (const Geometry& geometry,
			  double from, double middle, double to)
{
  const double old_C = total_C (geometry);
  const double old_N = total_N (geometry);

  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->swap (geometry, from, middle, to);

  const double new_C = total_C (geometry);
  const double new_N = total_N (geometry);
  
  assert (approximate (new_C, old_C));
  assert (approximate (new_N, old_N));
}

double 
AM::Implementation::total_C (const Geometry& geometry) const
{
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->total_C (geometry);
  return total;
}

double 
AM::Implementation::total_N (const Geometry& geometry) const
{
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->total_N (geometry);
  return total;
}

void 
AM::Implementation::pour (vector<double>& cc, vector<double>& nn)
{
  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->pour (cc, nn);
}

void
AM::output (Log& log, Filter& filter) const
{ impl.output (log, filter); }

void 
AM::append_to (vector<OM*>& added)
{ impl.append_to (added); }

bool 
AM::check () const
{ return impl.check (); }

void 
AM::mix (const Geometry& geometry,
	 double from, double to, double penetration)
{ impl.mix (geometry, from, to, penetration); }

void
AM::swap (const Geometry& geometry,
	  double from, double middle, double to)
{ impl.swap (geometry, from, middle, to); }

double 
AM::total_C (const Geometry& geometry) const
{ return impl.total_C (geometry); }

double 
AM::total_N (const Geometry& geometry) const
{ return impl.total_N (geometry); }

void 
AM::pour (vector<double>& cc, vector<double>& nn)
{ impl.pour (cc, nn); }

void 
AM::add (double C, double N)
{ impl.add (C, N); }

void 
AM::add (const Geometry& geometry,
	 double C, double N, 
	 const vector<double>& density)
{ impl.add (geometry, C, N, density); }

void 
AM::unlock ()
{ impl.unlock (); }

bool 
AM::locked () const
{ return impl.locked (); }

const string 
AM::crop_name () const
{ return impl.crop_name (); }

const string
AM:: crop_part_name () const
{ return impl.crop_part_name (); }

Library&
AM::library ()
{
  assert (AM_library);
  return *AM_library;
}

void 
AM::derive_type (const string& name, AttributeList& al, const string& super)
{
  assert (AM_library);
  AM_library->add (name, al, AM_library->syntax (super));
}

AM& 
AM::create (const AttributeList& al , const Geometry& geometry)
{
  return create (al, geometry, Time (1, 1, 1, 1));
}

AM& 
AM::create (const AttributeList& al, const Geometry& geometry, const Time& time)
{
  return *new AM (al, geometry, time);
}

// Crop part.
AM& 
AM::create (const Geometry& geometry, const Time& time,
	    vector<AttributeList*> ol,
	    const string name, const string part,
	    AM::lock_type lock)
{
  AM* am = new AM (geometry, time, ol, name, part);
  
  if (lock == AM::Locked)
    am->impl.lock = new AM::Implementation::Lock (name, part);
  return *am;
}

AM::AM (const Geometry& /* geometry */, const Time& t, 
	vector<AttributeList*> ol,
	const string sort, const string part)
  : impl (*new Implementation (t, 
			       sort + "/" + part,
			       map_construct<OM> (ol))),
    name ("state")
{ }

AM::AM (const AttributeList& al, const Geometry& geometry, const Time& time)
  : impl (*new Implementation 
	  (al.check ("creation") ? al.time ("creation") : time,
	    al.name (al.check ("name") ? "name" : "type"),
	    map_construct<OM> (al.alist_sequence ("om")))),
    name ("state")
{
  const string syntax = al.name ("syntax");
  
  if (syntax == "state")
    {
      if (al.check ("lock"))
	impl.lock = new Implementation::Lock (al.alist ("lock"));
    }
  else if (syntax == "organic")
    {
      // Get initialization parameters.
      const double weight = al.number ("weight") 
	* al.number ("dry_matter_fraction") 
	* 0.1;			// kg / m² --> g / cm²
      const double C = weight * al.number ("total_C_fraction");
      const double N = weight * IM::N_left (al);
      
      add (C, N);
    }
  else if (syntax == "mineral")
    assert (false);
  else if (syntax == "crop")
    assert (false);
  else if (syntax == "initial")
    {
      const vector<AttributeList*>& oms = al.alist_sequence ("om");
      const vector<OM*>& om = impl.om;
      
      const vector<AttributeList*>& layers
	= al.alist_sequence ("layers");
      
      double last = 0.0;
      for (unsigned int i = 0; i < layers.size (); i++)
	{
	  const double end = layers[i]->number ("end");
	  const double weight = layers[i]->number ("weight"); // Kg C/m²
	  const double C = weight * 1000.0 / (100.0 * 100.0); // g C / cm²
	  int missing_number = -1;
	  double missing_fraction = 1.0;
	  for (unsigned int j = 0; j < oms.size (); j++)
	    {
	      const double fraction = oms[j]->number ("initial_fraction");
	      if (fraction != OM::Unspecified)
		{
		  missing_fraction -= fraction;
		  geometry.add (om[j]->C, last, end, C * fraction);
		}
	      else if (missing_number != -1)
		// Should be catched by syntax check.
		cerr << "Missing initial fraction in initial am.\n";
	      else
		missing_number = j;
	    }
	  if (missing_number > -1)
	    {
	      if (missing_fraction < -0.1e-10)
		cerr << "Specified over 100% C in om in initial am.\n";
	      else if (missing_fraction > 0.0)
		geometry.add (om[missing_number]->C, 
			  last, end, C * missing_fraction);
	    }
	  else if (missing_fraction < -0.1e-10)
	    cerr << "Specified more than all C in om in initial am.\n";
	  else if (missing_fraction > 0.1e-10)
	    cerr << "Specified less than all C in om in initial am.\n";
	  
	  last = end;
	}
      // Fill C_per_N to match C.
      for (unsigned int i = 0; i < om.size (); i++)
	{
	  assert (om[i]->C_per_N.size () > 0);
	  while (om[i]->C_per_N.size () < om[i]->C.size ())
	    om[i]->C_per_N.push_back(om[i]->C_per_N[om[i]->C_per_N.size ()-1]);
	}
    }
  else if (syntax == "root")
    {
      // Get paramters.
      const double weight = al.number ("weight"); // Kg DM /m²
      const double total_C_fraction = al.number ("total_C_fraction");
      const double C = weight * 1000.0 / (100.0 * 100.0)
	* total_C_fraction; // g C / cm²;
      const double k = M_LN2 / al.number ("dist");
      const double depth = al.number ("depth");

      // Calculate density.
      vector<double> density (geometry.size (), 0.0);
      for (unsigned int i = 0; 
	   i < geometry.size () && geometry.z (i) > -depth;
	   i++)
	{
	  density[i] = k * exp (k * geometry.z (i));
	}

      // Add it.
      impl.add (geometry, C, density);
    }
}

AM::~AM ()
{ 
  assert (!locked ());
  delete &impl;
}

static bool check_organic (const AttributeList& al)
{ 
  if (!al.check ("syntax"))
    {
      cerr << "no syntax";
      return false;
    }

  const string syntax = al.name ("syntax");
  assert (syntax == "organic");
  
  bool ok = true;
  ::check (al, "dry_matter_fraction", ok);
  ::check (al, "total_C_fraction", ok);
  ::check (al, "total_N_fraction", ok);
  const vector<AttributeList*>& om_alist = al.alist_sequence ("om");
  bool has_all_initial_fraction = true;
  bool has_all_C_per_N = true;
  for (unsigned int i = 0; i < om_alist.size(); i++)
    {
      bool om_ok = true;
      if (has_all_initial_fraction)
	{
	  if (om_alist[i]->number ("initial_fraction") == OM::Unspecified)
	    has_all_initial_fraction = false;
	}
      else
	::check (*om_alist[i], "initial_fraction", om_ok);
      if (has_all_C_per_N)
	{
	  if (!om_alist[i]->check ("C_per_N"))
	    has_all_C_per_N = false;
	}
      else
	::check (*om_alist[i], "C_per_N", om_ok);
      ::check (*om_alist[i], "turnover_rate", om_ok);
      ::check (*om_alist[i], "efficiency", om_ok);
      if (!om_ok)
	{
	  cerr << "in om[" << i << "]\n";
	  ok = false;
	}
      }
  if (has_all_initial_fraction)
    {
      cerr << "you should leave initial_fraction in one om unspecified\n";
      ok = false;
    }
  if (has_all_C_per_N)
    {
      cerr << "you should leave C_per_N in one om unspecified\n";
      ok = false;
    }
  ::check (al, "weight", ok);
  if (!ok)
    cerr << "in am\n";
  
  return ok;
}

int AM_init::count;

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<OM>;
template class add_submodule<IM>;
#endif

AM_init::AM_init ()
{ 
  if (count++ == 0)
    {
      // Library.
      AM_library = new Library ("am", &AM::derive_type);

      // State.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();

	syntax.add ("creation", Syntax::Date, Syntax::Const);
	alist.add ("syntax", "state");
	syntax.add ("name", Syntax::String, Syntax::Const);
	Syntax& syntax_lock = *new Syntax ();
	syntax_lock.add ("crop", Syntax::String, Syntax::Const);
	syntax_lock.add ("name", Syntax::String, Syntax::Const);
	syntax.add ("lock", syntax_lock, Syntax::Optional);
	add_submodule_sequence<OM> ("om", syntax, Syntax::Const);
	AM_library->add ("state", alist, syntax);
      }
      // Organic fertilizer.
      {
	Syntax& syntax = *new Syntax (check_organic);
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "organic");
	syntax.add ("weight", Syntax::Number, Syntax::Const);
	syntax.add ("dry_matter_fraction",
		    Syntax::Number, Syntax::Const);
	syntax.add ("total_C_fraction",
		    Syntax::Number, Syntax::Const);
	syntax.add ("total_N_fraction",
		    Syntax::Number, Syntax::Const);
	add_submodule_sequence<OM> ("om", syntax, Syntax::Const);
	add_submodule<IM> ("im", syntax, alist);
	AM_library->add ("organic", alist, syntax);
      }
      // Mineral fertilizer.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "mineral");
	IM::load_syntax (syntax, alist);
	AM_library->add ("mineral", alist, syntax);
      }
      // Initialization.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "initial");
	Syntax& layer_syntax = *new Syntax ();
	AttributeList& layer_alist = *new AttributeList ();
	layer_syntax.add ("end", Syntax::Number, Syntax::Const);
	layer_syntax.add ("weight", Syntax::Number, Syntax::Const); // Kg C/m²
	layer_syntax.order ("end", "weight");
	syntax.add ("layers", layer_syntax, Syntax::Const, Syntax::Sequence);
	alist.add ("layers", layer_alist);
	add_submodule_sequence<OM> ("om", syntax, Syntax::Const);
	AM_library->add ("initial", alist, syntax);
      }
      // Root initialization,
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "root");
	syntax.add ("depth", Syntax::Number, Syntax::Const);
	syntax.add ("dist", Syntax::Number, Syntax::Const);
	syntax.add ("weight", Syntax::Number, Syntax::Const); // Tons DM / ha
	syntax.add ("total_C_fraction", Syntax::Number, Syntax::Const);
	add_submodule_sequence<OM> ("om", syntax, Syntax::Const);
	AM_library->add ("root", alist, syntax);
      }
    }
  assert (count > 0);
}

AM_init::~AM_init ()
{ 
  if (--count == 0)
    {
      assert (AM_library);
      delete AM_library;
      AM_library = NULL;
    }
  assert (count >= 0);
}
