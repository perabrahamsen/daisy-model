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

Librarian<AM>::Content* Librarian<AM>::content = NULL;

const char *const AM::description = "\
The `am' component describes various kinds of fertilizer and other\n\
added matter such as crop residues.  In particular, it describes how\n\
they decompose.";

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
  void output (Log&) const;
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
  void output (Log&) const;
    
  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList&);
  Lock (string c, string p);
  Lock (const AttributeList& al);
};

void 
AM::Implementation::Lock::output (Log& log) const
{
  log.output ("crop", crop);
  log.output ("part", part);
}  


void
AM::Implementation::Lock::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("crop", Syntax::String, Syntax::State, 
	      "Crop to which this am is locked");
  syntax.add ("part", Syntax::String, Syntax::State, 
	      "Crop part to which this am is locked");
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
AM::Implementation::output (Log& log) const
{ 
  log.output ("creation", creation);
  log.output ("name", name);
  if (lock)
    output_submodule (*lock, "lock", log);
  output_vector (om, "om", log);
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
	  CERR << "in om[" << i << "]\n";
	  ok = false;
	}
      }
  if (!ok)
    CERR << "in am\n";
  
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
AM::output (Log& log) const
{ impl.output (log); }

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

AM& 
AM::create (const AttributeList& al , const Geometry& geometry)
{ 
  AM& am = *new AM (al); 
  am.initialize (geometry);
  return am;
}

// Crop part.
AM& 
AM::create (const Geometry& /*geometry*/, const Time& time,
	    const vector<AttributeList*>& ol,
	    const string& sort, const string& part,
	    AM::lock_type lock)
{
  AttributeList al;
  al.add ("type", "state");
  al.add ("creation", time);
  al.add ("name", sort + "/" + part);
  al.add ("om", ol);
  AM& am = *new AM (al);
  if (lock == AM::Locked)
    am.impl.lock = new AM::Implementation::Lock (sort, part);
  return am;
}

AM::AM (const AttributeList& al)
  : impl (*new Implementation 
	  (al.time ("creation"),
	   al.name (al.check ("name") ? "name" : "type"),
	   map_construct<OM> (al.alist_sequence ("om")))),
    alist (al),
    name ("state")
{
  if (al.check ("lock"))
    impl.lock = new AM::Implementation::Lock (al.alist ("lock"));
 }

void
AM::initialize (const Geometry& geometry)
{
  const string syntax = alist.name ("syntax");
  
  if (syntax == "state")
    {
      if (alist.check ("lock"))
	impl.lock = new Implementation::Lock (alist.alist ("lock"));
    }
  else if (syntax == "organic")
    {
      // Get initialization parameters.
      const double weight = alist.number ("weight") 
	* alist.number ("dry_matter_fraction") 
	* 0.01;			// T / ha --> g / cm²

      const double C = weight * alist.number ("total_C_fraction");
      const double N = weight * alist.number ("total_N_fraction")
	* (1.0 - (alist.number ("NO3_fraction") 
		  + alist.number ("NH4_fraction")));
      add (C, N);
    }
  else if (syntax == "mineral")
    assert (false);
  else if (syntax == "crop")
    assert (false);
  else if (syntax == "initial")
    {
      const vector<AttributeList*>& oms = alist.alist_sequence ("om");
      const vector<OM*>& om = impl.om;
      
      const vector<AttributeList*>& layers
	= alist.alist_sequence ("layers");
      
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
		CERR << "Missing initial fraction in initial am.\n";
	      else
		missing_number = j;
	    }
	  if (missing_number > -1)
	    {
	      if (missing_fraction < -0.1e-10)
		CERR << "Specified over 100% C in om in initial am.\n";
	      else if (missing_fraction > 0.0)
		geometry.add (om[missing_number]->C, 
			  last, end, C * missing_fraction);
	    }
	  else if (missing_fraction < -0.1e-10)
	    CERR << "Specified more than all C in om in initial am.\n";
	  else if (missing_fraction > 0.1e-10)
	    CERR << "Specified less than all C in om in initial am.\n";
	  
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
      const double weight = alist.number ("weight"); // T DM / ha
      const double total_C_fraction = alist.number ("total_C_fraction");
      const double C = weight * 1000.0*1000.0 / (100.0*100.0*100.0*100.0)
	* total_C_fraction; // g C / cm²;
      const double k = M_LN2 / alist.number ("dist");
      const double depth = alist.number ("depth");

      // Calculate density.
      vector<double> density (geometry.size (), 0.0);
      for (unsigned int i = 0; 
	   i < geometry.size () && geometry.z (i) > depth;
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
      CERR << "no syntax";
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
	  CERR << "in om[" << i << "]\n";
	  ok = false;
	}
      }
  if (has_all_initial_fraction)
    {
      CERR << "you should leave initial_fraction in one om unspecified\n";
      ok = false;
    }
  if (has_all_C_per_N)
    {
      CERR << "you should leave C_per_N in one om unspecified\n";
      ok = false;
    }
  ::check (al, "weight", ok);
  if (!ok)
    CERR << "in am\n";
  
  return ok;
}

static bool check_root (const AttributeList& al)
{ 
  assert (al.name ("syntax") == "root");
  
  bool ok = true;

  non_positive (al.number ("depth"), "depth", ok);
  non_negative (al.number ("dist"), "dist", ok);
  non_negative (al.number ("weight"), "weight", ok);
  is_fraction (al.number ("total_C_fraction"), "total_C_fraction", ok);

  return ok;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<AM::Implementation::Lock>;
template class add_submodule_sequence<OM>;
template class add_submodule<IM>;
#endif

static struct AM_Syntax
{
  static AM&
  make (const AttributeList& al)
    { return *new AM (al); }
  AM_Syntax ()
    {
      // State.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Most AM models are only used for initialization, they will be comnverted \
to this generic model after creation, so this is what you will see in a \
checkpoint.  This model contains a number (typically 2) of separate \
pools, each of which have their own turnover rate.");

	syntax.add ("creation", Syntax::Date, Syntax::State, 
		    "Time this AM was created.");
	alist.add ("syntax", "state");
	syntax.add ("name", Syntax::String, Syntax::State, "\
A name given to this AOM so you can identify it in for example log files.");
	add_submodule<AM::Implementation::Lock> ("lock", syntax, alist,
						 Syntax::OptionalState, "\
This AM belongs to a still living plant");
	add_submodule_sequence<OM> ("om", syntax, Syntax::State, 
				    "The individual AOM pools.");
	Librarian<AM>::add_type ("state", alist, syntax, &make);
      }
      // Organic fertilizer.
      {
	Syntax& syntax = *new Syntax ();
	syntax.add_check (check_organic);
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Organic fertilizer, typically slurry or manure from animals.");
	syntax.add ("creation", Syntax::Date, Syntax::State, 
		    "Time of application.");
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "organic");
	syntax.add ("weight", "T w.w./ha", Syntax::Const,
		    "Amount of fertilizer applied.");
	syntax.add ("dry_matter_fraction", Syntax::None (), Syntax::Const,
		    "Dry matter fraction of total weight.");
	syntax.add ("total_C_fraction", Syntax::None (), Syntax::Const,
		    "Carbon fraction of dry matter.");
	syntax.add ("total_N_fraction", Syntax::None (), Syntax::Const,
		    "Nitrogen fraction of dry matter");
	add_submodule_sequence<OM> ("om", syntax, Syntax::State,
				    "The individual AOM pools.");
	syntax.add ("NO3_fraction", Syntax::None (), Syntax::Const, 
		    "Nitrate fraction of total N in fertilizer.  \
The remaining nitrogen is assumed to be ammonium or organic.");
	alist.add ("NO3_fraction", 0.0);
	syntax.add ("NH4_fraction", Syntax::None (), Syntax::Const, 
		    "Ammonium fraction of total N in fertilizer.  \
The remaining nitrogen is assumed to be nitrate or organic.");
	alist.add ("NH4_fraction", 0.0);
	syntax.add ("NH4_evaporation", Syntax::None (), Syntax::Const, 
		    "Fraction of NH4 that evaporates on application.");
	alist.add ("NH4_evaporation", 0.0);
	Librarian<AM>::add_type ("organic", alist, syntax, &make);
      }
      // Mineral fertilizer.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "Mineral fertilizer.");
	syntax.add ("creation", Syntax::Date, Syntax::State, 
		    "Time of application.");
	alist.add ("creation", Time (1, 1, 1, 1));
	syntax.add ("weight", "kg N/ha", Syntax::Const,
		    "Amount of fertilizer applied.");
	syntax.add ("NH4_fraction", Syntax::None (), Syntax::Const, 
		    "Ammonium fraction of total N in fertilizer.  \
The remaining nitrogen is assumed to be nitrate.");
	syntax.add ("NH4_evaporation", Syntax::None (), Syntax::Const, 
		    "Fraction of NH4 that evaporates on application.");
	alist.add ("NH4_evaporation", 0.0);
	alist.add ("syntax", "mineral");
	Librarian<AM>::add_type ("mineral", alist, syntax, &make);
      }
      // Initialization.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Initial added organic matter at the start of the simulation.");
	syntax.add ("creation", Syntax::Date, Syntax::State,
		    "Start of simulation.");
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "initial");
	Syntax& layer_syntax = *new Syntax ();
	layer_syntax.add ("end", "cm", Syntax::Const,
			  "\
Height where this layer ends (a negative number).");
	layer_syntax.add ("weight", "kg C/m^2", Syntax::Const,
			  "Carbon in this layer.");
	layer_syntax.order ("end", "weight");
	syntax.add ("layers", layer_syntax, Syntax::Sequence, "\
Carbon content in different soil layers.  The carbon is assumed to be \
uniformly distributed in each layer.");
	add_submodule_sequence<OM> ("om", syntax, Syntax::State,
				    "The individual AOM pools.");
	Librarian<AM>::add_type ("initial", alist, syntax, &make);
      }
      // Root initialization,
      {
	Syntax& syntax = *new Syntax ();
	syntax.add_check (check_root);
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "Initialization of old root remains.");
	syntax.add ("creation", Syntax::Date, Syntax::State,
		    "Start of simulation.");
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "root");
	syntax.add ("depth", "cm", Syntax::Const, "\
How far down does the old root reach? (a negative number)");
	syntax.add ("dist", "cm", Syntax::Const, "\
Distance to go down in order to decrease the root density to half the \
original.");
	syntax.add ("weight", "T DM/ha", Syntax::Const, 
		    "Total weight of old root dry matter.");
	syntax.add ("total_C_fraction", Syntax::None (), Syntax::Const, 
		    "Carbon fraction of total root dry matter");
	add_submodule_sequence<OM> ("om", syntax, Syntax::State,
				    "The individual AOM pools.");
	Librarian<AM>::add_type ("root", alist, syntax, &make);
      }
    }
} am_syntax;
