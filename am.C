// am.C
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


#include "am.h"
#include "om.h"
#include "im.h"
#include "syntax.h"
#include "alist.h"
#include "time.h"
#include "log.h"
#include "soil.h"
#include "check.h"
#include "tmpstream.h"
#include "message.h"
#include "mathlib.h"
#include <numeric>

EMPTY_TEMPLATE
Librarian<AM>::Content* Librarian<AM>::content = NULL;

const char *const AM::description = "\
The 'am' component describes various kinds of fertilizer and other\n\
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
  bool check (Treelog& err) const;
  void mix (const Geometry&, double from, double to, double penetration = 1.0);
  void swap (const Geometry&, double from, double middle, double to);
  double total_C (const Geometry& geometry) const;
  double total_N (const Geometry& geometry) const;
  double C_at (unsigned int at) const;
  double N_at (unsigned int at) const;
  void pour (vector<double>& cc, vector<double>& nn); // Move content to cc&nn.
  void append_to (vector<OM*>& added); // Add OM's to added.
  void distribute (double C, vector<double>& om_C, // Helper for 'add' fns.
		   double N, vector<double>& om_N);
  void add (double C, double N);// Add dead leafs.
  void add (const Geometry& geometry, AM::Implementation& other);
  void add (const Geometry&,	// Add dead roots.
	    double C, double N, 
	    const vector<double>& density);
  void add (const Geometry&,	// Add initial dead roots.
	    double C, /* Fixed C/N */
	    const vector<double>& density);
  double top_C () const;
  double top_N () const;
  void multiply_top (double fraction);


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
  assert (C >= 0.0);
  assert (N >= 0.0);
  assert (C == 0.0 || N > 0.0);

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
      const double C_per_N = om[missing_fraction]->initial_C_per_N;
      assert (C_per_N >= 0.0);
      om_N[missing_fraction] = om_C[missing_fraction] / C_per_N;
      assert (om_N[missing_fraction] >= 0.0);
    }
  om_N[missing_C_per_N] = N - accumulate (om_N.begin (), om_N.end (), 0.0);

  if (om_N[missing_C_per_N] < 0.0)
    {
      // Too little N, distribute evenly.
      for (unsigned int i = 0; i < om.size (); i++)
	{
	  const double fraction = om[i]->initial_fraction;
	  if (fraction == OM::Unspecified)
	    {
	      assert (i == missing_fraction);
	      om_N[i] = 0.0;
	    }
	  else
	    om_N[i] = N * fraction;
	}
      om_N[missing_fraction]
	= N - accumulate (om_N.begin (), om_N.end (), 0.0);
      assert (om_N[missing_fraction] >= 0.0);
    }

  assert (approximate (C, accumulate (om_C.begin (), om_C.end (), 0.0)));
  assert (approximate (N, accumulate (om_N.begin (), om_N.end (), 0.0)));
}

void
AM::Implementation::add (double C, double N)
{
  assert (C >= 0);
  assert (N >= 0);
  vector<double> om_C (om.size (), 0.0);
  vector<double> om_N (om.size (), 0.0);

  distribute (C, om_C, N, om_N);

  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->add (om_C[i], om_N[i]);
}

void
AM::Implementation::add (const Geometry& geometry,
			 AM::Implementation& other)
{
  assert (&other != this);
  const double old_C = total_C (geometry) + other.total_C (geometry);
  const double old_N = total_N (geometry) + other.total_N (geometry);

  vector<double> cc (geometry.size (), 0.0);
  vector<double> nn (geometry.size (), 0.0);
  other.pour (cc, nn);

  for (unsigned int at = 0; at < geometry.size (); at++)
    {
      vector<double> om_C (om.size (), 0.0);
      vector<double> om_N (om.size (), 0.0);

      distribute (cc[at], om_C, nn[at], om_N);

      for (unsigned int i = 0; i < om.size (); i++)
	om[i]->add (at, om_C[i], om_N[i]);
    }

  add (other.top_C (), other.top_N ());

  const double new_C = total_C (geometry);
  const double new_N = total_N (geometry);
  assert (approximate (old_C, new_C));
  assert (approximate (old_N, new_N));
}

void
AM::Implementation::add (const Geometry& geometry, 
			 double C, double N,
			 const vector<double>& density)
{
  assert (C >= 0);
  assert (N >= 0);
  const double old_C = total_C (geometry);
  const double old_N = total_N (geometry);

  vector<double> om_C (om.size (), 0.0);
  vector<double> om_N (om.size (), 0.0);

  distribute (C, om_C, N, om_N);

  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->add (geometry, om_C[i], om_N[i], density);

  assert (approximate (old_C + C, total_C (geometry)));
  assert (approximate (old_N + N, total_N (geometry)));
}

void
AM::Implementation::add (const Geometry& geometry, 
			 double C, /* fixed C/N */
			 const vector<double>& density)
{
  assert (C >= 0);
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

double
AM::Implementation::top_C () const
{ 
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->top_C;
  return total;
}

double 
AM::Implementation::top_N () const
{ 
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->top_N;
  return total;
}

void 
AM::Implementation::multiply_top (double fraction)
{ 
  for (unsigned int i = 0; i < om.size (); i++)
    {
      om[i]->top_N *= fraction;
      om[i]->top_C *= fraction;
    }
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
  Log::Maybe maybe (log, name);
  log.output ("creation", creation);
  log.output ("name", name);
  if (lock)
    output_submodule (*lock, "lock", log);
  output_vector (om, "om", log);
}

bool 
AM::Implementation::check (Treelog& /*err*/) const
{ 
  bool ok = true;
#if 0
  for (unsigned int i = 0; i < om.size (); i++)
    {
      TmpStream tmp;
      tmp () << "om[" << i << "]";
      Treelog::Open nest (err, tmp.str ());

      non_negative (om[i]->top_C, "top_C", ok, err);

      for (unsigned int j = 0; j < om[i]->C_per_N.size (); j++)
	non_negative (om[i]->C_per_N[j], "C_per_N", ok, err, j);

      non_negative (om[i]->turnover_rate, "turnover_rate", ok, err);

      for (unsigned int j = 0; j < om[i]->efficiency.size (); j++)
	non_negative (om[i]->efficiency[j], "efficiency", ok, err, j);

      non_negative (om[i]->maintenance, "maintenance", ok, err);
    }
#endif
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
  
  if (!approximate (new_C, old_C))
    CERR << "BUG: AM::mix new C (" << new_C 
	 << ") != old_C (" << old_C << ")\n";
  if (!approximate (new_N, old_N))
    CERR << "BUG: AM::mix new_N (" << new_N 
	 << ") != old_N (" << old_N << ")\n";
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
  
  if (!approximate (new_C, old_C))
    CERR << "BUG: AM::swap new C (" << new_C 
	 << ") != old_C (" << old_C << ")\n";
  if (!approximate (new_N, old_N))
    CERR << "BUG: AM::swap new_N (" << new_N 
	 << ") != old_N (" << old_N << ")\n";
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

double 
AM::Implementation::C_at (const unsigned int at) const
{
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->C_at (at);
  return total;
}

double 
AM::Implementation::N_at (const unsigned int at) const
{
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->N_at (at);
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
AM::check (Treelog& err) const
{ return impl.check (err); }

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

double 
AM::C_at (unsigned int at) const
{ return impl.C_at (at); }

double 
AM::N_at (unsigned int at) const
{ return impl.N_at (at); }

void 
AM::pour (vector<double>& cc, vector<double>& nn)
{ impl.pour (cc, nn); }

void 
AM::add (double C, double N)
{ impl.add (C, N); }

void 
AM::add (const Geometry& geometry, AM& other)
{ impl.add (geometry, other.impl); }

void 
AM::add (const Geometry& geometry,
	 double C, double N, 
	 const vector<double>& density)
{ impl.add (geometry, C, N, density); }

double
AM::top_C () const
{ return impl.top_C (); }

double 
AM::top_N () const
{ return impl.top_N (); }

void 
AM::multiply_top (double fraction)
{ impl.multiply_top (fraction); }

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
AM::create (const AttributeList& al1 , const Soil& soil)
{ 
  AttributeList al2 (al1);
  al2.add ("type", "state");
  if (!al2.check ("name"))
    al2.add ("name", al1.name ("type"));
  AM& am = *new AM (al2); 
  am.initialize (soil);
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

#ifdef BORLAND_TEMPLATES
template class map_construct<OM>;
#endif

const vector<AttributeList*>&
AM::default_AOM ()
{
  static vector<AttributeList*>* AOM = NULL;

  if (!AOM)
    {
      Syntax om_syntax;
      AttributeList om_alist;
      OM::load_syntax (om_syntax, om_alist);
      AttributeList& AOM1 = *new AttributeList (om_alist);
      AttributeList& AOM2 = *new AttributeList (om_alist);
      AOM1.add ("initial_fraction", 0.80);
      vector<double> CN;
      CN.push_back (90.0);
      AOM1.add ("C_per_N", CN);
      vector<double> efficiency1;
      efficiency1.push_back (0.50);
      efficiency1.push_back (0.50);
      AOM1.add ("efficiency", efficiency1);
      AOM1.add ("turnover_rate", 2.0e-4);
      vector<double> fractions1;
      fractions1.push_back (0.50);
      fractions1.push_back (0.50);
      fractions1.push_back (0.00);
      AOM1.add ("fractions", fractions1);
      vector<double> efficiency2;
      efficiency2.push_back (0.50);
      efficiency2.push_back (0.50);
      AOM2.add ("efficiency", efficiency2);
      AOM2.add ("turnover_rate", 2.0e-3);
      vector<double> fractions2;
      fractions2.push_back (0.00);
      fractions2.push_back (1.00);
      fractions2.push_back (0.00);
      AOM2.add ("fractions", fractions2);
      AOM = new vector<AttributeList*>;
      AOM->push_back (&AOM1);
      AOM->push_back (&AOM2);
    }
  return *AOM;
}

const AttributeList& 
AM::default_root ()
{
  static AttributeList root;
  
  if (!root.check ("type"))
    {
      root.add ("type", "root");
      root.add ("description", "Initialization of old root remains.");
      root.add ("creation", Time (1, 1, 1, 1));
      root.add ("syntax", "root");
      root.add ("dist", 7.0);
      root.add ("weight", 1.2);
      root.add ("total_C_fraction", 0.40);
      root.add ("total_N_fraction", 0.01);
      root.add ("om", AM::default_AOM ());
    }
  return root;
}

double
AM::get_NO3 (const AttributeList& al)
{
  if (al.check ("weight"))
    {
      if (al.name ("syntax") == "organic")
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 0.01;			// T w.w. / ha --> g / cm²
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NO3_fraction");
	}
      // Mineral fertilizer.
      assert (al.name ("syntax") == "mineral");
      return al.number ("weight")
	* (1.0 - al.number ("NH4_fraction"))
	* (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0))); // kg/ha -> g/cm^2
    }
  // Other.
  return al.number ("NO3");
}

double
AM::get_NH4 (const AttributeList& al)
{
  if (al.check ("weight"))
    {
      const double volatilization 
	= al.check ("NH4_evaporation") 
	? al.number ("NH4_evaporation")
	: al.number ("volatilization");

      if (al.name ("syntax") == "organic")
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 0.01;			// T w.w. / ha --> g / cm²
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NH4_fraction") * (1.0 - volatilization);
	}
      // Mineral fertilizer.
      assert (al.name ("syntax") == "mineral");
      
      return al.number ("weight")
	* al.number ("NH4_fraction") * (1.0 - volatilization)
	* (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0))); // kg/ha -> g/cm^2
    }
  // Other.
  assert (!al.check ("NH4_evaporation") && !al.check ("volatilization"));
  return al.number ("NH4");
}

double
AM::get_volatilization (const AttributeList& al)
{
  if (al.check ("weight"))
    {
      const double volatilization 
	= al.check ("NH4_evaporation") 
	? al.number ("NH4_evaporation")
	: al.number ("volatilization");

      if (al.name ("syntax") == "organic")
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 1000;			// T w.w. / ha --> kg DM / ha
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NH4_fraction") * volatilization;
	}
      // Mineral fertilizer.
      assert (al.name ("syntax") == "mineral");
      
      return al.number ("weight")
	* al.number ("NH4_fraction") * volatilization; 
    }
  // Other.
  assert (!al.check ("NH4_evaporation") && !al.check ("volatilization"));
  return 0.0;
}

void
AM::set_utilized_weight (AttributeList& am, const double weight)
{
  const string syntax = am.name ("syntax");
    
  if (syntax == "mineral")
    am.add ("weight", weight);
  else
    {
      assert (syntax == "organic");
      assert (am.check ("first_year_utilization"));
      assert (am.check ("total_N_fraction"));
      assert (am.check ("dry_matter_fraction"));
      const double N_fraction = am.number ("total_N_fraction");
      const double utilization = am.number ("first_year_utilization");
      const double dry_matter_fraction = am.number ("dry_matter_fraction");
      const double kg_per_ton = 1000.0;
      am.add ("weight", weight 
	      / (dry_matter_fraction *N_fraction * utilization * kg_per_ton));
    }
}

double
AM::utilized_weight (const AttributeList& am)
{
  if (am.check ("first_year_utilization")
      && am.check ("dry_matter_fraction")
      && am.check ("total_N_fraction")
      && am.check ("weight"))
    {
       const double kg_per_ton = 1000.0;
       return am.number ("weight")
	 * am.number ("dry_matter_fraction")
	 * am.number ("total_N_fraction")
	 * am.number ("first_year_utilization")
	 * kg_per_ton;
    }
  else if (am.name ("syntax") == "mineral")
    return am.number ("weight");

  return 0.0;
}
double
AM::second_year_utilization (const AttributeList& am)
{
  if (am.check ("second_year_utilization")
      && am.check ("dry_matter_fraction")
      && am.check ("total_N_fraction")
      && am.check ("weight"))
    {
       const double kg_per_ton = 1000.0;
       return am.number ("weight")
	 * am.number ("dry_matter_fraction")
	 * am.number ("total_N_fraction")
	 * am.number ("second_year_utilization")
	 * kg_per_ton;
    }
  return 0.0;
}

AM::AM (const AttributeList& al)
  : impl (*new Implementation 
	  (al.time ("creation"),
	   al.name ("name"),
	   (vector<OM*>&) map_construct<OM> (al.alist_sequence ("om")))),
    alist (al),
    name ("state")
{
  if (al.check ("lock"))
    impl.lock = new AM::Implementation::Lock (al.alist ("lock"));
 }

void
AM::initialize (const Soil& soil)
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
		  soil.add (om[j]->C, last, end, C * fraction);
		}
	      else if (missing_number != -1)
		// Should be catched by syntax check.
		throw ("Missing initial fraction in initial am");
	      else
		missing_number = j;
	    }
	  if (missing_number > -1)
	    {
	      if (missing_fraction < -0.1e-10)
		throw ("Specified over 100% C in om in initial am");
	      else if (missing_fraction > 0.0)
		soil.add (om[missing_number]->C, 
			  last, end, C * missing_fraction);
	    }
	  else if (missing_fraction < -0.1e-10)
	    throw ("Specified more than all C in om in initial am");
	  else if (missing_fraction > 0.1e-10)
	    throw ("Specified less than all C in om in initial am");
	  
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
      const double total_N_fraction = alist.number ("total_N_fraction");
      const double C = weight * 1000.0*1000.0 / (100.0*100.0*100.0*100.0)
	* total_C_fraction; // g C / cm²;
      const double N = weight * 1000.0*1000.0 / (100.0*100.0*100.0*100.0)
	* total_N_fraction; // g C / cm²;
      const double k = M_LN2 / alist.number ("dist");
      const double depth = alist.check ("depth") 
	? alist.number ("depth") : soil.MaxRootingDepth ();

      // Calculate density.
      vector<double> density (soil.size (), 0.0);
      for (unsigned int i = 0; 
	   i < soil.size () && soil.z (i) > depth;
	   i++)
	{
	  density[i] = k * exp (k * soil.z (i));
	}

      // Add it.
      impl.add (soil, C, N, density);
    }
}

AM::~AM ()
{ 
  assert (!locked ());
  delete &impl;
}

static bool check_organic (const AttributeList& al, Treelog& err)
{ 
  if (!al.check ("syntax"))
    {
      err.entry ("no syntax");
      return false;
    }

  const string syntax = al.name ("syntax");
  assert (syntax == "organic");

  static bool warned = false;
  if (al.check ("NH4_evaporation") && !warned)
    {
      err.entry ("OBSOLETE: Use 'volatilization' instead "
		 "of 'NH4_evaporation'");
      warned = true;
    }
  
  bool ok = true;
  const vector<AttributeList*>& om_alist = al.alist_sequence ("om");
  int missing_initial_fraction = 0;
  int missing_C_per_N = 0;

  for (unsigned int i = 0; i < om_alist.size(); i++)
    {
      if (om_alist[i]->number ("initial_fraction") == OM::Unspecified)
	missing_initial_fraction++;
      if (!om_alist[i]->check ("C_per_N"))
	missing_C_per_N++;
    }
  if (missing_initial_fraction != 1)
    {
      err.entry ("you should leave initial_fraction in one om unspecified");
      ok = false;
    }
  if (missing_C_per_N != 1)
    {
      err.entry ("You must leave C/N unspecified in exactly one pool.");
      ok = false;
    }
  return ok;
}

static bool check_root (const AttributeList& al, Treelog& err)
{ 
  assert (al.name ("syntax") == "root");
  
  bool ok = true;

  // We need exactly one pool with unspecified OM.
  assert (al.check ("om"));
  int unspecified = 0;
  const vector<AttributeList*>& om = al.alist_sequence ("om");
  for (unsigned int i = 0; i < om.size (); i++)
    if (OM::get_initial_C_per_N (*om[i]) == OM::Unspecified)
      unspecified++;
  if (unspecified != 1)
    { 
      err.entry ("You must leave C/N unspecified in exactly one pool.");
      ok = false;
    }
  return ok;
}

static struct AM_Syntax
{
  static AM&
  make (const AttributeList& al1)
  { 
    AttributeList al2 (al1);
    al2.add ("type", "state");
    if (!al2.check ("name"))
      al2.add ("name", al1.name ("type"));
    return *new AM (al2); 
  }
  AM_Syntax ()
    {
      // State.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Most AM models are only used for initialization, they will be comnverted\n\
to this generic model after creation, so this is what you will see in a\n\
checkpoint.  This model contains a number (typically 2) of separate\n\
pools, each of which have their own turnover rate.");

	syntax.add ("creation", Syntax::Date, Syntax::State, 
		    "Time this AM was created.");
	alist.add ("syntax", "state");
	syntax.add ("name", Syntax::String, Syntax::State, "\
A name given to this AOM so you can identify it in for example log files.");
	syntax.add_submodule ("lock", alist, Syntax::OptionalState, "\
This AM belongs to a still living plant",
			      AM::Implementation::Lock::load_syntax);
	syntax.add_submodule_sequence ("om", Syntax::State, 
				       "The individual AOM pools.",
				       OM::load_syntax);
	Librarian<AM>::add_type ("state", alist, syntax, &make);
      }
      // Organic fertilizer.
      {
	Syntax& syntax = *new Syntax ();
	syntax.add_check (check_organic);
	AttributeList& alist = *new AttributeList ();
	syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		    "Description of this fertilizer type."); 
	alist.add ("description", "\
Organic fertilizer, typically slurry or manure from animals.");
	syntax.add ("creation", Syntax::Date, Syntax::State, 
		    "Time of application.");
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "organic");
	syntax.add ("weight", "T w.w./ha", Check::non_negative (),
		    Syntax::Const,
		    "Amount of fertilizer applied.");
	alist.add ("weight", 0.0);
	syntax.add ("first_year_utilization", 
		    Syntax::Fraction (), Syntax::OptionalConst, 
		    "Estimated useful N fraction for the first year.\n\
In Denmark, this is governed by legalisation.");
	syntax.add_fraction ("second_year_utilization", 
			     Syntax::OptionalConst, "\
Estimated useful N fraction for the second year.\n\
In Denmark, this is governed by legalisation.");
	syntax.add_fraction ("dry_matter_fraction", Syntax::Const,
			     "Dry matter fraction of total weight.");
	syntax.add_fraction ("total_C_fraction", Syntax::Const,
			     "Carbon fraction of dry matter.");
	syntax.add_fraction ("total_N_fraction", Syntax::Const,
			     "Nitrogen fraction of dry matter");
	syntax.add_submodule_sequence ("om", Syntax::State,
				       "The individual AOM pools.",
				       OM::load_syntax);
	syntax.add_fraction ("NO3_fraction", Syntax::Const, 
		    "Nitrate fraction of total N in fertilizer. \n\
The remaining nitrogen is assumed to be ammonium or organic.");
	alist.add ("NO3_fraction", 0.0);
	syntax.add_fraction ("NH4_fraction", Syntax::Const, "\
Ammonium fraction of total N in fertilizer. \n\
The remaining nitrogen is assumed to be nitrate or organic.");
	alist.add ("NH4_fraction", 0.0);
	syntax.add_fraction ("NH4_evaporation", Syntax::OptionalConst, 
			     "Obsolete alias for 'volatilization'.");
	syntax.add_fraction ("volatilization", Syntax::Const, "\
Fraction of NH4 that evaporates on application.");
	alist.add ("volatilization", 0.0);
	Librarian<AM>::add_type ("organic", alist, syntax, &make);
      }
      // Mineral fertilizer.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		    "Description of this fertilizer type."); 
	alist.add ("description", "Mineral fertilizer.");
	syntax.add ("creation", Syntax::Date, Syntax::State, 
		    "Time of application.");
	alist.add ("creation", Time (1, 1, 1, 1));
	syntax.add ("weight", "kg N/ha", Check::non_negative (), Syntax::Const,
		    "Amount of fertilizer applied.");
	alist.add ("weight", 0.0);
	syntax.add_fraction ("NH4_fraction", Syntax::Const, "\
Ammonium fraction of total N in fertilizer. \n\
The remaining nitrogen is assumed to be nitrate.");
	syntax.add_fraction ("NH4_evaporation", Syntax::OptionalConst, "\
Obsolete alias for 'volatilization'.");
	syntax.add_fraction ("volatilization", Syntax::Const, "\
Fraction of NH4 that evaporates on application.");
	alist.add ("volatilization", 0.0);
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
	layer_syntax.add ("end", "cm", Check::negative (), Syntax::Const,
			  "\
Height where this layer ends (a negative number).");
	layer_syntax.add ("weight", "kg C/m^2", Check::non_negative (),
			  Syntax::Const, "Carbon in this layer.");
	layer_syntax.order ("end", "weight");
	syntax.add ("layers", layer_syntax, Syntax::Sequence, "\
Carbon content in different soil layers.  The carbon is assumed to be\n\
uniformly distributed in each layer.");
	syntax.add_submodule_sequence ("om", Syntax::State,
				       "The individual AOM pools.",
				       OM::load_syntax);
	Librarian<AM>::add_type ("initial", alist, syntax, &make);
      }
      // Root initialization,
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList (AM::default_root ());
	syntax.add_check (check_root);
	syntax.add ("creation", Syntax::Date, Syntax::State,
		    "Start of simulation.");
	syntax.add ("depth", "cm", Check::negative (), 
		    Syntax::OptionalConst, "\
How far down does the old root reach? (a negative number)\n\
By default, the soils maximal rooting depth will be used.");
	syntax.add ("dist", "cm", Check::positive (), Syntax::Const, "\
Distance to go down in order to decrease the root density to half the\n\
original.");
	syntax.add ("weight", "T DM/ha", Check::non_negative (), Syntax::Const, 
		    "Total weight of old root dry matter.");
	syntax.add_fraction ("total_C_fraction", Syntax::Const, 
			     "Carbon fraction of total root dry matter");
	syntax.add_fraction ("total_N_fraction", Syntax::Const, 
			     "Nitrogen fraction of total root dry matter");
	syntax.add_submodule_sequence ("om", Syntax::State,
				       "The individual AOM pools.",
				       OM::load_syntax);
	Librarian<AM>::add_type ("root", alist, syntax, &make);
      }
    }
} am_syntax;
