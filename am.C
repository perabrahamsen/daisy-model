// am.C -- Added Matter, i.e. fertilizer and residuals.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002, 2006 Per Abrahamsen and KVL.
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

#include "am.h"
#include "aom.h"
#include "chemical.h"
#include "metalib.h"
#include "library.h"
#include "submodeler.h"
#include "frame_submodel.h"
#include "time.h"
#include "log.h"
#include "geometry.h"
#include "check.h"
#include "vcheck.h"
#include "mathlib.h"
#include "program.h"
#include "memutils.h"
#include "librarian.h"
#include "unit.h"
#include "treelog.h"
#include "syntax.h"
#include <numeric>
#include <sstream>

const char *const AM::component = "am";

symbol
AM::library_id () const
{
  static const symbol id (component);
  return id;
}

struct AM::Implementation
{
  // Check.
  struct Check_OM_Pools : public VCheck
  {
    // Check that the OM pools are correct for organic fertilizer.
    void check (const Metalib&, 
		const Syntax& syntax, const AttributeList& alist, 
		const symbol key) const throw (std::string);
  };

  // Content.
  bool initialized;             // Whether initialize_derived has been called.
  const Time creation;		// When it was created.
  const symbol name;		// What is was.
  const std::vector<AOM*> om;		// Organic matter pool.

  // Use this if a living crop is adding to this AM.
  struct Lock;
  const Lock* lock;
  void unlock ();		// Crop died.
  bool locked () const;		// Test if this AM can be safely removed.
  symbol crop_name () const; // Name of locked crop.
  symbol crop_part_name () const; // Name of locked crop part.

  // Simulation.
  void output (Log&) const;
  bool check (Treelog& err) const;
  void mix (const Geometry&, double from, double to, double penetration,
            double& tillage_N_top, double& tillage_C_top,
            std::vector<double>& tillage_N_soil, std::vector<double>& tillage_C_soil,
            double dt);
  void mix (const Geometry&, const Volume&, double penetration,
            double& tillage_N_top, double& tillage_C_top,
            std::vector<double>& tillage_N_soil, std::vector<double>& tillage_C_soil,
            double dt);
  void mix (const Geometry&, double from, double to);
  void mix (const Geometry&, const Volume&);
  void swap (const Geometry&, double from, double middle, double to,
             std::vector<double>& tillage_N_soil, std::vector<double>& tillage_C_soil,
             double dt);
  double total_C (const Geometry& geo) const;
  double total_N (const Geometry& geo) const;
  double C_at (size_t at) const;
  double N_at (size_t at) const;
  void pour (std::vector<double>& cc, std::vector<double>& nn); // Move content to cc&nn.
  void append_to (std::vector<AOM*>& added); // Add OM's to added.
  void distribute (double C, std::vector<double>& om_C, // Helper for 'add' fns.
		   double N, std::vector<double>& om_N);
  void add (double C, double N);// Add dead leafs.
  void add (const Geometry& geo, AM::Implementation& other);
  void add_surface (const Geometry&,	// Add dead roots.
                    double C, double N, 
                    const std::vector<double>& density);
  double top_C () const;
  double top_N () const;
  void multiply_top (double fraction);

  // Create and Destroy.
  Implementation (bool initialized, 
                  const Time& c, symbol n, const std::vector<AOM*>& o);
  ~Implementation ();
};

void
AM::Implementation::Check_OM_Pools::check (const Metalib&, 
					   const Syntax& syntax, 
					   const AttributeList& alist, 
					   const symbol key) 
  const throw (std::string)
{ 
  daisy_assert (alist.check (key));
  daisy_assert (syntax.lookup (key) == Value::AList);
  daisy_assert (!syntax.is_log (key));
  daisy_assert (syntax.size (key) == Value::Sequence);

  if (alist.flag ("initialized", false))
    // No checking checkpoints.
    return;

  const std::vector<const AttributeList*>& om_alist 
    = alist.alist_sequence (key);
  int missing_initial_fraction = 0;
  int missing_C_per_N = 0;
  double total_fractions = 0.0;
  bool same_unspecified = false;
  for (size_t i = 0; i < om_alist.size (); i++)
    {
      if (!om_alist[i]->check ("initial_fraction"))
	missing_initial_fraction++;
      else 
	total_fractions += om_alist[i]->number ("initial_fraction");
      if (!om_alist[i]->check ("C_per_N"))
	missing_C_per_N++;

      if (!om_alist[i]->check ("initial_fraction") 
	  && !om_alist[i]->check ("C_per_N"))
	same_unspecified = true;
    }
  daisy_assert (total_fractions >= 0.0);
  if (total_fractions < 1e-10 && !same_unspecified)
    throw std::string ("you should specify at least one non-zero fraction");
  if (approximate (total_fractions, 1.0))
    throw std::string ("sum of specified fractions should be < 1.0");
  if (total_fractions > 1.0)
    throw std::string ("sum of fractions should be < 1.0");
  if (missing_initial_fraction != 1)
    throw std::string ("you should leave initial_fraction in one om unspecified");
  if (missing_C_per_N != 1)
    throw std::string ("You must leave C/N unspecified in exactly one pool");
}

struct AM::Implementation::Lock
{ 
  // Content.
  symbol crop;
  symbol part;

  // Simulation.
  void output (Log&) const;
    
  // Create and Destroy.
  static void load_syntax (Frame&);
  Lock (symbol c, symbol p);
  Lock (const AttributeList& al);
};

void 
AM::Implementation::Lock::output (Log& log) const
{
  output_variable (crop, log);
  output_variable (part, log);
}  

void
AM::Implementation::Lock::load_syntax (Frame& frame)
{
  frame.add ("crop", Value::String, Value::State, 
	      "Crop to which this am is locked");
  frame.add ("part", Value::String, Value::State, 
	      "Crop part to which this am is locked");
}

AM::Implementation::Lock::Lock (symbol c, symbol p)
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
  daisy_assert (lock != NULL);
  delete lock;
  lock = NULL;
}

bool 
AM::Implementation::locked () const
{ return lock != NULL; }

symbol
AM::Implementation::crop_name () const
{ 
  daisy_assert (lock);
  return lock->crop;
}

symbol
AM::Implementation::crop_part_name () const
{
  daisy_assert (lock);
  return lock->part;
}

void
AM::Implementation::distribute (double C, std::vector<double>& om_C, 
				double N, std::vector<double>& om_N)
{
  daisy_assert (C >= 0.0);
  daisy_assert (N >= 0.0);
  daisy_assert (!std::isnormal (C) || N > 0.0);

  // Fill out the blanks.
  int missing_fraction = -1;
  int missing_C_per_N = -1;
  
  for (size_t i = 0; i < om.size (); i++)
    {
      const double fraction = om[i]->initial_fraction;
      const double C_per_N = om[i]->initial_C_per_N;

      if (!approximate (fraction, OM::Unspecified))
	{
	  om_C[i] = C * fraction;
	  daisy_assert (om_C[i] >= 0.0);

	  if (!approximate (C_per_N, OM::Unspecified))
	    {
	      om_N[i] = om_C[i] / C_per_N;
	      daisy_assert (om_N[i] >= 0.0);
	    }
	  else
	    {
	      daisy_assert (missing_C_per_N < 0);
	      missing_C_per_N = i;
	    }
	}
      else
	{
	  daisy_assert (missing_fraction < 0);
	  missing_fraction = i;
	  if (approximate (om[i]->initial_C_per_N, OM::Unspecified))
	    {
	      daisy_assert (missing_C_per_N < 0);
	      missing_C_per_N = i;
	    }
	}
    }
  daisy_assert (missing_C_per_N > -1);
  daisy_assert (missing_fraction > -1);

  // Calculate C in missing fraction.
  om_C[missing_fraction] = C - accumulate (om_C.begin (), om_C.end (), 0.0);
  daisy_assert (om_C[missing_fraction] >= 0.0);

  // Calculate N in missing C/N.
  if (missing_fraction != missing_C_per_N)
    {
      const double C_per_N = om[missing_fraction]->initial_C_per_N;
      daisy_assert (C_per_N >= 0.0);
      om_N[missing_fraction] = om_C[missing_fraction] / C_per_N;
      daisy_assert (om_N[missing_fraction] >= 0.0);
    }
  om_N[missing_C_per_N] = N - accumulate (om_N.begin (), om_N.end (), 0.0);

  if (om_N[missing_C_per_N] < 0.0)
    {
      // Too little N, distribute evenly.
      for (size_t i = 0; i < om.size (); i++)
	{
	  const double fraction = om[i]->initial_fraction;
	  if (approximate (fraction, OM::Unspecified))
	    {
	      daisy_assert (i == missing_fraction);
	      om_N[i] = 0.0;
	    }
	  else
	    om_N[i] = N * fraction;
	}
      om_N[missing_fraction]
	= N - accumulate (om_N.begin (), om_N.end (), 0.0);
      daisy_assert (om_N[missing_fraction] >= 0.0);
    }
  daisy_assert (om_N[missing_C_per_N] >= 0.0);

  daisy_assert (approximate (C, accumulate (om_C.begin (), om_C.end (), 0.0)));
  daisy_assert (approximate (N, accumulate (om_N.begin (), om_N.end (), 0.0)));
}

void
AM::Implementation::add (double C, double N)
{
  daisy_assert (C >= 0);
  daisy_assert (N >= 0);
  std::vector<double> om_C (om.size (), 0.0);
  std::vector<double> om_N (om.size (), 0.0);

  distribute (C, om_C, N, om_N);

  for (size_t i = 0; i < om.size (); i++)
    om[i]->add (om_C[i], om_N[i]);
}

void
AM::Implementation::add (const Geometry& geo,
			 AM::Implementation& other)
{
  daisy_assert (&other != this);
  const double old_C = total_C (geo) + other.total_C (geo);
  const double old_N = total_N (geo) + other.total_N (geo);

  std::vector<double> cc (geo.cell_size (), 0.0);
  std::vector<double> nn (geo.cell_size (), 0.0);
  other.pour (cc, nn);

  for (size_t at = 0; at < geo.cell_size (); at++)
    {
      std::vector<double> om_C (om.size (), 0.0);
      std::vector<double> om_N (om.size (), 0.0);

      distribute (cc[at], om_C, nn[at], om_N);

      for (size_t i = 0; i < om.size (); i++)
	om[i]->add (at, om_C[i], om_N[i]);
    }

  add (other.top_C (), other.top_N ());

  const double new_C = total_C (geo);
  const double new_N = total_N (geo);
  daisy_assert (approximate (old_C, new_C));
  daisy_assert (approximate (old_N, new_N));
}

void
AM::Implementation::add_surface (const Geometry& geo, 
                                 double C, double N,
                                 const std::vector<double>& density)
{
  daisy_assert (C >= 0);
  daisy_assert (N >= 0);
  C *= geo.surface_area ();
  N *= geo.surface_area ();
  const double old_C = total_C (geo);
  const double old_N = total_N (geo);

  std::vector<double> om_C (om.size (), 0.0);
  std::vector<double> om_N (om.size (), 0.0);

  distribute (C, om_C, N, om_N);

  for (size_t i = 0; i < om.size (); i++)
    om[i]->add (geo, om_C[i], om_N[i], density);

  daisy_assert (approximate (old_C + C, total_C (geo)));
  daisy_assert (approximate (old_N + N, total_N (geo)));
}

double
AM::Implementation::top_C () const
{ 
  double total = 0.0;
  for (size_t i = 0; i < om.size (); i++)
    total += om[i]->top_C;
  return total;
}

double 
AM::Implementation::top_N () const
{ 
  double total = 0.0;
  for (size_t i = 0; i < om.size (); i++)
    total += om[i]->top_N;
  return total;
}

void 
AM::Implementation::multiply_top (double fraction)
{ 
  for (size_t i = 0; i < om.size (); i++)
    {
      om[i]->top_N *= fraction;
      om[i]->top_C *= fraction;
    }
}

void 
AM::Implementation::append_to (std::vector<AOM*>& added)
{
  for (unsigned i = 0; i < om.size (); i++)
    added.push_back (om[i]);
}

void
AM::Implementation::output (Log& log) const
{ 
  output_variable (initialized, log);
  if (creation != Time (1, 1, 1, 1))
    output_submodule (creation, "creation", log);
  output_variable (name, log);
  if (lock)
    output_submodule (*lock, "lock", log);
  output_ordered (om, "om", log);
}

bool 
AM::Implementation::check (Treelog& /*err*/) const
{ 
  bool ok = true;
  return ok;
}

void 
AM::Implementation::mix (const Geometry& geo,
			 double from, double to, double penetration,
                         double& tillage_N_top, double& tillage_C_top,
                         std::vector<double>& tillage_N_soil, 
                         std::vector<double>& tillage_C_soil, 
                         const double dt)
{
  const double old_C = total_C (geo);
  const double old_N = total_N (geo);

  for (size_t i = 0; i < om.size (); i++)
    {
      om[i]->penetrate (geo, from, to, penetration, 
                        tillage_N_top, tillage_C_top, 
                        tillage_N_soil, tillage_C_soil);
      om[i]->mix (geo, from, to, 
                  tillage_N_soil, tillage_C_soil, dt);
    }
  const double new_C = total_C (geo);
  const double new_N = total_N (geo);
  
  daisy_assert (approximate (new_C, old_C));
  daisy_assert (approximate (new_N, old_N));
}

void 
AM::Implementation::mix (const Geometry& geo, 
                         const Volume& volume, double penetration,
                         double& tillage_N_top, double& tillage_C_top,
                         std::vector<double>& tillage_N_soil, 
                         std::vector<double>& tillage_C_soil, 
                         const double dt)
{
  const double old_C = total_C (geo);
  const double old_N = total_N (geo);

  for (size_t i = 0; i < om.size (); i++)
    {
      om[i]->penetrate (geo, volume, penetration, 
                        tillage_N_top, tillage_C_top, 
                        tillage_N_soil, tillage_C_soil);
      om[i]->mix (geo, volume, tillage_N_soil, tillage_C_soil, dt);
    }
  const double new_C = total_C (geo);
  const double new_N = total_N (geo);
  
  daisy_assert (approximate (new_C, old_C));
  daisy_assert (approximate (new_N, old_N));
}

void
AM::Implementation::swap (const Geometry& geo,
			  const double from, const double middle, 
                          const double to,
                          std::vector<double>& tillage_N_soil,
                          std::vector<double>& tillage_C_soil,
                          const double dt)
{
  const double old_C = total_C (geo);
  const double old_N = total_N (geo);

  for (size_t i = 0; i < om.size (); i++)
    om[i]->swap (geo, from, middle, to, 
                 tillage_N_soil, tillage_C_soil, dt);

  const double new_C = total_C (geo);
  const double new_N = total_N (geo);
  
  daisy_assert (approximate (new_C, old_C));
  daisy_assert (approximate (new_N, old_N));
}

double 
AM::Implementation::total_C (const Geometry& geo) const
{
  double total = 0.0;
  for (size_t i = 0; i < om.size (); i++)
    total += om[i]->full_C (geo);
  return total;
}

double 
AM::Implementation::total_N (const Geometry& geo) const
{
  double total = 0.0;
  for (size_t i = 0; i < om.size (); i++)
    total += om[i]->full_N (geo);
  return total;
}

double 
AM::Implementation::C_at (const size_t at) const
{
  double total = 0.0;
  for (size_t i = 0; i < om.size (); i++)
    total += om[i]->C_at (at);
  return total;
}

double 
AM::Implementation::N_at (const size_t at) const
{
  double total = 0.0;
  for (size_t i = 0; i < om.size (); i++)
    total += om[i]->N_at (at);
  return total;
}

void 
AM::Implementation::pour (std::vector<double>& cc, std::vector<double>& nn)
{
  for (size_t i = 0; i < om.size (); i++)
    om[i]->pour (cc, nn);
}

AM::Implementation::Implementation (const bool i,
                                    const Time& c, const symbol n,
				    const std::vector<AOM*>& o)
  : initialized (i),
    creation (c),
    name (n),
    om (o),
    lock (NULL)
{ }

AM::Implementation::~Implementation ()
{ 
  if (lock)
    delete lock;
  sequence_delete (om.begin (), om.end ()); 
}

symbol
AM::real_name () const
{ return impl->name; }

void
AM::output (Log& log) const
{ impl->output (log); }

void 
AM::append_to (std::vector<AOM*>& added)
{ impl->append_to (added); }

bool 
AM::check (Treelog& err) const
{ return impl->check (err); }

void 
AM::mix (const Geometry& geo,
	 const double from, const double to, const double penetration,
         double& tillage_N_top, double& tillage_C_top,
         std::vector<double>& tillage_N_soil,
	 std::vector<double>& tillage_C_soil,
         const double dt)
{ impl->mix (geo, from, to, penetration, 
             tillage_N_top, tillage_C_top, 
             tillage_N_soil, tillage_C_soil, dt); }

void 
AM::mix (const Geometry& geo,
	 const Volume& volume, const double penetration,
         double& tillage_N_top, double& tillage_C_top,
         std::vector<double>& tillage_N_soil,
	 std::vector<double>& tillage_C_soil,
         const double dt)
{ impl->mix (geo, volume, penetration, 
             tillage_N_top, tillage_C_top, 
             tillage_N_soil, tillage_C_soil, dt); }

void
AM::swap (const Geometry& geo,
	  const double from, const double middle, const double to,
          std::vector<double>& tillage_N_soil,
	  std::vector<double>& tillage_C_soil,
          const double dt)
{ impl->swap (geo, from, middle, to, tillage_N_soil, tillage_C_soil, dt); }

double 
AM::total_C (const Geometry& geo) const
{ return impl->total_C (geo); }

double 
AM::total_N (const Geometry& geo) const
{ return impl->total_N (geo); }

double 
AM::C_at (size_t at) const
{ return impl->C_at (at); }

double 
AM::N_at (size_t at) const
{ return impl->N_at (at); }

void 
AM::pour (std::vector<double>& cc, std::vector<double>& nn)
{ impl->pour (cc, nn); }

void 
AM::add (double C, double N)
{ impl->add (C, N); }

void 
AM::add (const Geometry& geo, AM& other)
{ impl->add (geo, *other.impl); }

void 
AM::add_surface (const Geometry& geo,
                 double C, double N, 
                 const std::vector<double>& density)
{ impl->add_surface (geo, C, N, density); }

double
AM::top_C () const
{ return impl->top_C (); }

double 
AM::top_N () const
{ return impl->top_N (); }

void 
AM::multiply_top (double fraction)
{ impl->multiply_top (fraction); }

void 
AM::unlock ()
{ impl->unlock (); }

bool 
AM::locked () const
{ return impl->locked (); }

symbol
AM::crop_name () const
{ return impl->crop_name (); }

symbol
AM:: crop_part_name () const
{ return impl->crop_part_name (); }

const VCheck& 
AM::check_om_pools ()
{ 
  static Implementation::Check_OM_Pools check;
  return check;
}

#if 1
AM& 
AM::create (Metalib& metalib, const AttributeList& al1 , const Geometry& geo, 
            const Time& now, const double max_rooting_depth, Treelog& msg)
{ 
  AttributeList al2 (al1);
  if (!al2.check ("type"))
    al2.add ("type", "state");
  if (!al2.check ("creation"))
    {
      AttributeList time_alist;
      now.set_alist (time_alist);
      al2.add ("creation", time_alist);
    }
  if (!al2.check ("initialized"))
    al2.add ("initialized", false);
  AM& am = *Librarian::build_free<AM> (metalib, msg, al2, "fertilizer");
  am.initialize (geo, max_rooting_depth);
  return am;
}

// Crop part.
AM& 
AM::create (Metalib& metalib, const Geometry& geo, const Time& time,
	    const std::vector<const AttributeList*>& ol,
	    const symbol sort, const symbol part,
	    AM::lock_type lock, Treelog& msg)
{
  const Library& library = metalib.library (AM::component);
  const Frame& frame = library.frame ("state");
  AttributeList al (frame.alist ());
  al.add ("type", "state");
  al.add ("initialized", true);
  AttributeList new_time;
  time.set_alist (new_time);
  al.add ("creation", new_time);
  al.add ("name", sort + "/" + part);
  al.add ("om", ol);
  AM& am = *Librarian::build_free<AM> (metalib, msg, al, "crop part");
  for (size_t i = 0; i < am.impl->om.size (); i++)
    am.impl->om[i]->initialize (geo.cell_size ());
  if (lock == AM::Locked)
    am.impl->lock = new AM::Implementation::Lock (sort, part);
  return am;
}

#else
AM& 
AM::create (Metalib&, const AttributeList& al1 , const Geometry& geo, 
            const Time& now, const double max_rooting_depth, Treelog&)
{ 
  AttributeList al2 (al1);
  al2.add ("type", "state");
  if (!al2.check ("creation"))
    {
      AttributeList time_alist;
      now.set_alist (time_alist);
      al2.add ("creation", time_alist);
    }
  if (!al2.check ("initialized"))
    al2.add ("initialized", false);
  AM& am = *new AM (al2); 
  am.initialize (geo, max_rooting_depth);
  return am;
}

// Crop part.
AM& 
AM::create (Metalib&, const Geometry& geo, const Time& time,
	    const std::vector<const AttributeList*>& ol,
	    const symbol sort, const symbol part,
	    AM::lock_type lock, Treelog& msg)
{
  AttributeList al;
  al.add ("type", "state");
  AttributeList new_time;
  time.set_alist (new_time);
  al.add ("creation", new_time);
  al.add ("name", sort + "/" + part);
  al.add ("om", ol);
  al.add ("initialized", false);
  AM& am = *new AM (al);
  for (size_t i = 0; i < am.impl->om.size (); i++)
    am.impl->om[i]->initialize (geo.cell_size ());
  if (lock == AM::Locked)
    am.impl->lock = new AM::Implementation::Lock (sort, part);
  return am;
}
#endif

const std::vector<const AttributeList*>&
AM::default_AM ()
{
  static std::vector<const AttributeList*> am;

  if (am.size () < 1)
    {
      FrameSubmodel aom_frame (AOM::load_syntax);
      const AttributeList& aom_alist = aom_frame.alist ();
      AttributeList& AOM1 = *new AttributeList (aom_alist);
      AttributeList& AOM2 = *new AttributeList (aom_alist);
      AOM1.add ("initial_fraction", 0.80);
      std::vector<double> CN;
      CN.push_back (90.0);
      AOM1.add ("C_per_N", CN);
      std::vector<double> efficiency1;
      efficiency1.push_back (0.50);
      efficiency1.push_back (0.50);
      AOM1.add ("efficiency", efficiency1);
      AOM1.add ("turnover_rate", 2.0e-4);
      std::vector<double> fractions1;
#if 1 // SANDER_PARAMS
      fractions1.push_back (0.00);
      fractions1.push_back (1.00);
#else
      fractions1.push_back (0.50);
      fractions1.push_back (0.50);
#endif
      fractions1.push_back (0.00);
      AOM1.add ("fractions", fractions1);
      std::vector<double> efficiency2;
      efficiency2.push_back (0.50);
      efficiency2.push_back (0.50);
      AOM2.add ("efficiency", efficiency2);
      AOM2.add ("turnover_rate", 2.0e-3);
      std::vector<double> fractions2;
      fractions2.push_back (0.00);
      fractions2.push_back (1.00);
      fractions2.push_back (0.00);
      AOM2.add ("fractions", fractions2);
      am.push_back (&AOM1);
      am.push_back (&AOM2);
    }
  return am;
}

double
AM::get_NO3 (const Metalib& metalib, const AttributeList& al)
{
  if (al.check ("weight"))
    {
      if (is_organic (metalib, al))
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 0.01;			// T w.w. / ha --> g / cm²
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NO3_fraction");
	}
      // Mineral fertilizer.
      daisy_assert (is_mineral (metalib, al));
      return al.number ("weight")
	* (1.0 - al.number ("NH4_fraction"))
	* (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0))); // kg/ha -> g/cm^2
    }
  // Other.
  return al.number ("NO3");
}

double
AM::get_NH4 (const Metalib& metalib, const AttributeList& al)
{
  daisy_assert (IM::storage_unit () == symbol ("g/cm^2"));

  if (al.check ("weight"))
    {
      const double volatilization = al.number ("volatilization");

      if (is_organic (metalib, al))
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 0.01;			// T w.w. / ha --> g / cm²
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NH4_fraction") * (1.0 - volatilization);
	}
      // Mineral fertilizer.
      daisy_assert (is_mineral (metalib, al));
      
      return al.number ("weight")
	* al.number ("NH4_fraction") * (1.0 - volatilization)
	* (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0))); // kg/ha -> g/cm^2
    }
  // Other.
  daisy_assert (!al.check ("volatilization"));
  return al.number ("NH4");
}

IM
AM::get_IM (const Metalib& metalib, const Unit& unit, const AttributeList& al)
{
  daisy_assert (unit.native_name () == IM::storage_unit ());
  IM result (unit);
  result.set_value (Chemical::NH4 (), unit, get_NH4 (metalib, al));
  result.set_value (Chemical::NO3 (), unit, get_NO3 (metalib, al));
  return result;
}

double
AM::get_volatilization (const Metalib& metalib, const AttributeList& al)
{
  if (al.check ("weight"))
    {
      const double volatilization = al.number ("volatilization");

      if (is_organic (metalib, al))
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 100.0;			// T w.w. / ha --> g / m^2
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NH4_fraction") * volatilization;
	}
      // Mineral fertilizer.
      daisy_assert (is_mineral (metalib, al));
      
      return al.number ("weight")
	* al.number ("NH4_fraction") * volatilization; 
    }
  // Other.
  daisy_assert (!al.check ("volatilization"));
  return 0.0;
}

double
AM::get_DM (const Metalib& metalib, const AttributeList& al)	// [Mg DM/ha]
{
  if (al.check ("weight") && is_organic (metalib, al))
    return al.number ("weight") * al.number ("dry_matter_fraction");

  return 0.0;
}

double
AM::get_water (const Metalib& metalib, const AttributeList& al)	// [mm]
{
  if (al.check ("weight") && is_organic (metalib, al))
    return al.number ("weight")
      * (1.0  - al.number ("dry_matter_fraction"))
      * 0.1;                    // t/ha -> mm

  return 0.0;
}

void
AM::set_utilized_weight (const Metalib& metalib, 
                         AttributeList& am, const double weight)
{
  if (is_mineral (metalib, am))
    am.add ("weight", weight);
  else
    {
      daisy_assert (is_organic (metalib, am));
      daisy_assert (am.check ("total_N_fraction"));
      daisy_assert (am.check ("dry_matter_fraction"));
      const double N_fraction = am.number ("total_N_fraction");
      const double utilization = am.number ("first_year_utilization", 1.0);
      const double dry_matter_fraction = am.number ("dry_matter_fraction");
      const double kg_per_ton = 1000.0;
      am.add ("weight", weight 
	      / (dry_matter_fraction *N_fraction * utilization * kg_per_ton));
    }
}

double
AM::utilized_weight (const Metalib& metalib, const AttributeList& am)
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
  else if (is_mineral (metalib, am))
    return am.number ("weight");

  return 0.0;
}

double
AM::second_year_utilization (const Metalib&, const AttributeList& am)
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

void
AM::set_mineral (const Metalib&, AttributeList& am, double NH4, double NO3)
{
  am.add ("syntax", "mineral");
  const double total_N = NH4 + NO3;
  am.add ("weight", total_N);
  am.add ("NH4_fraction", (total_N > 0.0) ? NH4 / total_N : 0.0);
  am.add ("volatilization", 0.0);
}

bool 
AM::is_fertilizer (const Metalib& metalib, const AttributeList& am) 
{ return is_mineral (metalib, am) || is_organic (metalib, am); }

bool 
AM::is_mineral (const Metalib& metalib, const AttributeList& am)
{ 
  Library& library = metalib.library (component);
  const bool m1 = library.is_derived_from (am.name ("type"), "mineral");
  const bool m2 = am.name ("syntax") == "mineral"; 
  daisy_assert (m1 == m2);
  return m1;
}

bool 
AM::is_organic (const Metalib& metalib, const AttributeList& am)
{ 
  Library& library = metalib.library (component);
  const bool m1 = library.is_derived_from (am.name ("type"), "organic");
  const bool m2 = am.name ("syntax") == "organic"; 
  daisy_assert (m1 == m2);
  return m1;
}

AM::AM (Block& al)
  : ModelAListed (al.alist ()),
    impl (new Implementation 
	  (al.flag ("initialized"),
           al.check ("creation")
	   ? Time (al.alist ("creation"))
	   : Time (1, 1, 1, 1),
           al.check ("name") ? al.name ("name") : al.name ("type"),
	   map_construct<AOM> (al.alist_sequence ("om"))))
{
  if (al.check ("lock"))
    impl->lock = new AM::Implementation::Lock (al.alist ("lock"));
}

void
AM::initialize (const Geometry& geo, const double max_rooting_depth)
{
  for (size_t i = 0; i < impl->om.size (); i++)
    impl->om[i]->initialize (geo.cell_size ());

  if (alist.check ("lock"))
    impl->lock = new Implementation::Lock (alist.alist ("lock"));

  if (!impl->initialized)
    {
      impl->initialized = true;
      initialize_derived (geo, max_rooting_depth);
    }
}

void
AM::initialize_derived (const Geometry& geo, const double max_rooting_depth)
{
  const symbol syntax = alist.name ("syntax");

  if (syntax == "state")
    /* Do nothing */;
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
    daisy_notreached ();
  else if (syntax == "crop")
    daisy_notreached ();
  else if (syntax == "initial")
    {
      const std::vector<const AttributeList*>& oms
	= alist.alist_sequence ("om");
      const std::vector<AOM*>& om = impl->om;
      
      const std::vector<const AttributeList*>& layers
	= alist.alist_sequence ("layers");
      
      double last = 0.0;
      for (size_t i = 0; i < layers.size (); i++)
	{
	  const double end = layers[i]->number ("end");
	  const double weight = layers[i]->number ("weight"); // Kg C/m²
	  const double C = weight * 1000.0 / (100.0 * 100.0); // g C / cm²
	  int missing_number = -1;
	  double missing_fraction = 1.0;
	  for (size_t j = 0; j < oms.size (); j++)
	    {
	      if (oms[j]->check  ("initial_fraction"))
		{
		  const double fraction = oms[j]->number ("initial_fraction");
		  daisy_assert (fraction >= 0.0);
		  daisy_assert (fraction <= 1.0);
		  missing_fraction -= fraction;
		  geo.add_surface (om[j]->C, last, end, C * fraction);
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
		geo.add_surface (om[missing_number]->C, 
                                 last, end, C * missing_fraction);
	    }
	  else if (missing_fraction < -0.1e-10)
	    throw ("Specified more than all C in om in initial am");
	  else if (missing_fraction > 0.1e-10)
	    throw ("Specified less than all C in om in initial am");
	  
	  last = end;
	}
      // Fill N to match C.
      for (size_t i = 0; i < om.size (); i++)
	{
	  daisy_assert (om[i]->initial_C_per_N > 0);
	  while (om[i]->N.size () < om[i]->C.size ())
	    om[i]->N.push_back (om[i]->C[om[i]->N.size ()] 
			        / om[i]->initial_C_per_N);
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
      const double depth = alist.number ("depth", max_rooting_depth);
      daisy_assert (depth < 0.0);

      // Calculate density.
      std::vector<double> density (geo.cell_size (), 0.0);
      for (size_t i = 0; i < geo.cell_size (); i++)
        if (geo.cell_z (i) > depth)
          density[i] = k * exp (k * geo.cell_z (i));

      // Add it.
      impl->add_surface (geo, C, N, density);
    }
}

AM::~AM ()
{ }

static struct AMInit : public DeclareComponent 
{
  AMInit ()
    : DeclareComponent (AM::component, "\
The 'am' component describes various kinds of fertilizer and other\n\
added matter such as crop residues.  In particular, it describes how\n\
they decompose.")
  { }
} AM_init;

static struct AMMineralSyntax : public DeclareBase
{
  AMMineralSyntax ()
    : DeclareBase (AM::component, "mineral", "Mineral fertilizer.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.add ("weight", "kg N/ha", Check::non_negative (), Value::Const,
                "Amount of fertilizer applied.");
    frame.add ("weight", 0.0);
    frame.add_fraction ("NH4_fraction", Value::Const, "\
Ammonium fraction of total N in fertilizer. \n\
The remaining nitrogen is assumed to be nitrate.");
    frame.add_fraction ("volatilization", Value::Const, "\
Fraction of NH4 that evaporates on application.");
    frame.add ("volatilization", 0.0);
    frame.alist ().add ("syntax", "mineral");
  }
} AMMineral_syntax;

static struct AMBaseSyntax : public DeclareBase
{
  AMBaseSyntax ()
    : DeclareBase (AM::component, "base", "\
Common attributes for all added organic matter models.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add ("initialized", Value::Boolean, Value::State, "\
True if this AM has been initialized.\n\
It will usually be false in user setup files, but true in checkpoints.");
    frame.add ("initialized", false);
    frame.add_submodule ("creation", Value::OptionalState, 
                         "Time this AM was created.", Time::load_syntax);
    frame.add ("name", Value::String, Value::OptionalState, "\
A name given to this AOM so you can identify it in for example log files.");
    frame.add_submodule ("lock", Value::OptionalState, "\
This AM belongs to a still living plant",
                          AM::Implementation::Lock::load_syntax);
    frame.add_submodule_sequence ("om", Value::OptionalState, 
                                  "The individual AOM pools.",
                                  AOM::load_syntax);
  }
} AMBase_syntax;

static struct AMStateSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new AM (al); }
  AMStateSyntax ()
    : DeclareModel (AM::component, "state", "base", "\
Most AM models are only used for initialization, they will be comnverted\n\
to this generic model after creation, so this is what you will see in a\n\
checkpoint.  This model contains a number (typically 2) of separate\n\
pools, each of which have their own turnover rate.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.alist ().add ("syntax", "state");
  }
} AMState_syntax;

static struct AMOrganicSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new AM (al); }
  AMOrganicSyntax ()
    : DeclareModel (AM::component, "organic", "base", "\
Organic fertilizer, typically slurry or manure from animals.")
  { }
  static bool check_alist (const AttributeList& al, Treelog& err)
  { 
    if (!al.check ("syntax"))
      {
	err.entry ("no syntax");
	return false;
      }

    const symbol syntax = al.name ("syntax");
    daisy_assert (syntax == "organic");
    return true;
  }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.add_check (check_alist);
    frame.alist ().add ("syntax", "organic");
    frame.add ("weight", "Mg w.w./ha", Check::non_negative (),
                Value::Const,
                "Amount of fertilizer applied.");
    frame.add ("weight", 0.0);
    frame.add_fraction ("first_year_utilization", Value::OptionalConst, 
                         "\
Estimated useful N fraction for the first year.\n\
In Denmark, this is governed by legalisation.");
    frame.add_fraction ("second_year_utilization", 
                         Value::OptionalConst, "\
Estimated useful N fraction for the second year.\n\
In Denmark, this is governed by legalisation.");
    frame.add_fraction ("dry_matter_fraction", Value::Const,
                         "Dry matter fraction of total (wet) weight.");
    frame.add_fraction ("total_C_fraction", Value::Const,
                         "Carbon fraction of dry matter.");
    frame.add_fraction ("total_N_fraction", Value::Const,
                         "Nitrogen fraction of dry matter.");
    frame.add_check ("om", AM::check_om_pools ());
    frame.add_fraction ("NO3_fraction", Value::Const, 
                         "Nitrate fraction of total N in fertilizer. \n\
The remaining nitrogen is assumed to be ammonium or organic.");
    frame.add ("NO3_fraction", 0.0);
    frame.add_fraction ("NH4_fraction", Value::Const, "\
Ammonium fraction of total N in fertilizer. \n\
The remaining nitrogen is assumed to be nitrate or organic.");
    frame.add ("NH4_fraction", 0.0);
    frame.add_fraction ("volatilization", Value::Const, "\
Fraction of NH4 that evaporates on application.");
    frame.add ("volatilization", 0.0);
  }
} AMOrganic_syntax;

static struct AMInitialSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new AM (al); }
  AMInitialSyntax ()
    : DeclareModel (AM::component, "initial", "base", "\
Initial added organic matter at the start of the simulation.")
  { }
  static bool check_alist (const AttributeList& al, Treelog& err)
  { 
    if (al.flag ("initialized", false))
      // No checking checkpoints.
    return true;

    daisy_assert (al.name ("syntax") == "initial");
  
    bool ok = true;

    // We need exactly one pool with unspecified OM.
    daisy_assert (al.check ("om"));
    const std::vector<const AttributeList*>& om = al.alist_sequence ("om");
    for (size_t i = 0; i < om.size (); i++)
      if (approximate (OM::get_initial_C_per_N (*om[i]), OM::Unspecified))
	{
	  std::ostringstream tmp;
	  tmp << "om[" << i << "]";
	  Treelog::Open nest (err, tmp.str ());
	  err.entry ("You must specify C/N for all pools.");
	  ok = false;
	}
    return ok;
  }

  static void load_layer (Frame& frame)
  {
    frame.add ("end", "cm", Check::negative (), Value::Const, "\
Height where this layer ends (a negative number).");
    frame.add ("weight", "kg C/m^2", Check::non_negative (),
               Value::Const, "Carbon in this layer.");
    frame.order ("end", "weight");
  }

  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.alist ().add ("syntax", "initial");
    frame.add_submodule_sequence ("layers", Value::Const, "\
Carbon content in different soil layers.  The carbon is assumed to be\n \
uniformly distributed in each layer.", load_layer);
  }
} AMInitial_syntax;

static struct AMRootSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new AM (al); }
  AMRootSyntax ()
    : DeclareModel (AM::component, "root", "base", "\
Initialization of old root remains.")
  { }
  static bool check_alist (const AttributeList& al, Treelog& err)
  { 
    if (al.flag ("initialized", false))
      // No checking checkpoints.
    return true;

    daisy_assert (al.name ("syntax") == "root");
  
    bool ok = true;

    // We need exactly one pool with unspecified OM.
    daisy_assert (al.check ("om"));
    int unspecified = 0;
    const std::vector<const AttributeList*>& om = al.alist_sequence ("om");
    for (size_t i = 0; i < om.size (); i++)
      if (approximate (OM::get_initial_C_per_N (*om[i]), OM::Unspecified))
	unspecified++;
    if (unspecified != 1)
      { 
	err.entry ("You must leave C/N unspecified in exactly one pool.");
	ok = false;
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.alist ().add ("syntax", "root");
    frame.add ("depth", "cm", Check::negative (), 
               Value::OptionalConst, "\
How far down does the old root reach? (a negative number)\n\
By default, the soils maximal rooting depth will be used.");
    frame.add ("dist", "cm", Check::positive (), Value::Const, "\
Distance to go down in order to decrease the root density to half the\n\
original.");
    frame.add ("dist", 7.0);
    frame.add ("weight", "Mg DM/ha", Check::non_negative (), Value::Const, 
               "Total weight of old root dry matter.");
    frame.add ("weight", 1.2);
    frame.add_fraction ("total_C_fraction", Value::Const, 
                        "Carbon fraction of total root dry matter");
    frame.add ("total_C_fraction", 0.40);
    frame.add_fraction ("total_N_fraction", Value::Const, 
                        "Nitrogen fraction of total root dry matter");
    frame.add ("total_N_fraction", 0.01);
    frame.add ("om", AM::default_AM ());
  }
} AMRoot_syntax;

struct ProgramAM_table : public Program
{
  const Library& library;

  // Use.
  bool run (Treelog& msg)
  {
    std::vector<symbol> entries;
    library.entries (entries);
    std::ostringstream tmp;
    tmp << "Name\tClass\tSuper\tFile\tNH4\tNO3\tvolatilization\tN\tC\tDM";
    for (std::vector<symbol>::const_iterator i = entries.begin ();
         i != entries.end ();
         i++)
      {
        tmp << "\n";
        const symbol name = *i;
        daisy_assert (library.check (name));
        const AttributeList& alist = library.lookup (name);
        // const Syntax& syntax = library.syntax (name);
        daisy_assert (alist.check ("syntax"));
        const symbol type = alist.name ("syntax");
        static const symbol buildin ("build-in");
        const symbol super = alist.check ("type") 
          ? alist.name ("type")
          : buildin;
        tmp << name << "\t" << type << "\t" << super << "\t";
        if (alist.check ("parsed_from_file"))
          tmp << alist.name ("parsed_from_file");
        tmp << "\t";
        if (alist.check ("NH4_fraction"))
          tmp << alist.number ("NH4_fraction");
        tmp << "\t";
        if (alist.check ("NO3_fraction"))
          tmp << alist.number ("NO3_fraction");
        tmp << "\t";
        if (alist.check ("volatilization"))
          tmp << alist.number ("volatilization");
        tmp << "\t";
        if (alist.check ("total_N_fraction"))
          tmp << alist.number ("total_N_fraction");
        tmp << "\t";
        if (alist.check ("total_C_fraction"))
          tmp << alist.number ("total_C_fraction");
        tmp << "\t";
        if (alist.check ("dry_matter_fraction"))
          tmp << alist.number ("dry_matter_fraction");
      }
    msg.message (tmp.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { };
  bool check (Treelog&)
  { return true; }
  ProgramAM_table (Block& al)
    : Program (al),
      library (al.metalib ().library (AM::component))
  { }
  ~ProgramAM_table ()
  { }
};

static struct ProgramAM_tableSyntax : DeclareModel
{
  Model* make (Block& al) const
  { return new ProgramAM_table (al); }
  ProgramAM_tableSyntax ()
    : DeclareModel (Program::component, "AM_table", "\
Generate a table of fertilizers.")
  { }
  void load_frame (Frame& frame) const
  { }
} ProgramAM_table_syntax;

// am.C ends here.
