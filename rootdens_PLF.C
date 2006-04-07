// rootdens_PLF.C -- Use piecewise linear functions for root density.
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


#include "rootdens.h"
#include "geometry.h"
#include "plf.h"
#include "submodeler.h"
#include "check.h"
#include "vcheck.h"
#include "mathlib.h"
#include "memutils.h"

using namespace std;

struct Rootdens_PLF : public Rootdens
{
  // Parameters.
  // Check.
  struct Check_Indexes : public VCheck
  {
    // Check that the indexes are monotonically increasing.
    void check (const Syntax& syntax, const AttributeList& alist, 
		const string& key) const throw (string);
  };

  struct Entry
  {
    // Parameters.
    const double index;
    const PLF& density;
    
    // Create and Destroy.
    static void load_syntax (Syntax&, AttributeList&);
    Entry (const AttributeList&);
    ~Entry ();
  };
  vector<const Entry*> entries;

  // Simulation.
  void get_density (Treelog&, vector<double>& Density,
		    const Geometry& geo, 
		    double WRoot, double value, double z_factor,
		    double max_depth = 1e100);
  void output (Log&) const
  { }

  // Create.
  Rootdens_PLF (Block&);
  ~Rootdens_PLF ();
};

static Rootdens_PLF::Check_Indexes check_indexes;

void 
Rootdens_PLF::Entry::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("index", Syntax::Unknown (), Check::none (), Syntax::Const, 
	      "Index for specifying root density.");
  syntax.add ("density", Syntax::Unknown (), Syntax::None (), Syntax::Const, "\
Relative root density as a function of root depth .");
  syntax.order ("index", "density");
}

void
Rootdens_PLF::Check_Indexes::check (const Syntax& syntax, 
				    const AttributeList& alist, 
				    const string& key) const throw (string)
{ 
  daisy_assert (alist.check (key));
  daisy_assert (syntax.lookup (key) == Syntax::AList);
  daisy_assert (!syntax.is_log (key));
  daisy_assert (syntax.size (key) == Syntax::Sequence);

  const vector<AttributeList*>& alists = alist.alist_sequence (key);

  if (alists.size () < 1)
    throw string ("You must specify at least one entry");
  else
    {
      double last_index = alists[0]->number ("index");
      
      for (unsigned int i = 1; i < alists.size (); i++)
	{
	  if (!alists[i]->check ("index"))
	    continue;
	  const double new_index = alists[i]->number ("index");
	  
	  if (new_index <= last_index)
	    throw string ("Index should be monotonically increasing");
	  last_index = new_index;
	}
    }
}

Rootdens_PLF::Entry::Entry (const AttributeList& al)
  : index (al.number ("index")),
    density (al.plf ("density"))
{ }

Rootdens_PLF::Entry::~Entry ()
{ }
	
void 
Rootdens_PLF::get_density (Treelog&, vector<double>& abs_dens,
			   const Geometry& geo, 
			   const double WRoot,
			   const double index, const double z_factor, 
			   const double max_depth)
{ 
  daisy_assert (abs_dens.size () == geo.size ());

  // Find entries before and after current index.
  const Entry* before = NULL;
  const Entry* after = NULL;

  for (unsigned int i = 0; i < entries.size (); i++)
    {
      const Entry* entry = entries[i];
      
      if (entry->index <= index)
	before = entry;
      else if (entry->index >= index && after == NULL)
	after = entry;
    }

  // Find relative distibution.
  if (before == NULL)
    {				// Current index is after last entry.
      daisy_assert (after != NULL);
      for (unsigned int i = 0; i < geo.size (); i++)
	abs_dens[i] = after->density (geo.z (i) * z_factor);
    }
  else if (after == NULL)
    {				// Current index is before first entry.
      daisy_assert (before != NULL);
      for (unsigned int i = 0; i < geo.size (); i++)
	abs_dens[i] = before->density (geo.z (i) * z_factor);
    }
  else
    {				// Current index is between two entries.
      daisy_assert (after != NULL);
      daisy_assert (before != NULL);
      daisy_assert (after->index > before->index);

      const double rel_dist 
	= (index - before->index) / (after->index - before->index);
      for (unsigned int i = 0; i < geo.size (); i++)
	{
	  const double z = geo.z (i) * z_factor;
	  if (z < max_depth || i == 1)
	    {
	      const double a = before->density (z);
	      const double b = after->density (z);
	      abs_dens[i] = a + (b-a) * rel_dist;
	    }
	  else
	    abs_dens[i] = 0.0;
	}
    }

  // Find absolute distribution.
  daisy_assert (WRoot > 0.0);
  daisy_assert (SpRtLength > 0.0);
  static const double m_per_cm = 0.01;
  const double LengthPrArea = m_per_cm * SpRtLength * WRoot; // [cm/cm^2]
  const double sum = geo.total (abs_dens);
  daisy_assert (sum > 0.0);
  const double factor = LengthPrArea / sum;
  for (unsigned int i = 0; i < abs_dens.size (); i++)
    abs_dens[i] *= factor;
  daisy_assert (approximate (LengthPrArea, geo.total (abs_dens)));
}

Rootdens_PLF::Rootdens_PLF (Block& al)
  : Rootdens (al),
    entries (map_construct_const<Entry> (al.alist_sequence ("entries")))
{ }

Rootdens_PLF::~Rootdens_PLF ()
{ sequence_delete (entries.begin (), entries.end ()); }

struct Rootdens_DS_Depth : public Rootdens_PLF
{
  // Simulation.
  void set_density (Treelog& msg, vector<double>& abs_dens,
		    const Geometry& geo, 
		    double /* Depth */, double /* PotRtDpt */,
		    double WRoot, double DS)
  { get_density (msg, abs_dens, geo, WRoot, DS, -1.0); }
  
  // Create.
  Rootdens_DS_Depth (Block& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_DS_Depth ()
  { }
};

static struct Rootdens_DS_Depth_Syntax
{
  static Rootdens&
  make (Block& al)
  { return *new Rootdens_DS_Depth (al); }

  Rootdens_DS_Depth_Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    Rootdens::load_syntax (syntax, alist);
    alist.add ("description", "\
Specify root density as a function of development stage.");
    syntax.add_submodule_sequence("entries", Syntax::Const, "\
A list of pairs, where the first element of each pair is a development\n\
stage (usually a number between 0 (emergence) and 2 (ripe), and the\n\
second element is a PLF specifying the relative root density as a\n\
function of soil depth in cm (a positive number).\n\
\n\
To find the absolute root density, Daisy will interpolate the relative\n\
root density distribution specified for the entries before and after\n\
the current development stage, and scale them to match the current\n\
total root mass.",
				  Rootdens_PLF::Entry::load_syntax);
    syntax.add_check ("entries", check_indexes);
    Librarian<Rootdens>::add_type ("DS_Depth", alist, syntax, make);
  }
} Rootdens_DS_Depth_syntax;

struct Rootdens_DS_Rel : public Rootdens_PLF
{
  // Simulation.
  void set_density (Treelog& msg, vector<double>& abs_dens,
		    const Geometry& geo, 
		    double Depth, double /* PotRtDpt */,
		    double WRoot, double DS)
  { 
    daisy_assert (Depth > 0.0);
    get_density (msg, abs_dens, geo, WRoot, DS, -1.0 / Depth); 
  }

  // Create.
  Rootdens_DS_Rel (Block& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_DS_Rel ()
  { }
};

static struct Rootdens_DS_Rel_Syntax
{
  static Rootdens&
  make (Block& al)
  { return *new Rootdens_DS_Rel (al); }

  Rootdens_DS_Rel_Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    Rootdens::load_syntax (syntax, alist);
    alist.add ("description", "\
Specify root density as a function of development stage.");
    syntax.add_submodule_sequence("entries", Syntax::Const, "\
A list of pairs, where the first element of each pair is a development\n\
stage (usually a number between 0 (emergence) and 2 (ripe), and the\n\
second element is a PLF specifying the relative root density as a\n\
function of soil depth relative to the total root depth.\n\
\n\
To find the absolute root density, Daisy will interpolate the relative\n\
root density distribution specified for the entries before and after\n\
the current development stage, and scale them to match the current\n\
total root mass.",
				  Rootdens_PLF::Entry::load_syntax);
    syntax.add_check ("entries", check_indexes);

    Librarian<Rootdens>::add_type ("DS_Rel", alist, syntax, make);
  }
} Rootdens_DS_Rel_syntax;

struct Rootdens_Depth_Depth : public Rootdens_PLF
{
  // Simulation.
  void set_density (Treelog& msg, vector<double>& abs_dens,
		    const Geometry& geo, 
		    double Depth, double PotRtDpt,
		    double WRoot, double /* DS */)
  { 
    get_density (msg, abs_dens, geo, WRoot, -PotRtDpt, -1.0, -Depth); 
  }

  // Create.
  Rootdens_Depth_Depth (Block& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_Depth_Depth ()
  { }
};

static struct Rootdens_Depth_Depth_Syntax
{
  static Rootdens&
  make (Block& al)
  { return *new Rootdens_Depth_Depth (al); }

  Rootdens_Depth_Depth_Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    Rootdens::load_syntax (syntax, alist);
    alist.add ("description", "\
Specify root density as a function of development stage.");
    syntax.add_submodule_sequence("entries", Syntax::Const, "\
A list of pairs, where the first element of each pair is the root depth,\
\n(a positive number), and the second element is a PLF specifying the\n\
relative root density as a function of soil depth in cm (a positive number).\n\
\n\
To find the absolute root density, Daisy will interpolate the relative\n\
root density distribution specified for the entries before and after\n\
the current development stage, and scale them to match the current\n\
total root mass.",
				  Rootdens_PLF::Entry::load_syntax);
    syntax.add_check ("entries", check_indexes);

    Librarian<Rootdens>::add_type ("Depth_Depth", alist, syntax, make);
  }
} Rootdens_Depth_Depth_syntax;

