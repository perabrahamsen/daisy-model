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
#include "check.h"
#include "mathlib.h"

class Rootdens_PLF : public Rootdens
{
  // Parameters.
private:
  struct Entry
  {
    // Parameters.
    const double index;
    const PLF& density;
    
    // Create and Destroy.
    static void load_syntax (Syntax&, AttributeList&);
    static bool check_alists (const vector<AttributeList*>&, Treelog& err);
    Entry (const AttributeList&);
    ~Entry ();
  };
  vector<const Entry*> entries;

  // Simulation.
protected:
  void get_density (vector<double>& Density,
		    const Geometry& geometry, 
		    double WRoot, double value, double z_factor,
		    double max_depth = 1e100);

  // Create.
protected:
  Rootdens_PLF (const AttributeList&);
  ~Rootdens_PLF ();
};

void 
Rootdens_PLF::Entry::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add_check (check_alists);
  syntax.add ("index", Syntax::Unknown (), Check::none (), Syntax::Const, 
	      "Index for specifying root density.");
  syntax.add ("density", Syntax::Unknown (), Syntax::None (), Syntax::Const, "\
Relative root density as a function of root depth .");
  syntax.order ("index", "density");
}

bool 
Rootdens_PLF::Entry::check_alists (const vector<AttributeList*>& alists,
				   Treelog& err)
{ 
  bool ok = true;

  if (alists.size () < 1)
    {
      err.entry ("You must specify at least one entry");
      ok = false;
    }
  else
    {
      double last_index = alists[0]->number ("index");
      
      for (unsigned int i = 1; i < alists.size (); i++)
	{
	  const double new_index = alists[i]->number ("index");
	  
	  if (new_index <= last_index)
	    {
	      err.entry ("Index should be monotonically increasing");
	      ok = false;
	    }
	  last_index = new_index;
	}
    }
  return ok;
}

Rootdens_PLF::Entry::Entry (const AttributeList& al)
  : index (al.number ("index")),
    density (al.plf ("density"))
{ }

Rootdens_PLF::Entry::~Entry ()
{ }
	
void 
Rootdens_PLF::get_density (vector<double>& abs_dens,
			   const Geometry& geometry, 
			   const double WRoot,
			   const double index, const double z_factor, 
			   const double max_depth)
{ 
  assert (abs_dens.size () == geometry.size ());

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
      assert (after != NULL);
      for (unsigned int i = 0; i < geometry.size (); i++)
	abs_dens[i] = after->density (geometry.z (i) * z_factor);
    }
  else if (after == NULL)
    {				// Current index is before first entry.
      assert (before != NULL);
      for (unsigned int i = 0; i < geometry.size (); i++)
	abs_dens[i] = before->density (geometry.z (i) * z_factor);
    }
  else
    {				// Current index is between two entries.
      assert (after != NULL);
      assert (before != NULL);
      assert (after->index > before->index);

      const double rel_dist 
	= (index - before->index) / (after->index - before->index);
      for (unsigned int i = 0; i < geometry.size (); i++)
	{
	  const double z = geometry.z (i) * z_factor;
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
  assert (WRoot > 0.0);
  assert (SpRtLength > 0.0);
  static const double m_per_cm = 0.01;
  const double LengthPrArea = m_per_cm * SpRtLength * WRoot; // [cm/cm^2]
  const double sum = geometry.total (abs_dens);
  assert (sum > 0.0);
  const double factor = LengthPrArea / sum;
  for (unsigned int i = 0; i < abs_dens.size (); i++)
    abs_dens[i] *= factor;
  assert (approximate (LengthPrArea, geometry.total (abs_dens)));
}

Rootdens_PLF::Rootdens_PLF (const AttributeList& al)
  : Rootdens (al),
    entries (map_construct_const<Entry> (al.alist_sequence ("entries")))
{ }

Rootdens_PLF::~Rootdens_PLF ()
{ sequence_delete (entries.begin (), entries.end ()); }

struct Rootdens_DS_Depth : public Rootdens_PLF
{
  // Simulation.
  void set_density (vector<double>& abs_dens,
		    const Geometry& geometry, 
		    double /* Depth */, double /* PotRtDpt */,
		    double WRoot, double DS)
  { get_density (abs_dens, geometry, WRoot, DS, -1.0); }
  
  // Create.
  Rootdens_DS_Depth (const AttributeList& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_DS_Depth ()
  { }
};

static struct Rootdens_DS_Depth_Syntax
{
  static Rootdens&
  make (const AttributeList& al)
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

    Librarian<Rootdens>::add_type ("DS_Depth", alist, syntax, make);
  }
} Rootdens_DS_Depth_syntax;

struct Rootdens_DS_Rel : public Rootdens_PLF
{
  // Simulation.
  void set_density (vector<double>& abs_dens,
		    const Geometry& geometry, 
		    double Depth, double /* PotRtDpt */,
		    double WRoot, double DS)
  { 
    assert (Depth > 0.0);
    get_density (abs_dens, geometry, WRoot, DS, -1.0 / Depth); 
  }

  // Create.
  Rootdens_DS_Rel (const AttributeList& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_DS_Rel ()
  { }
};

static struct Rootdens_DS_Rel_Syntax
{
  static Rootdens&
  make (const AttributeList& al)
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

    Librarian<Rootdens>::add_type ("DS_Rel", alist, syntax, make);
  }
} Rootdens_DS_Rel_syntax;

struct Rootdens_Depth_Depth : public Rootdens_PLF
{
  // Simulation.
  void set_density (vector<double>& abs_dens,
		    const Geometry& geometry, 
		    double Depth, double PotRtDpt,
		    double WRoot, double /* DS */)
  { 
    get_density (abs_dens, geometry, WRoot, -PotRtDpt, -1.0, -Depth); 
  }

  // Create.
  Rootdens_Depth_Depth (const AttributeList& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_Depth_Depth ()
  { }
};

static struct Rootdens_Depth_Depth_Syntax
{
  static Rootdens&
  make (const AttributeList& al)
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

    Librarian<Rootdens>::add_type ("Depth_Depth", alist, syntax, make);
  }
} Rootdens_Depth_Depth_syntax;

