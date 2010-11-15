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

#define BUILD_DLL

#include "rootdens.h"
#include "geometry.h"
#include "plf.h"
#include "submodeler.h"
#include "check.h"
#include "vcheck.h"
#include "mathlib.h"
#include "memutils.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "treelog.h"
#include "block_model.h"

struct Rootdens_PLF : public Rootdens
{
  // Parameters.
  // Check.
  struct Check_Indexes : public VCheck
  {
    // Check that the indexes are monotonically increasing.
    bool verify (const Metalib&, const Frame& frame, const symbol key, 
                 Treelog& msg) const;
  };

  struct Entry
  {
    // Parameters.
    const double index;
    const PLF& density;
    
    // Create and Destroy.
    static void load_syntax (Frame&);
    Entry (const FrameSubmodel&);
    ~Entry ();
  };
  std::vector<const Entry*> entries;

  // Simulation.
  void get_density (Treelog&, std::vector<double>& Density,
		    const Geometry& geo, 
		    double WRoot, double value, double z_factor,
		    double max_depth = 1e100);
  void output (Log&) const
  { }

  // Create.
  void initialize (const Geometry&, double /* row_width */, double, Treelog&)
  { }
  Rootdens_PLF (const BlockModel&);
  ~Rootdens_PLF ();
};

static Rootdens_PLF::Check_Indexes check_indexes;

void 
Rootdens_PLF::Entry::load_syntax (Frame& frame)
{
  frame.declare ("index", Attribute::Unknown (), Check::none (), Attribute::Const, 
	      "Index for specifying root density.");
  frame.declare ("density", Attribute::Unknown (), Attribute::None (), Attribute::Const, "\
Relative root density as a function of root depth .");
  frame.order ("index", "density");
}

bool
Rootdens_PLF::Check_Indexes::verify (const Metalib&, const Frame& frame, 
                                     const symbol key, Treelog& msg) const
{ 
  daisy_assert (frame.check (key));
  daisy_assert (frame.lookup (key) == Attribute::Submodel);
  daisy_assert (!frame.is_log (key));
  daisy_assert (frame.type_size (key) == Attribute::Variable);

  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& alists 
    = frame.submodel_sequence (key);

  if (alists.size () < 1)
    {
      msg.error ("You must specify at least one entry");
      return false;
    }
  double last_index = alists[0]->number ("index");

  for (unsigned int i = 1; i < alists.size (); i++)
    {
      if (!alists[i]->check ("index"))
        continue;
      const double new_index = alists[i]->number ("index");

      if (new_index <= last_index)
        {
          msg.error ("Index should be monotonically increasing");
          return false;
        }
      last_index = new_index;
    }
  return true;
}

Rootdens_PLF::Entry::Entry (const FrameSubmodel& al)
  : index (al.number ("index")),
    density (al.plf ("density"))
{ }

Rootdens_PLF::Entry::~Entry ()
{ }
	
void 
Rootdens_PLF::get_density (Treelog&, std::vector<double>& abs_dens,
			   const Geometry& geo, 
			   const double WRoot,
			   const double index, const double z_factor, 
			   const double max_depth)
{ 
  daisy_assert (abs_dens.size () == geo.cell_size ());

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
      for (size_t i = 0; i < geo.cell_size (); i++)
	abs_dens[i] = after->density (geo.cell_z (i) * z_factor);
    }
  else if (after == NULL)
    {				// Current index is before first entry.
      daisy_assert (before != NULL);
      for (size_t i = 0; i < geo.cell_size (); i++)
	abs_dens[i] = before->density (geo.cell_z (i) * z_factor);
    }
  else
    {				// Current index is between two entries.
      daisy_assert (after != NULL);
      daisy_assert (before != NULL);
      daisy_assert (after->index > before->index);
      
      const double top = geo.top ();
      const double rel_dist 
	= (index - before->index) / (after->index - before->index);
      for (size_t i = 0; i < geo.cell_size (); i++)
	{
	  const double z = geo.cell_z (i) * z_factor;
	  if (z < max_depth || approximate (geo.cell_top (i) + 1.0, top + 1.0))
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
  daisy_assert (WRoot > 0.0);          // [g DM/m^2 S]
  daisy_assert (SpRtLength > 0.0);     // [m R/g DM]
  static const double m_per_cm = 0.01; // [m/cm]
  // [cm R/cm^2 S] = [m/cm] * [m R/g DM] * [g DM/m^2 S]
  const double LengthPrArea = m_per_cm * SpRtLength * WRoot; // [cm R/cm^2 S]
  const double sum = geo.total_soil (abs_dens);              // [cm^3 S]
  daisy_assert (sum > 0.0);
  const double surface_area = geo.surface_area (); // [cm^2 S]
  const double TotalRootLength = LengthPrArea * surface_area; // [cm R]
  const double factor 
    = TotalRootLength / sum; // [cm R/cm^3 S]
  for (unsigned int i = 0; i < abs_dens.size (); i++)
    abs_dens[i] *= factor;
  daisy_assert (approximate (TotalRootLength, geo.total_soil (abs_dens)));
}

Rootdens_PLF::Rootdens_PLF (const BlockModel& al)
  : Rootdens (al),
    entries (map_construct_const<Entry> (al.submodel_sequence ("entries")))
{ }

Rootdens_PLF::~Rootdens_PLF ()
{ sequence_delete (entries.begin (), entries.end ()); }

struct Rootdens_DS_Depth : public Rootdens_PLF
{
  // Simulation.
  void set_density (const Geometry& geo, 
		    double /* SoilDepth */, double /* CropDepth */,
		    const double /* CropWidth [cm] */,
		    double WRoot, double DS,
		    std::vector<double>& abs_dens, Treelog& msg)
  { get_density (msg, abs_dens, geo, WRoot, DS, -1.0); }
  
  // Create.
  Rootdens_DS_Depth (const BlockModel& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_DS_Depth ()
  { }
};

static struct Rootdens_DS_Depth_Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new Rootdens_DS_Depth (al); }

  Rootdens_DS_Depth_Syntax ()
    : DeclareModel (Rootdens::component, "DS_Depth", "\
Specify root density as a function of development stage.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule_sequence("entries", Attribute::Const, "\
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
    frame.set_check ("entries", check_indexes);
  }
} Rootdens_DS_Depth_syntax;

struct Rootdens_DS_Rel : public Rootdens_PLF
{
  // Simulation.
  void set_density (const Geometry& geo, 
		    double SoilDepth, double CropDepth,
		    const double /* CropWidth [cm] */,
		    double WRoot, double DS,
		    std::vector<double>& abs_dens, Treelog& msg)
  { 
    const double Depth = std::min (SoilDepth, CropDepth);
    daisy_assert (Depth > 0.0);
    get_density (msg, abs_dens, geo, WRoot, DS, -1.0 / Depth); 
  }

  // Create.
  Rootdens_DS_Rel (const BlockModel& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_DS_Rel ()
  { }
};

static struct Rootdens_DS_Rel_Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new Rootdens_DS_Rel (al); }

  Rootdens_DS_Rel_Syntax ()
    : DeclareModel (Rootdens::component, "DS_Rel", "\
Specify root density as a function of development stage.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule_sequence("entries", Attribute::Const, "\
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
    frame.set_check ("entries", check_indexes);

  }
} Rootdens_DS_Rel_syntax;

struct Rootdens_Depth_Depth : public Rootdens_PLF
{
  // Simulation.
  void set_density (const Geometry& geo, 
		    double SoilDepth, double CropDepth,
		    const double /* CropWidth [cm] */,
		    double WRoot, double /* DS */,
		    std::vector<double>& abs_dens, Treelog& msg)
  { 
    const double Depth = std::min (SoilDepth, CropDepth);
    get_density (msg, abs_dens, geo, WRoot, CropDepth, -1.0, Depth); 
  }

  // Create.
  Rootdens_Depth_Depth (const BlockModel& al)
    : Rootdens_PLF (al)
  { }
  ~Rootdens_Depth_Depth ()
  { }
};

static struct Rootdens_Depth_Depth_Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new Rootdens_Depth_Depth (al); }

  Rootdens_Depth_Depth_Syntax ()
    : DeclareModel (Rootdens::component, "Depth_Depth", "\
Specify root density as a function of development stage.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule_sequence("entries", Attribute::Const, "\
A list of pairs, where the first element of each pair is the root depth,\
\n(a positive number), and the second element is a PLF specifying the\n\
relative root density as a function of soil depth in cm (a positive number).\n\
\n\
To find the absolute root density, Daisy will interpolate the relative\n\
root density distribution specified for the entries before and after\n\
the current development stage, and scale them to match the current\n\
total root mass.",
				  Rootdens_PLF::Entry::load_syntax);
    frame.set_check ("entries", check_indexes);

  }
} Rootdens_Depth_Depth_syntax;

// rootdens_PLF.C ends here.
