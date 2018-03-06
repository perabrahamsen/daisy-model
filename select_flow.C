// select_flow.C --- Flows in and out of a volume.
// 
// Copyright 2006 KVL and Per Abrahamsen
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
#include "select_value.h"
#include "block_model.h"
#include "volume.h"
#include "border.h"
#include "column.h"
#include "geometry.h"
#include "treelog.h"
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include "mathlib.h"

#ifdef DEBUG_EDGES
#include <sstream>
#endif

class SelectFlow : public SelectValue
{
  // Content.
private:
  
  const bool density;
  std::unique_ptr<Volume> volume;

  // Column cache.
  struct colweight
  {
    std::vector<size_t> edges;
    std::vector<double> weight;
  };
  typedef std::map<const Column*, colweight> colcache_t;
  colcache_t colcache;
  colcache_t::const_iterator active;

  // Use.
protected:
  virtual bool use_edge (const Geometry&, int out, int in) const = 0;

  // Output routines.
private:
  void set_column (const Column&, Treelog&);
  void output_array (const std::vector<double>&);

  // Create and Destroy.
private:
  symbol default_dimension (const symbol spec_dim) const;
  bool initialize (const Units&, const Volume& volume,
		   const symbol timestep, Treelog& msg);
  bool check_border (const Border& border, 
                     const Volume& default_volume,
                     Treelog& msg) const;
public:
  SelectFlow (const BlockModel&);
};

symbol 
SelectFlow::default_dimension (const symbol spec_dim) const
{ 
  if (density)
    return spec_dim;
  
  return Units::multiply (spec_dim, Units::cm2 ());
}

bool 
SelectFlow::initialize (const Units& units, const Volume& default_volume,
                        const symbol timestep, Treelog& msg)
{
  bool ok = true;

  if (!Select::initialize (units, default_volume, timestep, msg))
    ok = false;

  if (!volume->limit (default_volume, msg))
    ok = false;

  return ok;
}

bool 
SelectFlow::check_border (const Border& border, const Volume& default_volume,
                          Treelog& msg) const
{ return volume->check_border (border, default_volume, msg); }

void
SelectFlow::set_column (const Column& column, Treelog& msg)
{
  // Same as old?
  if (active != colcache.end () && &column == active->first)
    // Do nothing.
    return;

  // Already created?
  const colcache_t::const_iterator look = colcache.find (&column);
  if (look != colcache.end ())
    {
      // Make it active.
      active = look;
      return;
    }

  // Create a new entry.
  colweight entry;
  std::vector<size_t>& edges = entry.edges; 
  std::vector<double>& weight = entry.weight;    

  Treelog::Open nest (msg, name);

  const Geometry& geo = column.get_geometry ();
  const size_t size = geo.edge_size ();
  double total_area = 0.0;
  for (size_t e = 0; e < size; e++)
    {
      const int from = geo.edge_from (e);
      const bool from_inside = geo.cell_center_in_volume (from, *volume);
      const int to = geo.edge_to (e);
      const bool to_inside = geo.cell_center_in_volume (to, *volume);
      if (from_inside)
        {
          if (to_inside)
            // Both inside volume, don't use.
            continue;
          if (!use_edge (geo, to, from))
            continue;
        }
      else
        {
          if (!to_inside)
            // Both outside volume, don't use.
            continue;
          if (!use_edge (geo, from, to))
            continue;
        }
      edges.push_back (e);
      double area = geo.edge_area (e);
      total_area += area;
      if (from_inside)
        // Positive inwards.
        area *= -1;
      weight.push_back (area);
    }
  if (weight.size () == 0)
    msg.warning ("No edges found");
  else
    daisy_assert (std::isnormal (total_area));

  if (density)
    for (size_t i = 0; i < weight.size (); i++)
      weight[i] /= total_area;
#ifdef DEBUG_EDGES
  std::ostringstream tmp;
  tmp << "Total area: " << total_area << ", density = " << density
      << ", #edges = " << edges.size ();
  for (size_t i = 0; i < edges.size (); i++)
    tmp << "\n" << geo.edge_name (edges[i]) << ": weigth " << weight[i];
  msg.message (tmp.str ());
#endif
  daisy_assert (edges.size () == weight.size ());

  // Make it official.
  colcache[&column] = entry;
  active = colcache.find (&column);
  daisy_assert (active != colcache.end ());
}

void
SelectFlow::output_array (const std::vector<double>& array)
{ 
  if (array.size () == 0)
    return;

  if (active == colcache.end ())
    throw "Needs soil to log flow";
  
  const colweight& entry = active->second;
  const std::vector<size_t>& edges = entry.edges; 
  const std::vector<double>& weight = entry.weight;    
  
  daisy_assert (edges.size () <= array.size ());
  double sum = 0.0;
  for (size_t i = 0; i < edges.size (); i++)
    sum += array[edges[i]] * weight[i];
  
  add_result (sum);
}

SelectFlow::SelectFlow (const BlockModel& al)
  : SelectValue (al),
    density (al.flag ("density")),
    volume (Volume::build_obsolete (al)),
    active (colcache.end ())
{ }

static struct SelectFlowSyntax : public DeclareBase
{
  SelectFlowSyntax ()
    : DeclareBase (Select::component, "flow", "value", "\
Common base for logging flow through a specific plane.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_boolean ("density", Attribute::Const, 
               "If true, divide value with volume height.");
    frame.set ("density", false);
    frame.declare_object ("volume", Volume::component, 
                      Attribute::Const, Attribute::Singleton,
                      "Soil volume to log flow into.");
    frame.set ("volume", "box");
  }
} SelectFlow_syntax;

struct SelectFlowTop : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_above)
      return true;
    if (!geo.cell_is_internal (outside))
      return false;
    daisy_assert (geo.cell_is_internal (inside));
    return geo.cell_z (outside) > geo.cell_z (inside);
  }

  // Create and Destroy.
  SelectFlowTop (const BlockModel& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowTopSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectFlowTop (al); }

  SelectFlowTopSyntax ()
    : DeclareModel (Select::component, "flow_top", "flow", "\
Extract flow from top of specified volume.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("from", "cm", Attribute::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
  }
} Select_flow_top_syntax;

static struct SelectFluxTopSyntax : public DeclareParam
{
  SelectFluxTopSyntax ()
    : DeclareParam (Select::component, "flux_top", "flow_top", "\
Flux leaving top of specified volume.\n\
OBSOLETE: Use '(flow_top (negate true) (density true))' instead.")
  { }
  void load_frame (Frame& frame) const
  { frame.set ("density", true); }
} Select_flux_top_syntax;

struct SelectFlowBottom : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_below)
      return true;
    if (!geo.cell_is_internal (outside))
      return false;
    daisy_assert (geo.cell_is_internal (inside));
    return geo.cell_z (outside) < geo.cell_z (inside);
  }

  // Create and Destroy.
  SelectFlowBottom (const BlockModel& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowBottomSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectFlowBottom (al); }

  SelectFlowBottomSyntax ()
    : DeclareModel (Select::component, "flow_bottom", "flow", "\
Extract flow from bottom of specified volume.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("to", "cm", Attribute::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");

  }
} Select_flow_bottom_syntax;

static struct SelectFluxBottomSyntax : public DeclareParam
{
  SelectFluxBottomSyntax ()
    : DeclareParam (Select::component, "flux_bottom", "flow_bottom", "\
Flux entering bottom of specified volume.\n\
OBSOLETE: Use '(flow_bottom (density true))' instead.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set ("density", true);
  }
} Select_flux_bottom_syntax;

struct SelectFlowLeft : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_left)
      return true;
    if (!geo.cell_is_internal (outside))
      return false;
    daisy_assert (geo.cell_is_internal (inside));
    return geo.cell_x (outside) < geo.cell_x (inside);
  }

  // Create and Destroy.
  SelectFlowLeft (const BlockModel& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowLeftSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectFlowLeft (al); }

  SelectFlowLeftSyntax ()
    : DeclareModel (Select::component, "flow_left", "flow", "\
Extract flow from left of specified volume.")
  { }
  void load_frame (Frame&) const
  { }
} Select_flow_left_syntax;

struct SelectFlowRight : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_right)
      return true;
    if (!geo.cell_is_internal (outside))
      return false;
    daisy_assert (geo.cell_is_internal (inside));
    return geo.cell_x (outside) > geo.cell_x (inside);
  }

  // Create and Destroy.
  SelectFlowRight (const BlockModel& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowRightSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectFlowRight (al); }

  SelectFlowRightSyntax ()
    : DeclareModel (Select::component, "flow_right", "flow", "\
Extract flow from right of specified volume.")
  { }
  void load_frame (Frame& frame) const
  { }
} Select_flow_right_syntax;

struct SelectFlowFront : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_front)
      return true;
    if (!geo.cell_is_internal (outside))
      return false;
    daisy_assert (geo.cell_is_internal (inside));
    return geo.cell_y (outside) < geo.cell_y (inside);
  }

  // Create and Destroy.
  SelectFlowFront (const BlockModel& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowFrontSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectFlowFront (al); }

  SelectFlowFrontSyntax ()
    : DeclareModel (Select::component, "flow_front", "flow", "\
Extract flow from front of specified volume.")
  { }
  void load_frame (Frame&) const
  { }
} Select_flow_front_syntax;

struct SelectFlowBack : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_back)
      return true;
    if (!geo.cell_is_internal (outside))
      return false;
    daisy_assert (geo.cell_is_internal (inside));
    return geo.cell_y (outside) < geo.cell_y (inside);
  }

  // Create and Destroy.
  SelectFlowBack (const BlockModel& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowBackSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectFlowBack (al); }

  SelectFlowBackSyntax ()
    : DeclareModel (Select::component, "flow_back", "flow", "\
Extract flow from back of specified volume.")
  { }
  void load_frame (Frame&) const
  { }
} Select_flow_back_syntax;

// select_flow.C ends here.
