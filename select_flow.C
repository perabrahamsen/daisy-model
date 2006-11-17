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

#include "select_value.h"
#include "volume.h"
#include "border.h"
#include "geometry.h"
#ifdef DEBUG_EDGES
#include "treelog.h"
#include <sstream>
#endif

class SelectFlow : public SelectValue
{
  // Content.
private:
  
  const bool density;
  std::auto_ptr<Volume> volume;
  const Geometry* last_geo;
  std::vector<size_t> edges;
  std::vector<double> weight;

  // Use.
protected:
  virtual bool use_edge (const Geometry&, int out, int in) const = 0;

  // Output routines.
private:
  void output_array (const std::vector<double>&, 
                     const Geometry*, const Soil*, Treelog&);

  // Create and Destroy.
private:
  symbol default_dimension (const symbol spec_dim) const;
  bool initialize (const Volume& volume,
		   const std::string& timestep, Treelog& msg);
  bool check_border (const Border& border, 
                     const Volume& default_volume,
                     Treelog& msg) const;
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  SelectFlow (Block&);
};

void 
SelectFlow::load_syntax (Syntax& syntax, AttributeList& alist)
{
  SelectValue::load_syntax (syntax, alist);
  syntax.add ("density", Syntax::Boolean, Syntax::Const, 
              "If true, divide value with volume height.");
  alist.add ("density", false);
  syntax.add ("volume", Librarian<Volume>::library (), 
              Syntax::Const, Syntax::Singleton,
              "Soil volume to log flow into.");
  alist.add ("volume", Volume::infinite_box ());
}

symbol 
SelectFlow::default_dimension (const symbol spec_dim) const
{ 
  if (density)
    return spec_dim;
  
  return Units::multiply (spec_dim, Units::cm2);
}

bool 
SelectFlow::initialize (const Volume& default_volume,
                        const std::string& timestep, Treelog& msg)
{
  bool ok = true;

  if (!Select::initialize (default_volume, timestep, msg))
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
SelectFlow::output_array (const std::vector<double>& array, 
                          const Geometry* geo, const Soil*, Treelog& msg)
{ 
  if (geo != last_geo)
    {
#ifdef DEBUG_EDGES
      Treelog::Open nest (msg, name);
#endif
      last_geo = geo;
      const size_t size = geo->edge_size ();
      edges.clear ();
      weight.clear ();
      double total_area = 0.0;
      for (size_t e = 0; e < size; e++)
        {
          const int from = geo->edge_from (e);
          const bool from_inside = geo->node_center_in_volume (from, *volume);
          const int to = geo->edge_to (e);
          const bool to_inside = geo->node_center_in_volume (to, *volume);
          if (from_inside)
            {
              if (to_inside)
                // Both inside volume, don't use.
                continue;
              if (!use_edge (*geo, to, from))
                continue;
            }
          else
            {
              if (!to_inside)
                // Both outside volume, don't use.
                continue;
              if (!use_edge (*geo, from, to))
                continue;
            }
          edges.push_back (e);
          double area = geo->edge_area (e);
          total_area += area;
          if (from_inside)
            // Positive inwards.
            area *= -1;
          weight.push_back (area);
        }
      if (weight.size () ==0)
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
        tmp << "\n" << geo->edge_name (edges[i]) << ": weigth " << weight[i];
      msg.message (tmp.str ());
#endif
      daisy_assert (edges.size () == weight.size ());
    }
  daisy_assert (edges.size () <= array.size ());

  double sum = 0.0;
  for (size_t i = 0; i < edges.size (); i++)
    sum += array[edges[i]] * weight[i];
  
  add_result (sum);
}

SelectFlow::SelectFlow (Block& al)
  : SelectValue (al),
    density (al.flag ("density")),
    volume (Volume::build_obsolete (al)),
    last_geo (NULL)
{ }


struct SelectFlowTop : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_above)
      return true;
    if (!geo.is_regular_cell (outside))
      return false;
    daisy_assert (geo.is_regular_cell (inside));
    return geo.z (outside) > geo.z (inside);
  }

  // Create and Destroy.
  SelectFlowTop (Block& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowTopSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowTop (al); }

  SelectFlowTopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Extract flow from top of specified volume.");
    SelectFlow::load_syntax (syntax, alist);
    syntax.add ("from", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");

    Librarian<Select>::add_type ("flow_top", alist, syntax, &make);
  }
} Select_flow_top_syntax;

static struct SelectFluxTopSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowTop (al); }

  SelectFluxTopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Flux leaving top of specified volume.\n\
OBSOLETE: Use '(flow_top (negate true) (density true))' instead.");
    SelectFlow::load_syntax (syntax, alist);
    syntax.add ("from", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    alist.add ("density", true);
    Librarian<Select>::add_type ("flux_top", alist, syntax, &make);
  }
} Select_flux_top_syntax;

struct SelectFlowBottom : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_below)
      return true;
    if (!geo.is_regular_cell (outside))
      return false;
    daisy_assert (geo.is_regular_cell (inside));
    return geo.z (outside) < geo.z (inside);
  }

  // Create and Destroy.
  SelectFlowBottom (Block& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowBottomSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowBottom (al); }

  SelectFlowBottomSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Extract flow from bottom of specified volume.");
    SelectFlow::load_syntax (syntax, alist);
    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");

    Librarian<Select>::add_type ("flow_bottom", alist, syntax, &make);
  }
} Select_flow_bottom_syntax;

static struct SelectFluxBottomSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowBottom (al); }

  SelectFluxBottomSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Flux entering bottom of specified volume.\n\
OBSOLETE: Use '(flow_bottom (density true))' instead.");
    SelectFlow::load_syntax (syntax, alist);
    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure to.\n\
By default, measure from the bottom.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
    alist.add ("density", true);
    Librarian<Select>::add_type ("flux_bottom", alist, syntax, &make);
  }
} Select_flux_bottom_syntax;

struct SelectFlowLeft : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_left)
      return true;
    if (!geo.is_regular_cell (outside))
      return false;
    daisy_assert (geo.is_regular_cell (inside));
    return geo.x (outside) < geo.x (inside);
  }

  // Create and Destroy.
  SelectFlowLeft (Block& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowLeftSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowLeft (al); }

  SelectFlowLeftSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Extract flow from left of specified volume.");
    SelectFlow::load_syntax (syntax, alist);

    Librarian<Select>::add_type ("flow_left", alist, syntax, &make);
  }
} Select_flow_left_syntax;

struct SelectFlowRight : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_right)
      return true;
    if (!geo.is_regular_cell (outside))
      return false;
    daisy_assert (geo.is_regular_cell (inside));
    return geo.x (outside) > geo.x (inside);
  }

  // Create and Destroy.
  SelectFlowRight (Block& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowRightSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowRight (al); }

  SelectFlowRightSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Extract flow from right of specified volume.");
    SelectFlow::load_syntax (syntax, alist);

    Librarian<Select>::add_type ("flow_right", alist, syntax, &make);
  }
} Select_flow_right_syntax;

struct SelectFlowFront : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_front)
      return true;
    if (!geo.is_regular_cell (outside))
      return false;
    daisy_assert (geo.is_regular_cell (inside));
    return geo.y (outside) < geo.y (inside);
  }

  // Create and Destroy.
  SelectFlowFront (Block& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowFrontSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowFront (al); }

  SelectFlowFrontSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Extract flow from front of specified volume.");
    SelectFlow::load_syntax (syntax, alist);

    Librarian<Select>::add_type ("flow_front", alist, syntax, &make);
  }
} Select_flow_front_syntax;

struct SelectFlowBack : public SelectFlow
{
  bool use_edge (const Geometry& geo, int outside, int inside) const
  { 
    if (outside == Geometry::cell_back)
      return true;
    if (!geo.is_regular_cell (outside))
      return false;
    daisy_assert (geo.is_regular_cell (inside));
    return geo.y (outside) < geo.y (inside);
  }

  // Create and Destroy.
  SelectFlowBack (Block& al)
    : SelectFlow (al)
  { }
};

static struct SelectFlowBackSyntax
{
  static Select& make (Block& al)
  { return *new SelectFlowBack (al); }

  SelectFlowBackSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Extract flow from back of specified volume.");
    SelectFlow::load_syntax (syntax, alist);

    Librarian<Select>::add_type ("flow_back", alist, syntax, &make);
  }
} Select_flow_back_syntax;

// select_flow.C ends here.
