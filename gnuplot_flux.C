// gnuplot_flux.C -- Plot flux content at specific time.
// 
// Copyright 2005 and 2010 Per Abrahamsen and KVL.
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
#include "gnuplot_base.h"
#include "block_model.h"
#include "lexer_flux.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include "time.h"
#include "units.h"
#include "submodeler.h"
#include "check.h"
#include "vcheck.h"
#include "geometry.h"
#include <sstream>
#include <boost/scoped_ptr.hpp>

struct GnuplotFlux : public GnuplotBase
{
  // Parameters.
  const boost::scoped_ptr<Time> when;
  const double plot_z;
  const double plot_x;
  const double top;
  double bottom;
  const double left;
  double right;
  double vmin;
  double vmax;
  
  // Data.
  LexerFlux lex;

  // Plot.
  symbol dimension;
  symbol tag;
  std::vector<double> position;
  std::vector<double> value;
  
  // Use.
  bool initialize (const Units& units, Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotFlux (const BlockModel& al);
  ~GnuplotFlux ();
};

bool
GnuplotFlux::initialize (const Units& units, Treelog& msg)
{ 
  // Read header.
  if (!lex.read_header (msg))
    return false;
  if (!lex.read_flux (msg))
    return false;

  if (!lex.good ())
    return false;

  // Array.
  tag = lex.flux_tag ();
  if (lex.edge_z ().size () > 0)
    {
      msg.error ("One dimensional data");
      return false;
    }

  const std::vector<int>& flux_from = lex.edge_from ();
  const std::vector<int>& flux_to = lex.edge_to ();
  const size_t array_size = flux_from.size ();
  daisy_assert (flux_to.size () == array_size);
  const std::vector<double>& center_z = lex.cell_z ();
  const std::vector<double>& center_x = lex.cell_x ();
  daisy_assert (center_z.size () == center_x.size ());
  if (array_size < 1)
    {
      msg.error ("No cells");
      return false;
    }

  symbol original (lex.flux_dimension ());

  if (dimension == Attribute::Unknown ())
    dimension = original;
  else if (!units.can_convert (original, dimension))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert from [" << original 
          << "] to [" << dimension << "]";
      lex.error (tmp.str ());
      return false;
    }

  // Read data.
  double closest = -42.42e42; // [h]
  
  std::vector<double> all_values;
  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      Time time (9999, 1, 1, 0);
      // Read entries.
      if (!lex.get_entries (entries))
        continue;
      if (!lex.get_time (entries, time, 8))
        continue;

      double distance = std::fabs (Time::hours_between (time, *when));

      if (closest < 0.0 || distance < closest)
        if (lex.flux_edges (entries, all_values, msg))
          closest = distance;
    }
  if (closest < 0.0)
    {
      msg.error ("No date found");
      return false;
    }

  daisy_assert (all_values.size () == array_size);

  // Find right and bottom limits.
  std::vector<double> all_z;
  std::vector<double> all_x;

  for (size_t i = 0; i < array_size; i++)
    {
      int from = flux_from[i];
      int to = flux_to[i];

      if (from > 0)
        {
          daisy_assert (center_z.size () > from);
          daisy_assert (center_x.size () > from);
          all_z.push_back (center_z[from]);
          all_x.push_back (center_x[from]);
        }

      if (to > 0)
        {
          daisy_assert (center_z.size () > to);
          daisy_assert (center_x.size () > to);
          all_z.push_back (center_z[to]);
          all_x.push_back (center_x[to]);
        }
    }
  if (all_z.size () < 1 || all_x.size () < 1)
    {
      msg.error ("No internal cells");
      return false;
    }

  const double soil_top = 0.0;
  std::sort (all_z.begin (), all_z.end ());
  const double soil_bottom = all_z.front ();
  if (!std::isfinite (bottom))
    bottom = soil_bottom;

  const double soil_left = 0.0;
  std::sort (all_x.begin (), all_x.end ());
  const double soil_right = all_x.back ();
  if (!std::isfinite (right))
    right = soil_right;
  
  // Filter and convert
  for (size_t i = 0; i < array_size; i++)
    {
      double val = all_values[i];
      double pos;
      int from = flux_from[i];
      int to = flux_to[i];

      if (std::isfinite (plot_z))
        {
          daisy_assert (!std::isfinite (plot_x));
          double from_z;
          double to_z;
          
          if (from == Geometry::cell_above)
            from_z = soil_top;
          else if (from == Geometry::cell_below)
            from_z = soil_bottom;
          else if (from < 0)
            continue;
          else
            from_z = center_z[from];

          if (to == Geometry::cell_above)
            to_z = soil_top;
          else if (to == Geometry::cell_below)
            to_z = soil_bottom;
          else if (to < 0)
            continue;
          else
            to_z = center_z[to];

          if ((to_z < plot_z) == (from_z < plot_z))
            continue;

          if (from < 0)
            pos = center_x[to];
          else if (to < 0)
            pos = center_x[from];
          else 
            pos = (center_x[to] + center_x[from]) / 2.0;
        }
      else 
        {
          daisy_assert (std::isfinite (plot_x));
          double from_x;
          double to_x;
          
          if (from == Geometry::cell_left)
            from_x = soil_left;
          else if (from == Geometry::cell_right)
            from_x = soil_right;
          else if (from < 0)
            continue;
          else
            from_x = center_x[from];

          if (to == Geometry::cell_left)
            to_x = soil_left;
          else if (to == Geometry::cell_right)
            to_x = soil_right;
          else if (to < 0)
            continue;
          else
            to_x = center_x[to]; 

          if ((to_x < plot_x) == (from_x < plot_x))
            continue;

          if (from < 0)
            pos = center_z[to];
          else if (to < 0)
            pos = center_z[from];
          else 
            pos = (center_z[to] + center_z[from]) / 2.0;
       }

      // Convert.
      if (dimension != original)
        {
          if (!units.can_convert (original, dimension, val))
            {
              std::ostringstream tmp;
              tmp << "Can't convert " << val << " from [" << original 
                  << "] to [" << dimension << "]";
              msg.error (tmp.str ());
              return false;
            }
        }
      val = units.convert (original, dimension, val);
      
      // Store.
      value.push_back (val);
      position.push_back (pos);
    }

  // Value range.
  if (value.size () < 1)
   {
     msg.error ("No values");
     return false;
   }
  if (!std::isfinite (vmax))
    vmax = *std::max_element (value.begin (), value.end ());
  if (!std::isfinite  (vmin))
    vmin = *std::min_element (value.begin (), value.end ());

  // Done.
  return true;
}

bool
GnuplotFlux::plot (std::ostream& out, Treelog& msg)
{ 
  // Header.
  plot_header (out);

  // Legend.
  if (legend != "auto")
    out << "set key " << legend_table[legend] << "\n";

  // Range
  if (std::isfinite (plot_x))
    out << "set yrange [" << bottom << ":" << top << "]\n"
        << "set xrange [" << vmin << ":" << vmax << "]\n";
  else
    out << "set xrange [" << left << ":" << right << "]\n"
        << "set yrange [" << vmin << ":" << vmax << "]\n";

  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "plot '-' with lines";

  if (std::isfinite (plot_x))
    out << " using 2:1";
  
  out << "\n";

  // Data.
  const size_t size = value.size ();
  daisy_assert (size == position.size ());

  for (size_t i = 0; i < size; i++)
    out << position[i] << " " << value[i] << "\n";

  out << "e\n";

  // The end.
  if (interactive ())
    out << "pause mouse\n";
  
  return true;
}

GnuplotFlux::GnuplotFlux (const BlockModel& al)
  : GnuplotBase (al),
    when (submodel<Time> (al, "when")),
    plot_z (al.number ("z", NAN)),
    plot_x (al.number ("x", NAN)),
    top (al.number ("top")),
    bottom (al.number ("bottom", NAN)),
    left (al.number ("left")),
    right (al.number ("right", NAN)),
    vmin (al.number ("min", NAN)),
    vmax (al.number ("max", NAN)),
    lex (al),
    dimension (al.name ("dimension", Attribute::Unknown ())),
    tag (Attribute::Unknown ())
{ }

GnuplotFlux::~GnuplotFlux ()
{ }

static struct GnuplotFluxSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GnuplotFlux (al); }
  GnuplotFluxSyntax ()
    : DeclareModel (Gnuplot::component, "flux", "common",
                    "Generate a 2D gnuplot graph with flux content.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;

    if (al.check ("z") == al.check ("x"))
      {
        msg.error ("You must specify exactly one of 'x' and 'z'");
        ok = false;
      }
    return ok;
  }

  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_submodule ("when", Attribute::Const, "\
Use value closest to this time.", Time::load_syntax);
    frame.declare ("z", "cm", Check::negative (),
                   Attribute::OptionalConst, "\
Plot flux through this depth.");
    frame.declare ("x", "cm", Check::negative (),
                   Attribute::OptionalConst, "\
Plot flux through this position.");
    frame.declare ("top", "cm", Check::non_positive (),
                   Attribute::Const, "\
Higest z value in plot.");
    frame.set ("top", 0.0);
    frame.declare ("bottom", "cm", Check::negative (),
                   Attribute::OptionalConst, "\
Deepest z value in plot.  By default, derive value from data file.");
    frame.declare ("left", "cm", Check::non_negative (), 
                   Attribute::Const, "\
Minimum x value in plot.");
    frame.set ("left", 0.0);
    frame.declare ("right", "cm", Check::positive (), 
                   Attribute::OptionalConst, "\
Maximum x value in plot. By default, derive value from data file.");
    frame.declare ("min", Attribute::User (), Attribute::OptionalConst, "\
Fixed lowest value.  By default determine this from the data.");
    frame.declare ("max", Attribute::User (), Attribute::OptionalConst, "\
Fixed highest value.  By default determine this from the data.");

    frame.declare_string ("type", Attribute::State, "Plot type.\n\
Valid options are 'block' and 'contour'.");
    frame.set ("type", "block");
    static VCheck::Enum type_check ("block", "smooth", "contour", "surface");
    frame.set_check ("type", type_check);

    LexerFlux::load_syntax (frame);
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
  }
} GnuplotFlux_syntax;

// gnuplot_flux.C ends here.
