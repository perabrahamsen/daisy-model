// gnuplot_vector.C -- Plot vector field at specific time.
// 
// Copyright 2005, 2010 and 2012 Per Abrahamsen and KVL.
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

struct GnuplotVector : public GnuplotBase
{
  // Ranges.
  const std::unique_ptr<Time> when;
  const std::unique_ptr<Time> begin;
  const std::unique_ptr<Time> end;
  const double top;
  double bottom;
  const double left;
  double right;
  const double factor;

  // Data.
  LexerFlux lex;
  std::vector<double> center_z;
  std::vector<double> center_x;
  std::vector<double> dz;
  std::vector<double> dx;

  // Plot.
  symbol dimension;
  symbol tag;
  
  // Use.
  bool load_when (std::vector<double>& all_values, Treelog& msg);
  bool load_interval (std::vector<double>& all_values, Treelog& msg);
  bool initialize (const Units& units, Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotVector (const BlockModel& al);
  ~GnuplotVector ();
};

static bool approx2 (double a, double b)
{ return approximate (a, b); }

bool
GnuplotVector::load_when (std::vector<double>& all_values, Treelog& msg)
{
  double closest = -42.42e42; // [h]
  
  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      Time time (9999, 1, 1, 0);
      // Read entries.
      if (!lex.get_entries (entries))
        continue;
      if (!lex.get_time_dh (entries, time, 8))
        continue;

      double distance = std::fabs (Time::fraction_hours_between (time, *when));

      if (closest < 0.0 || distance < closest)
        if (lex.flux_edges (entries, all_values, msg))
          closest = distance;
    }
  if (closest < 0.0)
    {
      msg.error ("No date found");
      return false;
    }
  return true;
}

bool
GnuplotVector::load_interval (std::vector<double>& all_values, Treelog& msg)
{
  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      Time time (9999, 1, 1, 0);
      // Read entries.
      if (!lex.get_entries (entries))
        continue;
      if (!lex.get_time_dh (entries, time, 8))
        continue;
      if (begin.get () && time < *begin)
        continue;
      if (end.get () && time > *end)
        continue;

      std::vector<double> values;
      
      if (!lex.flux_edges (entries, values, msg))
        continue;
      
      if (all_values.size () < 1)
        all_values = values;
      else 
        {
          const size_t size = all_values.size ();
          if (values.size () != size)
            continue;
          for (size_t i = 0; i < size; i++)
            all_values[i] += values[i];
        }
    }
  return all_values.size () > 0;
}

bool
GnuplotVector::initialize (const Units& units, Treelog& msg)
{ 
  // Read header.
  if (!lex.read_header (msg))
    return false;
  if (!lex.read_flux (msg))
    return false;
  if (!lex.good ())
    return false;

  // Array.
  // symbol tag = lex.flux_tag ();
  if (lex.edge_z ().size () > 0)
    {
      msg.error ("One dimensional data");
      return false;
    }

  const std::vector<int>& flux_from = lex.edge_from ();
  const std::vector<int>& flux_to = lex.edge_to ();
  const size_t array_size = flux_from.size ();
  daisy_assert (flux_to.size () == array_size);
  center_z = lex.cell_z ();
  center_x = lex.cell_x ();
  const size_t cell_size = center_z.size ();
  daisy_assert (cell_size == center_x.size ());
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
  std::vector<double> all_values;

  if (when.get ())
    load_when (all_values, msg);
  else
    load_interval (all_values, msg);

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

  if (!std::isfinite (bottom))
    {
      std::sort (all_z.begin (), all_z.end ());
      all_z.erase (std::unique (all_z.begin (), all_z.end (), approx2),
                   all_z.end ());
      std::reverse (all_z.begin (), all_z.end ());
      bottom = 0;      
      for (size_t i = 0; i < all_z.size (); i++)
        bottom += (all_z[i] - bottom) * 2.0;
    }

  if (!std::isfinite (right))
    {
      std::sort (all_x.begin (), all_x.end ());
      all_x.erase (std::unique (all_x.begin (), all_x.end (), approx2),
                   all_x.end ());
      right = 0;
      for (size_t i = 0; i < all_x.size (); i++)
        right += (all_x[i] - right) * 2.0;
    }
  
  // Filter and convert
  dz = std::vector<double> (cell_size, 0.0);
  dx = std::vector<double> (cell_size, 0.0);
  std::vector<size_t> count (cell_size, 0);
  for (size_t i = 0; i < array_size; i++)
    {
      double val = all_values[i];
      // Convert.
      if (!units.can_convert (original, dimension, val))
        {
          std::ostringstream tmp;
          tmp << "Can't convert " << val << " from [" << original 
              << "] to [" << dimension << "]";
          msg.error (tmp.str ());
          return false;
        }
      val = units.convert (original, dimension, val);
      int from = flux_from[i];
      daisy_assert (from < static_cast<int> (cell_size));
      int to = flux_to[i];
      daisy_assert (to < static_cast<int> (cell_size));
      switch (from)
        {
        case Geometry::cell_above:
          daisy_assert (to >= 0);
          dz[to] -= val;
          count[to]++;
          break;
        case Geometry::cell_below:
          daisy_assert (to >= 0);
          dz[to] += val;
          count[to]++;
          break;
        case Geometry::cell_left:
          dx[to] += val;
          count[to]++;
          daisy_assert (to >= 0);
          break;
        case Geometry::cell_right:
          dx[to] -= val;
          count[to]++;
          daisy_assert (to >= 0);
          break;
        default:
          daisy_assert (from >= 0);
          switch (to)
            {
            case Geometry::cell_above:
              daisy_assert (from >= 0);
              dz[from] += val;
              count[from]++;
              break;
            case Geometry::cell_below:
              daisy_assert (from >= 0);
              dz[from] -= val;
              count[from]++;
              break;
            case Geometry::cell_left:
              dx[from] -= val;
              count[from]++;
              daisy_assert (from >= 0);
              break;
            case Geometry::cell_right:
              dx[from] += val;
              count[from]++;
              daisy_assert (from >= 0);
              break;
            default:
              daisy_assert (to >= 0);
              count[from]++;
              count[to]++;
              const bool same_z = approximate (center_z[from], center_z[to]);
              const bool same_x = approximate (center_x[from], center_x[to]);
              daisy_assert (same_z != same_x);
              if (same_z)
                {
                  if (center_x[from] < center_x[to])
                    {
                      dx[from] += val;
                      dx[to] += val;
                    }
                  else
                    {
                      dx[from] -= val;
                      dx[to] -= val;
                    }
                }
              else
                {
                  if (center_z[from] < center_z[to])
                    {
                      dz[from] += val;
                      dz[to] += val;
                    }
                  else
                    {
                      dz[from] -= val;
                      dz[to] -= val;
                    }
                }
            }
        }
    }

  for (size_t i = 0; i < cell_size; i++)
    {
      daisy_assert (count[i] > 0);
      const double mul = factor / count[i];
      dz[i] *= mul;
      dx[i] *= mul;
    }

  // Done.
  return true;
}

bool
GnuplotVector::plot (std::ostream& out, Treelog& msg)
{ 

  // Header.
  plot_header (out);

  // Same size axes.
  out << "\
set size ratio -1\n";

  // Legend.
  if (legend != "auto")
    out << "set key " << legend_table[legend] << "\n";

  out << "set xrange [" << left << ":" << right << "]\n"
      << "set yrange [" << bottom << ":" << top << "]\n";

  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "plot '-' using 1:2:3:4 with vec title \"" << dimension << "\"\n";
  
  // Data.
  const size_t cell_size = center_z.size ();
  daisy_assert (center_x.size () == cell_size);
  daisy_assert (dz.size () == cell_size);
  daisy_assert (dx.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    out << center_x[c] << " " << center_z[c] 
        << " " << dx[c] << " " << dz[c] << "\n";
  out << "e\n";

  // The end.
  if (interactive ())
    out << "pause mouse\n";

  return true;
}

GnuplotVector::GnuplotVector (const BlockModel& al)
  : GnuplotBase (al),
    when (al.check ("when") ? submodel<Time> (al, "when") : NULL),
    begin (al.check ("begin") ? submodel<Time> (al, "begin") : NULL),
    end (al.check ("end") ? submodel<Time> (al, "end") : NULL),
    top (al.number ("top")),
    bottom (al.number ("bottom", NAN)),
    left (al.number ("left")),
    right (al.number ("right", NAN)),
    factor (al.number ("factor")),
    lex (al),
    dimension (al.name ("dimension", Attribute::Unknown ())),
    tag (Attribute::Unknown ())
{ }

GnuplotVector::~GnuplotVector ()
{ }

static struct GnuplotVectorSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GnuplotVector (al); }
  GnuplotVectorSyntax ()
    : DeclareModel (Gnuplot::component, "vector", "common",
                    "Generate a 2D gnuplot vector field.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule ("when", Attribute::OptionalConst, "\
Use value closest to this time.", Time::load_syntax);
    frame.declare_submodule ("begin", Attribute::OptionalConst, "\
Ignore values before this time.", Time::load_syntax);
    frame.declare_submodule ("end", Attribute::OptionalConst, "\
Ignore values after this time.", Time::load_syntax);
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
    frame.declare ("factor", Attribute::Unknown (), Attribute::Const, "\
Multiply vector values with this factor.");
    LexerFlux::load_syntax (frame);
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
  }
} GnuplotVector_syntax;

// gnuplot_vector.C ends here.
