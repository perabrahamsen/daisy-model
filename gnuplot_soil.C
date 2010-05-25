// gnuplot_time.C -- Plot soil content at specific time.
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
#include "lexer_table.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include "time.h"
#include "units.h"
#include "submodeler.h"
#include "check.h"
#include <sstream>
#include <boost/scoped_ptr.hpp>

#define USE_PM3D

struct GnuplotSoil : public GnuplotBase
{
  // Ranges.
  const boost::scoped_ptr<Time> when;
  const double top;
  const double bottom;
  const double left;
  const double right;
  const double vmin;
  const double vmax;

  // Data.
  LexerTable lex;

  // Plot.
  symbol dimension;
  symbol tag;
  std::vector<double> zplus;
  std::vector<double> xplus;
  std::vector<double> value;

  // Use.
  bool initialize (const Units& units, Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotSoil (const BlockModel& al);
  ~GnuplotSoil ();
};

bool
GnuplotSoil::initialize (const Units& units, Treelog& msg)
{ 
  // Read header.
  if (!lex.read_header (msg))
    return false;

  if (!lex.good ())
    return false;

  // Array.
  tag = lex.soil_tag ();
  zplus = lex.soil_zplus ();
  xplus = lex.soil_xplus ();
  if (xplus.size () < 1)
    {
      msg.error ("One dimensional data");
      return false;
    }
  symbol original (lex.soil_dimension ());

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
        if (lex.soil_cells (entries, value, msg))
          closest = distance;
    }
  if (closest < 0.0)
    {
      msg.error ("No data found");
      return false;
    }

  // Convert
  if (dimension != original)
    for (size_t i = 0; i < value.size (); i++)
      {
        if (!units.can_convert (original, dimension, value[i]))
          {
            std::ostringstream tmp;
            tmp << "Can't convert " << value[i] << " from [" << original 
                << "] to [" << dimension << "]";
            msg.error (tmp.str ());
            return false;
          }
        value[i] = units.convert (original, dimension, value[i]);
      }

  // Done.
  return true;
}

bool
GnuplotSoil::plot (std::ostream& out, Treelog& msg)
{ 
  if (xplus.size () < 1 || zplus.size () < 1)
    {
      msg.warning ("Nothing to plot");
      return false;
    }

  // Header.
  plot_header (out);

#ifdef USE_CONTOUR
  out << "\
set contour\n\
set view map\n\
unset surface\n\
set cntrparam levels 5\n";
#endif

#ifdef USE_PM3D
  out << "\
set pm3d map\n\
set pm3d corners2color c4\n";
#endif

  // Same size axes.
  out << "\
set size ratio -1\n";

  // Legend.
  if (legend != "auto")
    out << "set key " << legend_table[legend] << "\n";

  // Range
  const double vmaxf = std::isfinite (vmax)
    ? vmax
    : *std::max_element (value.begin (), value.end ());
  const double vminf = std::isfinite (vmin)
    ? vmin
    : *std::min_element (value.begin (), value.end ());
  const double bottomf = std::isfinite (bottom)
    ? bottom
    : zplus[zplus.size () - 1];
  const double rightf = std::isfinite (right)
    ? right
    : xplus[xplus.size () - 1];
  out << "set xrange [" << left << ":" << rightf << "]\n"
      << "set yrange [" << bottomf << ":" << top << "]\n"
      << "set cbrange [" << vminf << ":" << vmaxf << "]\n"
    ;
  
  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "splot '-' using 2:1:3 ";

#ifdef USE_CONTOUR
  out << "with lines ";
#endif
  out << "title \"" << dimension << "\"\n";
  
  // Data.
  daisy_assert (value.size () == xplus.size () * zplus.size ());
  
  size_t c = 0;
  out << "0 0 " << vmaxf << "\n";
  for (size_t iz = 0; iz < zplus.size (); iz++)
    out << zplus[iz] << " 0 " << vmaxf << "\n";
  for (size_t ix = 0; ix < xplus.size (); ix++)
    {
      out << "\n0 " << xplus[ix] << " " << vmaxf << "\n";
      for (size_t iz = 0; iz < zplus.size (); iz++)
        {
          daisy_assert (c < value.size ());
          out << zplus[iz] << " " << xplus[ix] << " " << value[c] << "\n";
          c++;
        }
    }
  daisy_assert (c == value.size ());

  out << "e\n";

  // The end.
  if (interactive ())
    out << "pause mouse\n";

  return true;
}

GnuplotSoil::GnuplotSoil (const BlockModel& al)
  : GnuplotBase (al),
    when (submodel<Time> (al, "when")),
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

GnuplotSoil::~GnuplotSoil ()
{ }

static struct GnuplotSoilSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GnuplotSoil (al); }
  GnuplotSoilSyntax ()
    : DeclareModel (Gnuplot::component, "soil", "common",
                    "Generate a 2D gnuplot graph with soil content.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule ("when", Attribute::Const, "\
Use value closest to this time.", Time::load_syntax);
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

    LexerTable::load_syntax (frame);
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
  }
} GnuplotSoil_syntax;

// gnuplot_soil.C ends here.
