// gnuplot_soil.C -- Plot soil content at specific time.
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
#include "lexer_soil.h"
#include "treelog.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include "time.h"
#include "units.h"
#include "submodeler.h"
#include "check.h"
#include "vcheck.h"
#include <sstream>
#include <boost/scoped_ptr.hpp>

struct GnuplotSoil : public GnuplotBase
{
  // Ranges.
  const double sizeratio;
  const boost::scoped_ptr<Time> when;
  const double top;
  const double bottom;
  const double left;
  const double right;
  const double vmin;
  const double vmax;

  // Data.
  LexerSoil lex;

  // Plot.
  enum type_t { block, smooth, contour, surface };
  static type_t symbol2type (symbol);
  static symbol type2symbol (type_t);
  type_t type;
  const int samples;
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

GnuplotSoil::type_t 
GnuplotSoil::symbol2type (const symbol s)
{
  static struct sym_set_t : std::map<symbol, type_t>
  {
    sym_set_t ()
    {
      insert (std::pair<symbol,type_t> ("block", block));
      insert (std::pair<symbol,type_t> ("smooth", smooth));
      insert (std::pair<symbol,type_t> ("contour", contour));
      insert (std::pair<symbol,type_t> ("surface", surface));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

symbol 
GnuplotSoil::type2symbol (const type_t t)
{
  static struct sym_set_t : std::map<type_t, symbol>
  {
    sym_set_t ()
    {
      insert (std::pair<type_t,symbol> (block, "block"));
      insert (std::pair<type_t,symbol> (smooth, "smooth"));
      insert (std::pair<type_t,symbol> (contour, "contour"));
      insert (std::pair<type_t,symbol> (surface, "surface"));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (t);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}

bool
GnuplotSoil::initialize (const Units& units, Treelog& msg)
{ 
  // Read header.
  if (!lex.read_header (msg))
    return false;
  if (!lex.read_soil (msg))
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
  
  std::vector<std::string> closest_entries;
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

      double distance = std::fabs (Time::fraction_hours_between (time, *when));

      if (closest < 0.0 || distance < closest)
        {
          closest = distance;
          closest_entries = entries;
        }
    }
  if (closest < 0.0)
    {
      msg.error ("No data found");
      return false;
    }
  if (!lex.soil_cells (closest_entries, value, msg))
    {
      msg.error ("Problem reading cell data");
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

  switch (type)
    {
    case block:
      out << "\
set pm3d map\n\
set pm3d corners2color c4\n";
      break;
    case smooth:
      out << "\
set pm3d map\n\
set samples " << samples << "\n\
set isosamples " << samples << "\n";
      break;
    case contour:
      out << "\
set contour\n\
set view map\n\
unset surface\n\
set cntrparam levels " << samples << "\n";
      break;
    case surface:
      break;
    }

  // Same size axes.
  out << "\
set size ratio " << sizeratio << "\n";

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
      << "set zrange [" << vminf << ":" << vmaxf << "]\n";

  if (type == block || type == smooth)
    out << "set cbrange [" << vminf << ":" << vmaxf << "]\n";
  
  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "splot '-' using 2:1:3 ";

  if (type == contour || type == surface)
    out << "with lines ";

  // Removed 2012-07-10: Used?
  if (false)
    out << "title \"" << dimension << "\"\n";
  else
    out << "title \"" << "" << "\"\n";
  
  // Data.
  daisy_assert (value.size () == xplus.size () * zplus.size ());
  size_t c = 0;

  switch (type)
    { 
    case block:
      // Cell corners only.
      daisy_assert (value.size () > 0);
      out << "0 0 " << value[0] << "\n";
      daisy_assert (value.size () >= zplus.size ());
      for (size_t iz = 0; iz < zplus.size (); iz++)
        out << zplus[iz] << " 0 " << value[iz] << "\n";
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
      break;
    case smooth:
      {
        // Outer corners and cell centers.
        double last_x = 0.0;

        // First line.
        double last_z = 0.0; 
        daisy_assert (c < value.size ()); 
        out << last_z << " " << last_x << " " << value[c] << "\n";
        for (size_t iz = 0; iz < zplus.size (); iz++)
          {
            const double z = (last_z + zplus[iz]) / 2.0;
            last_z = zplus[iz];
            daisy_assert (c < value.size ());
            out << z << " " << last_x << " " << value[c] << "\n";
            c++;
          }
        out << last_z << " " << last_x << " " << value[c - 1] << "\n";
        c -= zplus.size ();

        // Data.
        for (size_t ix = 0; ix < xplus.size (); ix++)
          {
            const double x = (last_x + xplus[ix]) / 2.0;
            last_x = xplus[ix];
            last_z = 0.0;
            out << "\n" << last_z << " " << x << " " << value[c] << "\n";
            for (size_t iz = 0; iz < zplus.size (); iz++)
              {
                const double z = (last_z + zplus[iz]) / 2.0;
                last_z = zplus[iz];
                daisy_assert (c < value.size ());
                out << z << " " << x << " " << value[c] << "\n";
                c++;
              }
            out << last_z << " " << x << " " << value[c-1] << "\n";
          }

        // Last line.
        c -= zplus.size ();
        last_z = 0.0; 
        daisy_assert (c < value.size ()); 
        out << "\n" << last_z << " " << last_x << " " << value[c] << "\n";
        for (size_t iz = 0; iz < zplus.size (); iz++)
          {
            const double z = (last_z + zplus[iz]) / 2.0;
            last_z = zplus[iz];
            daisy_assert (c < value.size ());
            out << z << " " << last_x << " " << value[c] << "\n";
            c++;
          }
        out << last_z << " " << last_x << " " << value[c - 1] << "\n";
      }
      break;
    case surface:
    case contour:
      // Cell centers only.
      {
        double last_x = 0.0;
        for (size_t ix = 0; ix < xplus.size (); ix++)
          {
            const double x = (last_x + xplus[ix]) / 2.0;
            last_x = xplus[ix];
            double last_z = 0.0;
            for (size_t iz = 0; iz < zplus.size (); iz++)
              {
                const double z = (last_z + zplus[iz]) / 2.0;
                last_z = zplus[iz];
                daisy_assert (c < value.size ());
                out << z << " " << x << " " << value[c] << "\n";
                c++;
              }
            out << "\n";
          }
      }
      break;
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
    sizeratio (al.number ("sizeratio")),
    when (submodel<Time> (al, "when")),
    top (al.number ("top")),
    bottom (al.number ("bottom", NAN)),
    left (al.number ("left")),
    right (al.number ("right", NAN)),
    vmin (al.number ("min", NAN)),
    vmax (al.number ("max", NAN)),
    lex (al),
    type (symbol2type (al.name ("type"))),
    samples (al.integer ("samples")),
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
    frame.declare ("sizeratio", Attribute::None (), Check::none (),
		   Attribute::Const, "\
If positive, ratio of y-axes length to x-axis length.\n\
If negative, use same ratio for units on the two axes.");
    frame.set ("sizeratio", -1.0);
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

    frame.declare_string ("type", Attribute::State, "Plot type.\n\
Valid options are 'block' and 'contour'.");
    frame.set ("type", "block");
    static VCheck::Enum type_check ("block", "smooth", "contour", "surface");
    frame.set_check ("type", type_check);
    frame.declare_integer ("samples", Attribute::Const, "\
Number of sample lines for the 'smooth' and 'contour' types.");
    frame.set ("samples", 25);
    LexerSoil::load_syntax (frame);
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
  }
} GnuplotSoil_syntax;

// gnuplot_soil.C ends here.
