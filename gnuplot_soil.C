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

struct GnuplotSoil : public GnuplotBase
{
  // Ranges.
  const boost::scoped_ptr<Time> at;
  const double width;
  const double depth;

  // Data.
  LexerTable lex;

  // Plot.
  symbol dimension;
  symbol tag;
  std::vector<double> z;
  std::vector<double> x;
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
  z = lex.soil_z ();
  x = lex.soil_x ();

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

      double distance = std::fabs (Time::hours_between (time, *at));

      if (closest < 0.0 || distance < closest)
        if (lex.soil_value (entries, value, msg))
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
  const size_t size = x.size ();
  if (size < 1)
    {
      msg.warning ("Nothing to plot");
      return false;
    }

  // Header.
  plot_header (out);
  out << "\
set contour\n\
set view map\n\
unset surface\n\
set cntrparam levels 5\n\
set size ratio -1\n";

  // Legend.
  if (legend != "auto")
    out << "set key " << legend_table[legend] << "\n";

  // Range
  out << "set xrange [0:" << width << "]\n";
  out << "set yrange [" << depth << ":0]\n";
  
  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "splot '-' using 1:2:3 with lines title \"\"\n";
  
  // Data.
  daisy_assert (z.size () == size);
  daisy_assert (value.size () == size);
  
  double last_x = x[0];
  for (size_t i = 0; i < size; i++)
    {
      if (!approximate (x[i], last_x))
        {
          out << "\n";
          last_x = x[i];
        }
      out << x[i] << "\t" << z[i] << "\t"<< value[i] << "\n";
    }

  out << "e\n";

  // The end.
  if (interactive ())
    out << "pause mouse\n";

  return true;
}

GnuplotSoil::GnuplotSoil (const BlockModel& al)
  : GnuplotBase (al),
    at (submodel<Time> (al, "at")),
    width (al.number ("width")),
    depth (al.number ("depth")),
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
    frame.declare_submodule ("at", Attribute::Const, "\
Use value closest to this time.", Time::load_syntax);
    frame.declare ("depth", "cm", Check::negative (), Attribute::Const, "\
Maximum z value in plot.");
    frame.declare ("width", "cm", Check::positive (), Attribute::Const, "\
Maximum x value in plot.");
    LexerTable::load_syntax (frame);
    frame.declare_string ("dimension", Attribute::OptionalConst, "\
Dimension for data.  By default, use dimension from file.");
  }
} GnuplotSoil_syntax;

// gnuplot_soil.C ends here.
