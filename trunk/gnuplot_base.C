// gnuplot_base.C -- Shared gnuplot interface code.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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
#include "vcheck.h"
#include "block_model.h"
#include "frame_submodel.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"

void 
GnuplotBase::Size::load_syntax (Frame& frame)
{
  frame.declare ("x", Attribute::None (), Attribute::Const, "\
Relative horizontal size of plot.");
  frame.declare ("y", Attribute::None (), Attribute::Const, "\
Relative vertical size of plot.");
  frame.order ("x", "y");
}
GnuplotBase::Size::Size (const FrameSubmodel* al)
  : x (al ? al->number ("x") : -42.42e42),
    y (al ? al->number ("y") : -42.42e42)
{ }
  
GnuplotBase::LegendTable::LegendTable ()
{
  (*this)["nw"] = "left Left reverse top";
  (*this)["ne"] = "right Right top";
  (*this)["sw"] = "left Left reverse bottom";
  (*this)["se"] = "right Right bottom";
  (*this)["below"] = "below";
  (*this)["outside"] = "outside";
  (*this)["none"] = "off";
}

GnuplotBase::LegendTable GnuplotBase::legend_table;

void
GnuplotBase::plot_header (std::ostream& out) const
{ 
  // Header.
  out << "reset\n";
  if (file != "screen")
    out << "set output " << quote (file) << "\n";
  else
    out << "unset output\n";

  if (device != "screen")
    out << "set terminal " << device;
  else if (getenv ("DISPLAY"))
    out << "set terminal x11";
  else 
    out << "set terminal windows"; 
  if (canvas != Attribute::None ())
    out << " size " << canvas;
  out << "\n";

  if (title != "")
    out << "set title " << quote (title) << "\n";

  if (size.x > 0.0)
    out << "set size " << size.x << ", " << size.y << "\n";
}

bool 
GnuplotBase::interactive () const
{ return device == "screen" || device == "windows" || device == "x11"; }

symbol
GnuplotBase::file2device (const symbol file_s)
{
  if (file_s == "screen" || file_s == "windows" || file_s == "x11")
    return file_s;

  const std::string file = file_s.name ();
  if (file.size () < 5 || file[file.size () - 4] != '.')
    return "unknown";

 std::string ext;
 ext += file[file.size () - 3];
 ext += file[file.size () - 2];
 ext += file[file.size () - 1];
 
 if (ext == "tex")
   return "pstricks";
 if (ext == "eps")
   return "postscript eps color";
 if (ext == "pdf")
   return "pdf";
 if (ext == "emf")
   return "emf";
 return "unknown";
}

GnuplotBase::GnuplotBase (const BlockModel& al)
  : Gnuplot (al),
    file (al.name ("where")),
    device (al.name ("device", file2device (file))),
    canvas (al.name ("canvas", Attribute::None ())),
    extra (al.name_sequence ("extra")),
    title (al.name ("title", "")),
    size (al.check ("size") ? &al.submodel ("size") : NULL),
    legend (al.name ("legend"))
{ }

GnuplotBase::~GnuplotBase ()
{ }


static struct GnuplotSyntax : public DeclareBase
{
  GnuplotSyntax ()
    : DeclareBase (Gnuplot::component, "common", "Common parameters.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("where", Attribute::OptionalConst, "\
File to store results in.  By default, show them on a window.\n\
The format is determined from the file name extension:\n\
  *.tex: LaTeX code with PostScript specials.\n\
  *.eps: Encapsulated PostScript.\n\
  *.pdf: Adobe PDF files.\n\
  *.emf: Enhanced Metafile.\n\
\n\
The special name 'screen' indicate that the data should be shown on\n\
the screen instead of being stored in a file.");
    static struct CheckWhere : public VCheck
    {
      bool verify (const Metalib&, const Frame& frame, const symbol key, 
                   Treelog& msg) const
      {
        daisy_assert (key == "where");
        daisy_assert (frame.lookup (key) == Attribute::String);
        daisy_assert (frame.type_size (key) == Attribute::Singleton);
        const symbol file = frame.name (key);
        if (file == "screen")
          return true;
        if (GnuplotBase::file2device (file) != "unknown")
          return true;
        msg.error ("Unknown file extension '" + file + "'");
        return false;
      }
    } check_where;
    // frame.set_check ("where", check_where);
    frame.set ("where", "screen");
    frame.declare_string ("device", Attribute::OptionalConst, "\
Output device.  By default, this is derived from the file extenstion.");
    frame.declare_string ("canvas", Attribute::OptionalConst, "\
Canvas size.  By default, this depend on the device.");
    frame.declare_string ("extra", Attribute::Const, 
                Attribute::Variable, "List of extra gnuplot commands.\n\
The commands will be inserted right before the plot command.\n\
Note that if you have multiple plots in the same command file,\n\
The extra commands may affect the subsequence plots.");
    frame.set_empty ("extra");
    frame.declare_string ("title", Attribute::OptionalConst, "\
Title of the plot, if any.  Set it to an empty string to disable.");
    frame.declare_submodule ("size", Attribute::OptionalConst, "\
Relative to size of plot.\n\
The standard size is 1.0, specify other numbers to scale accordingly.", 
                          GnuplotBase::Size::load_syntax);
    frame.declare_string ("legend", Attribute::OptionalConst, "\
Placement of legend.  This can be one of the four corners, named by\n\
compas locations (nw, ne, sw, se) to get the legend inside the graph\n\
in that corner, 'below' to get the legend below the graph, 'outside'\n\
to get the legend to the right of the graph, or 'none' to avoid\n\
getting a legend at all.\n\
\n\
The value 'auto' mean the legend will be places in the corner located\n\
farthest away from any data points.  Note that datapoints outside the\n\
graph are ignored, and so are the lines connecting the datapoints.  Thus,\n\
a line conncting two datapoints, one of them outside the graph, may\n\
cross the legend.");
    static struct CheckLegend : public VCheck
    {
      bool verify (const Metalib&, const Frame& frame, const symbol key, 
                   Treelog& msg) const
      {
        daisy_assert (key == "legend");
        daisy_assert (frame.lookup (key) == Attribute::String);
        daisy_assert (frame.type_size (key) == Attribute::Singleton);
        const symbol legend = frame.name (key);
        if (legend == "auto")
          return true;
        if (GnuplotBase::legend_table.find (legend) 
            != GnuplotBase::legend_table.end ())
          return true;
        msg.error (("Unknown legend placement '") + legend + "'");
        return false;
      }
    } check_legend;
    frame.set_check ("legend", check_legend);
    frame.set ("legend", "auto");
  }
} Gnuplot_syntax;

// gnuplot_base.C ends her.
