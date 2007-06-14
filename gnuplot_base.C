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
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"
#include "librarian.h"

void 
GnuplotBase::Size::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("x", Syntax::None (), Syntax::Const, "\
Relative horizontal size of plot.");
  syntax.add ("y", Syntax::None (), Syntax::Const, "\
Relative vertical size of plot.");
  syntax.order ("x", "y");
}
const AttributeList& GnuplotBase::Size::unset ()
{
  static AttributeList alist;
  if (!alist.check ("x"))
    {
      alist.add ("x", -42.42e42);
      alist.add ("y", -42.42e42);
    }
  return alist;
}

GnuplotBase::Size::Size (const AttributeList& al)
  : x (al.number ("x")),
     y (al.number ("y"))
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
  if (device != "screen")
    out << "set output " << quote (file) << "\n"
	<< "set terminal " << device << "\n";
  else if (getenv ("DISPLAY"))
    out << "unset output\n"
        << "set terminal x11\n";
  else 
    out << "unset output\n"
        << "set terminal windows\n";
  if (title != "")
    out << "set title " << quote (title) << "\n";
  if (size.x > 0.0)
    out << "set size " << size.x << ", " << size.y << "\n";
}

std::string 
GnuplotBase::file2device (const std::string& file)
{
  if (file == "screen")
    return "screen";
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
 return "unknown";
}

GnuplotBase::GnuplotBase (Block& al)
  : Gnuplot (al),
    file (al.name ("where")),
    device (file2device (file)),
    extra (al.identifier_sequence ("extra")),
    title (al.name ("title", "")),
    size (al.check ("size")
	  ? al.alist ("size")
	  : Size::unset ()),
    legend (al.name ("legend"))
{ }

GnuplotBase::~GnuplotBase ()
{ }

void 
GnuplotBase::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("base_model", "common");

  syntax.add ("where", Syntax::String, Syntax::OptionalConst, "\
File to store results in.  By default, show them on a window.\n\
The format is determined from the file name extension:\n\
  *.tex: LaTeX code with PostScript specials.\n\
  *.eps: Encapsulated PostScript.\n\
  *.pdf: Adobe PDF files.\n\
\n\
The special name 'screen' indicate that the data should be shown on\n\
the screen instead of being stored in a file.");
  static struct CheckWhere : public VCheck
  {
    void check (const Syntax& syntax, const AttributeList& alist, 
                const std::string& key) const throw (std::string)
    {
      daisy_assert (key == "where");
      daisy_assert (syntax.lookup (key) == Syntax::String);
      daisy_assert (syntax.size (key) == Syntax::Singleton);
      const std::string file = alist.name (key);
      if (file == "screen")
        return;
      if (GnuplotBase::file2device (file) == "unknown")
        throw std::string ("Unknown file extension '") + file + "'";
    }
  } check_where;
  syntax.add_check ("where", check_where);
  alist.add ("where", "screen");
  syntax.add ("extra", Syntax::String, Syntax::Const, 
              Syntax::Sequence, "List of extra gnuplot commands.\n\
The commands will be inserted right before the plot command.\n\
Note that if you have multiple plots in the same command file,\n\
The extra commands may affect the subsequence plots.");
  alist.add ("extra", std::vector<symbol> ());
  syntax.add ("title", Syntax::String, Syntax::OptionalConst, "\
Title of the plot, if any.  Set it to an empty string to disable.");
  syntax.add_submodule ("size", alist, Syntax::OptionalConst, "\
Relative to size of plot.\n\
The standard size is 1.0, specify other numbers to scale accordingly.", 
                        GnuplotBase::Size::load_syntax);
  syntax.add ("legend", Syntax::String, Syntax::OptionalConst, "\
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
    void check (const Syntax& syntax, const AttributeList& alist, 
                const std::string& key) const throw (std::string)
    {
      daisy_assert (key == "legend");
      daisy_assert (syntax.lookup (key) == Syntax::String);
      daisy_assert (syntax.size (key) == Syntax::Singleton);
      const std::string legend = alist.name (key);
      if (legend == "auto")
        return;
      if (GnuplotBase::legend_table.find (legend) 
          == GnuplotBase::legend_table.end ())
        throw std::string ("Unknown legend placement '") + legend + "'";
    }
  } check_legend;
  syntax.add_check ("legend", check_legend);
  alist.add ("legend", "auto");
}

static struct GnuplotSyntax
{
  GnuplotSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    GnuplotBase::load_syntax (syntax, alist);

    Librarian::add_base (Gnuplot::component, alist, syntax);
  }
} Gnuplot_syntax;
