// gnuplot_time.C -- 2D plot with Daisy time on the X axes. 
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

#include "gnuplot.h"
#include "source.h"
#include "vcheck.h"
#include "treelog.h"
#include "mathlib.h"
#include "memutils.h"
#include <string>
#include <sstream>

struct GnuplotTime : public Gnuplot
{
  // Content.
  const std::string file;
  const std::string device;
  const std::vector<symbol> extra;
  const std::string title;

  struct Size 
  {
    const double x;
    const double y;
    static void load_syntax (Syntax& syntax, AttributeList&)
    {
      syntax.add ("x", Syntax::None (), Syntax::Const, "\
Relative horizontal size of plot.");
      syntax.add ("y", Syntax::None (), Syntax::Const, "\
Relative vertical size of plot.");
      syntax.order ("x", "y");
    }
    static const AttributeList& unset ()
    {
      static AttributeList alist;
      if (!alist.check ("x"))
	{
	  alist.add ("x", -42.42e42);
	  alist.add ("y", -42.42e42);
	}
      return alist;
    }
    explicit Size (const AttributeList& al)
      : x (al.number ("x")),
	y (al.number ("y"))
    { }
  };
  Size size;
  
  // Legend placement.
  static struct LegendTable : public std::map<std::string,std::string>
  {
    explicit LegendTable ()
    {
      (*this)["nw"] = "left Left reverse top";
      (*this)["ne"] = "right Right top";
      (*this)["sw"] = "left Left reverse bottom";
      (*this)["se"] = "right Right bottom";
      (*this)["below"] = "below";
      (*this)["outside"] = "outside";
      (*this)["none"] = "off";
    }
  } legend_table;
  std::string legend;

  // Ranges.
  const Time* begin;
  const Time* end;
  const bool ymin_flag;
  const double ymin;
  const bool ymax_flag;
  const double ymax;
  const bool y2min_flag;
  const double y2min;
  const bool y2max_flag;
  const double y2max;

  // Source.
  const std::vector<Source*> source;

  // Use.
  static std::string timeform (const Time& time);
  bool initialize (Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  static std::string file2device (const std::string& file);
  explicit GnuplotTime (Block& al);
  ~GnuplotTime ();
};

GnuplotTime::LegendTable GnuplotTime::legend_table;

std::string
GnuplotTime::timeform (const Time& time)
{
  std::ostringstream tmp;
  tmp << "\"" << time.year () << "-" << time.month () << "-" << time.mday ()
      << "T" << time.hour () << "\"";
  return tmp.str ();
}

bool
GnuplotTime::initialize (Treelog& msg)
{ 
  bool ok = true;
  for (size_t i = 0; i < source.size(); i++)
    {
      std::ostringstream tmp;
      tmp << name << "[" << i << "]: " << source[i]->title ();
      Treelog::Open nest (msg, tmp.str ());
      if (!source[i]->load (msg))
        ok = false;
      else if (source[i]->value ().size () < 1)
        msg.error ("No data in plot, ignoring");
    }
  return ok;
}

bool
GnuplotTime::plot (std::ostream& out, Treelog& msg)
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
  out << "\
set xtics nomirror autofreq\n\
set ytics nomirror\n\
set xdata time\n\
set timefmt \"%Y-%m-%dT%H\"\n\
set style data lines\n";
  if (title != "")
    out << "set title " << quote (title) << "\n";
  if (size.x > 0.0)
    out << "set size " << size.x << ", " << size.y << "\n";

  // Removed: set format x "%m-%y"

  // Dimensions.
  std::vector<std::string> dims;
  std::vector<int> axis;
  for (size_t i = 0; i < source.size (); i++)
    {
      if (source[i]->value ().size () < 1)
        {
          axis.push_back (-42);
          continue;
        }

      const std::string dim = source[i]->dimension ();
      
      for (size_t j = 0; j < dims.size (); j++)
        if (dim == dims[j])
          {
            axis.push_back (j);
            goto cont2;
          }
      axis.push_back (dims.size ());
      dims.push_back (dim);
    cont2: ;
    }
  switch (dims.size ())
    {
    case 2:
      out << "set y2tics\n";
      out << "set y2label " << quote (dims[1]) << "\n";
      out << "set ylabel " << quote (dims[0]) << "\n";
      break;
    case 1:
      out << "unset y2tics\n";
      out << "unset y2label\n";
      out << "set ylabel " << quote (dims[0]) << "\n";
      break;
    default:
      msg.error ("Can only plot one or two units at a time");
      return false;
    }

  // Legend.
  if (legend == "auto")
    {
      // Find ranges.
      Time soft_begin (9999, 12, 31, 23);
      Time soft_end (1,1,1,0);
      double soft_ymin = 1e99;
      double soft_ymax = -soft_ymin;
      double soft_y2min = soft_ymin;
      double soft_y2max = soft_ymax;

      for (size_t i = 0; i < source.size (); i++)
        if (source[i]->value ().size () < 1)
          /**/;
        else if (axis[i] == 0)
	  source[i]->limit (soft_begin, soft_end, soft_ymin, soft_ymax);
	else
	  source[i]->limit (soft_begin, soft_end, soft_y2min, soft_y2max);

      if (begin)
	soft_begin = *begin;
      if (end)
	soft_end = *end;
      if (ymin_flag)
	soft_ymin = ymin;
      if (ymax_flag)
	soft_ymax = ymax;
      if (y2min_flag)
	soft_y2min = y2min;
      if (y2max_flag)
	soft_y2max = y2max;

      // Find distances.
      double nw = 1.0;
      double ne = 1.0;
      double sw = 1.0;
      double se = 1.0;
      for (size_t i = 0; i < source.size (); i++)
	if (source[i]->value ().size () < 1)
          /**/;
        else if (axis[i] == 0)
	  source[i]->distance (soft_begin, soft_end, soft_ymin, soft_ymax,
			       nw, ne, sw, se);
	else
	  source[i]->distance (soft_begin, soft_end, soft_y2min, soft_y2max,
			       nw, ne, sw, se);

      // Choose closest.
      const double max_distance = std::max (std::max (nw, ne), 
					    std::max (sw, se));
      if (max_distance < 0.05)
	legend = "outside";
      else if (approximate (max_distance, ne))
	legend = "ne";
      else if (approximate (max_distance, nw))
	legend = "nw";
      else if (approximate (max_distance, se))
	legend = "se";
      else
	{
	  daisy_assert (approximate (max_distance, sw));
	  legend = "sw";
	}
    }
  out << "set key " << legend_table[legend] << "\n";

  // X Range
  out << "set xrange [";
  if (begin)
    out << timeform (*begin);
  else
    out << "*";
  out << ":";
  if (end)
    out << timeform (*end);
  else
    out << "*";
  out << "]\n";

  // Y range
  out << "set yrange [";
  if (ymin_flag)
    out << ymin;
  else
    out << "*";
  out << ":";
  if (ymax_flag)
    out << ymax;
  else
    out << "*";
  out << "]\n";
  if (dims.size () == 2)
    {
      out << "set y2range [";
      if (y2min_flag)
        out << y2min;
      else
        out << "*";
      out << ":";
      if (y2max_flag)
        out << y2max;
      else
        out << "*";
      out << "]\n";
    }
  else
    out << "unset y2range\n";

  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "plot ";
  int points = 0;
  int lines = 0;
  bool first = true;
  daisy_assert (axis.size () == source.size ());
  for (size_t i = 0; i < source.size (); i++)
    {
      if (source[i]->value ().size () < 1)
        continue;
      const std::string with = source[i]->with ();
      if (first)
        first = false;
      else
        out << ", ";
      out << "'-' using 1:2";
      if (with == "errorbars")
	out << ":3";
      out << " title " << quote (source[i]->title ());
      if (axis[i] == 1)
	out << " axes x1y2";
      else
	daisy_assert (axis[i] == 0);
      out << " with ";	
      const int style = source[i]->style ();
      out << with;
      if (with == "points" || with == "errorbars")
	out << " " << (style < 0 ? ++points : ((style == 0) ? points : style));
      else if (with == "lines")
	out << " " << (style < 0 ? ++lines :  ((style == 0) ? points : style));
      else 
	{
	  if (style >= 0)
	    out << " " << style;
	}
    }
  out << "\n";
  
  // Data.
  for (size_t i = 0; i < source.size (); i++)
    {
      if (source[i]->value ().size () < 1)
        continue;
      const bool use_ebars = source[i]->with () == "errorbars";
      const size_t size = source[i]->time ().size ();
      daisy_assert (size == source[i]->value ().size ());
      for (size_t j = 0; j < size; j++)
        {
          const Time time = source[i]->time ()[j];
          out << time.year () << "-" << time.month () << "-" << time.mday ()
              << "T" << time.hour () << "\t" << source[i]->value ()[j];
	  if (use_ebars)
	    out << "\t" << source[i]->ebar ()[j];
	  out << "\n";
        }
      out << "e\n";
    }

  // The end.
  if (device == "screen")
    out << "pause mouse\n";

  return true;
}

std::string 
GnuplotTime::file2device (const std::string& file)
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

GnuplotTime::GnuplotTime (Block& al)
  : Gnuplot (al),
    file (al.name ("where")),
    device (file2device (file)),
    extra (al.identifier_sequence ("extra")),
    title (al.name ("title", "")),
    size (al.check ("size")
	  ? al.alist ("size")
	  : Size::unset ()),
    legend (al.name ("legend")),
    begin (al.check ("begin") 
	   ? new Time (al.alist ("begin")) 
	   : NULL),
    end (al.check ("end")
	 ? new Time (al.alist ("end")) 
	 : NULL),
    ymin_flag (al.check ("ymin")),
    ymin (al.number ("ymin", 42.42e42)),
    ymax_flag (al.check ("ymax")),
    ymax (al.number ("ymax", 42.42e42)),
    y2min_flag (al.check ("y2min")),
    y2min (al.number ("y2min", 42.42e42)),
    y2max_flag (al.check ("y2max")),
    y2max (al.number ("y2max", 42.42e42)),
    source (Librarian<Source>::build_vector (al, "source"))
{ }

GnuplotTime::~GnuplotTime ()
{ 
  sequence_delete (source.begin (), source.end ()); 
  delete begin;
  delete end;
}

static struct GnuplotTimeSyntax
{
  static Gnuplot& make (Block& al)
  { return *new GnuplotTime (al); }
  GnuplotTimeSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description",
               "Generate a gnuplot command file."); 
    syntax.add ("cd", Syntax::Boolean, Syntax::Const, "\
Set this flag to add a 'cd' command to the current working directory.\n\
This is useful under MS Windows when dragging the file to a gnuplot icon.");
#if defined(__unix)
    alist.add ("cd", false);
#else
    alist.add ("cd", true);
#endif
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
	if (GnuplotTime::file2device (file) == "unknown")
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
			  GnuplotTime::Size::load_syntax);
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
	if (GnuplotTime::legend_table.find (legend) 
            == GnuplotTime::legend_table.end ())
          throw std::string ("Unknown legend placement '") + legend + "'";
      }
    } check_legend;
    syntax.add_check ("legend", check_legend);
    alist.add ("legend", "auto");

    syntax.add_submodule ("begin", alist, Syntax::OptionalConst,
			  "First date at x-axis.", Time::load_syntax);
    syntax.add_submodule ("end", alist, Syntax::OptionalConst,
			  "Last date at x-axis.", Time::load_syntax);
    syntax.add ("ymin", Syntax::User (), Syntax::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    syntax.add ("ymax", Syntax::User (), Syntax::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
    syntax.add ("y2min", Syntax::User (), Syntax::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    syntax.add ("y2max", Syntax::User (), Syntax::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
                
    syntax.add ("source", Librarian<Source>::library (), Syntax::State, 
		Syntax::Sequence, "\
Time series to plot.");
    Librarian<Gnuplot>::add_type ("time", alist, syntax, &make);
  }
} GnuplotTime_syntax;
