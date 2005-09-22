// program_gnuplot.C -- gnuplot interface 
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

#include "program.h"
#include "source.h"
#include "vcheck.h"
#include "treelog.h"
#include "path.h"
#include <string>
#include <set>
#include <fstream>
#include <sstream>

struct ProgramGnuplot : public Program
{
  // Content.
  const std::string command_file;
  const enum { append_yes, append_no, append_maybe } append;
  const bool do_cd;
  const std::string file;
  const std::string device;
  const std::vector<symbol> extra;

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
    }
  } legend_table;
  const std::string legend;

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
  static std::string quote (const std::string& value);
  static std::string timerange (const Time& time);
  void run (Treelog& msg);
  
  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { }
  bool check (Treelog&)
  { return true; }
  static std::string file2device (const std::string& file);
  explicit ProgramGnuplot (const AttributeList& al);
  ~ProgramGnuplot ();
};

ProgramGnuplot::LegendTable ProgramGnuplot::legend_table;

std::string 
ProgramGnuplot::quote (const std::string& value)
{ return "'" + value + "'"; }

std::string
ProgramGnuplot::timerange (const Time& time)
{
  std::ostringstream tmp;
  tmp << "\"" << time.year () << "-" << time.month () << "-" << time.mday ()
      << "T" << time.hour () << "\"";
  return tmp.str ();
}

void 
ProgramGnuplot::run (Treelog& msg)
{ 
  for (size_t i = 0; i < source.size(); i++)
    {
      std::ostringstream tmp;
      tmp << name << "[" << i << "]: " << source[i]->title;
      Treelog::Open nest (msg, tmp.str ());
      if (!source[i]->load (msg))
        throw 1;
    }
  
  // We open for append if we have used this file before.
  const std::string dir = Path::get_directory ();
  static std::set<std::string> already_opened;
  const std::string me = dir + "/" + command_file;
  std::ios::openmode  flags = std::ios::out;
  if (append == append_yes
      || (append == append_maybe 
	  && already_opened.find (me) != already_opened.end ()))
    flags |= std::ios::app;
  already_opened.insert (me);
  std::ofstream out (command_file.c_str (), flags);
  if (do_cd)
    out << "cd " << quote (dir) << "\n";

  // Header.
  if (device != "default")
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
  if (legend != "")
    out << "set keys " << legend_table[legend];

  // Removed: set format x "%m-%y"

  // Dimensions.
  std::vector<std::string> dims;
  std::vector<int> axis;
  for (size_t i = 0; i < source.size (); i++)
    {
      const std::string dim = source[i]->dimension;
      
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
      out << "unset y2label\n";
      out << "set ylabel " << quote (dims[0]) << "\n";
      break;
    default:
      msg.error ("Can only plot one or two units at a time");
      throw 1;
    }

  // X Range
  out << "set xrange [";
  if (begin)
    out << timerange (*begin);
  out << ":";
  if (end)
    out << timerange (*end);
  out << "]\n";

  // Y range
  out << "set yrange [";
  if (ymin_flag)
    out << ymin;
  out << ":";
  if (ymax_flag)
    out << ymax;
  out << "]\n";
  out << "set y2range [";
  if (y2min_flag)
    out << y2min;
  out << ":";
  if (y2max_flag)
    out << y2max;
  out << "]\n";

  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "plot ";
  int points = 0;
  int lines = 0;
  daisy_assert (axis.size () == source.size ());
  for (size_t i = 0; i < source.size (); i++)
    {
      if (i != 0)
        out << ", ";
      out << "'-' using 1:2 title " << quote (source[i]->title);
      if (axis[i] == 1)
	out << " axes x1y2";
      else
	daisy_assert (axis[i] == 0);
      out << " with ";	
      const std::string with = source[i]->with;
      const int style = source[i]->style;
      if (with == "points")
	out << "points " << (style < 0 ? ++points : style);
      else if (with == "lines")
	out << "lines " << (style < 0 ? ++lines : style);
      else 
	{
	  out << with;
	  if (style >= 0)
	    out << " " << style;
	}
    }
  out << "\n";
  
  // Data.
  for (size_t i = 0; i < source.size (); i++)
    {
      const size_t size = source[i]->times.size ();
      daisy_assert (size == source[i]->values.size ());
      for (size_t j = 0; j < size; j++)
        {
          const Time time = source[i]->times[j];
          out << time.year () << "-" << time.month () << "-" << time.mday ()
              << "T" << time.hour () << "\t" << source[i]->values[j] << "\n";
        }
      out << "e\n";
    }

  // The end.
  if (device == "default")
    out << "pause mouse\n";

  if (!out.good ())
    {
      msg.error ("Problems writting to temporary file '" + command_file + "'");
      throw 1;
    }
}

std::string 
ProgramGnuplot::file2device (const std::string& file)
{
  if (file == "")
    return "default";
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

ProgramGnuplot::ProgramGnuplot (const AttributeList& al)
  : Program (al),
    command_file (al.name ("command_file")),
    append (al.check ("append") 
            ? (al.flag ("append") ? append_yes : append_no)
            : append_maybe),
    do_cd (al.flag ("cd")),
    file (al.name ("where", "")),
    device (file2device (file)),
    extra (al.identifier_sequence ("extra")),
    legend (al.name ("legend", "")),
    begin (al.check ("begin") ? new Time (al.alist ("begin")) : NULL),
    end (al.check ("end") ? new Time (al.alist ("end")) : NULL),
    ymin_flag (al.check ("ymin")),
    ymin (al.number ("ymin", 42.42e42)),
    ymax_flag (al.check ("ymax")),
    ymax (al.number ("ymax", 42.42e42)),
    y2min_flag (al.check ("y2min")),
    y2min (al.number ("y2min", 42.42e42)),
    y2max_flag (al.check ("y2max")),
    y2max (al.number ("y2max", 42.42e42)),
    source (map_construct<Source> (al.alist_sequence ("source")))
{ }

ProgramGnuplot::~ProgramGnuplot ()
{ 
  sequence_delete (source.begin (), source.end ()); 
  delete begin;
  delete end;
}

static struct ProgramGnuplotSyntax
{
  static Program&
  make (const AttributeList& al)
  { return *new ProgramGnuplot (al); }
  ProgramGnuplotSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description",
               "Generate a gnuplot command file."); 
    syntax.add ("command_file", Syntax::String, Syntax::Const, "\
File name for gnuplot commands.");
    alist.add ("command_file", "daisy.gnuplot");
    syntax.add ("append", Syntax::Boolean, Syntax::OptionalConst, "\
Set this to true to append rather the rewrite the command file.\n\
By default, do the right thing.");
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
*.pdf: Adobe PDF files.");
    static struct CheckWhere : public VCheck
    {
      void check (const Syntax& syntax, const AttributeList& alist, 
		  const std::string& key) const throw (std::string)
      {
	daisy_assert (key == "where");
	daisy_assert (syntax.lookup (key) == Syntax::String);
	daisy_assert (syntax.size (key) == Syntax::Singleton);
	const std::string file = alist.name (key);
	if (ProgramGnuplot::file2device (file) == "unknown")
	  throw std::string ("Unknown file extension '") + file + "'";
      }
    } check_where;
    syntax.add_check ("where", check_where);
    syntax.add ("extra", Syntax::String, Syntax::Const, 
                Syntax::Sequence, "List of extra gnuplot commands.\n\
The commands will be inserted right before the plot command.\n\
Note that if you have multiple plots in the same command file,\n\
The extra commands may affect the subsequence plots.");
    alist.add ("extra", std::vector<symbol> ());
    syntax.add ("legend", Syntax::String, Syntax::OptionalConst, "\
Placement of legend.  This can be one of the four corners, named by\n\
compas locations (nw, ne, sw, se) to get the legend inside the graph\n\
in that corner, 'below' to get the legend below the graph, 'outside'\n\
to get the legend to the right of the graph, or 'none' to avoid\n\
getting a legend at all.\n\
\n\
By default the legend will be places in the corner located farthest\n\
away from any data points.");
    static struct CheckLegend : public VCheck
    {
      void check (const Syntax& syntax, const AttributeList& alist, 
		  const std::string& key) const throw (std::string)
      {
	daisy_assert (key == "legend");
	daisy_assert (syntax.lookup (key) == Syntax::String);
	daisy_assert (syntax.size (key) == Syntax::Singleton);
	const std::string legend = alist.name (key);
	if (ProgramGnuplot::legend_table.find (legend) 
            == ProgramGnuplot::legend_table.end ())
          throw std::string ("Unknown legend placement '") + legend + "'";
      }
    } check_legend;
    syntax.add_check ("legend", check_legend);
    
    syntax.add_submodule ("begin", alist, Syntax::OptionalConst,
			  "First date at x-axis.", Time::load_syntax);
    syntax.add_submodule ("end", alist, Syntax::OptionalConst,
			  "Last date at x-axis.", Time::load_syntax);
    syntax.add ("ymin", Syntax::Unknown (), Syntax::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    syntax.add ("ymax", Syntax::Unknown (), Syntax::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
    syntax.add ("y2min", Syntax::Unknown (), Syntax::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    syntax.add ("y2max", Syntax::Unknown (), Syntax::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
                
    syntax.add_submodule_sequence ("source", Syntax::State, "\
Data sources to plot.", Source::load_syntax);
    Librarian<Program>::add_type ("gnuplot", alist, syntax, &make);
  }
} ProgramGnuplot_syntax;
