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
#include "vcheck.h"
#include "time.h"
#include "treelog.h"
#include "tmpstream.h"
#include "lexer_data.h"
#include "path.h"
#include "mathlib.h"
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
  struct Source;
  const std::vector<Source*> source;

  // Use.
  static std::string timerange (const Time& time);
  void run (Treelog& msg);
  
  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { }
  bool check (Treelog&)
  { return true; }
  static std::string file2device (const std::string& file);
  ProgramGnuplot (const AttributeList& al);
  ~ProgramGnuplot ();
};


struct ProgramGnuplot::Source
{
  const std::string filename;
  const std::string tag;
  const std::string title;
  std::string dimension;
  const std::string with;
  const int style;
  const std::vector<std::string> missing;
  std::string field_sep;
  std::vector<Time> times;
  std::vector<double> values;
  
  // Filter.
  struct Filter
  {
    const std::string tag;
    const std::vector<std::string> allowed;
    static void load_syntax (Syntax& syntax, AttributeList&);
    Filter (const AttributeList&);
  };
  std::vector<const Filter*> filter;

  // Read.
  static int find_tag (std::map<std::string,int>& tag_pos,
                       const std::string& tag);
  std::string get_entry (LexerData& lex) const;
  std::vector<std::string> get_entries (LexerData& lex) const;
  static int get_date_component (LexerData& lex,
                                 const std::vector<std::string>& entries, 
                                 int column, 
                                 int default_value);
  static double convert_to_double (LexerData& lex, const std::string& value);
  bool load (Treelog& msg);

  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList&);
  Source (const AttributeList& al);
  ~Source ();
};

void 
ProgramGnuplot::Source::Filter::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file to filter for.");
  syntax.add ("allowed", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of allowable values in filter.");
  syntax.add_check ("allowed", VCheck::min_size_1 ());
  syntax.order ("tag", "allowed");
}

ProgramGnuplot::Source::Filter::Filter (const AttributeList& al)
  : tag (al.name ("tag")),
    allowed (al.name_sequence ("allowed"))
{ }

int
ProgramGnuplot::Source::find_tag (std::map<std::string,int>& tag_pos,
                                  const std::string& tag)
{
  if (tag_pos.find (tag) == tag_pos.end ())
    return -1;
  return tag_pos[tag];
}

std::string
ProgramGnuplot::Source::get_entry (LexerData& lex) const
{
  std::string tmp_term;  // Data storage.
  const char* field_term;

  switch (field_sep.size ())
    { 
    case 0:
      // Whitespace
      field_term = " \t\n";
      break;
    case 1:
      // Single character field seperator.
      tmp_term = field_sep + "\n";
      field_term = tmp_term.c_str ();
      break;
    default:
      // Multi-character field seperator.
      daisy_assert (false);
    }

  // Find it.
  std::string entry = "";
  while (lex.good ())
    {
      int c = lex.peek ();
      if (strchr (field_term, c))
	break;
      entry += int2char (lex.get ());
    }
  return entry;
}

std::vector<std::string>
ProgramGnuplot::Source::get_entries (LexerData& lex) const
{
  lex.skip ("\n");
  std::vector<std::string> entries;

  while (lex.good ())
    {
      entries.push_back (get_entry (lex));

      if (lex.peek () == '\n')
        break;

      if (field_sep == "")
	lex.skip_space ();
      else
	lex.skip(field_sep.c_str ());
    }
  return entries;
}

int
ProgramGnuplot::Source::get_date_component (LexerData& lex,
                                            const std::vector<std::string>&
                                            /**/ entries, 
                                            int column, 
                                            int default_value)
{
  if (column < 0)
    return default_value;
  daisy_assert (column < entries.size ());
  const char *const str = entries[column].c_str ();
  const char* end_ptr = str;
  const long lval = strtol (str, const_cast<char**> (&end_ptr), 10);
  if (*end_ptr != '\0')
    lex.error (std::string ("Junk at end of number '") + end_ptr + "'");
  const int ival = lval;
  if (ival != lval)
    lex.error ("Number out of range");
  return ival;
}

double
ProgramGnuplot::Source::convert_to_double (LexerData& lex,
                                           const std::string& value)
{
  const char *const str = value.c_str ();
  const char* end_ptr = str;
  const double val = strtod (str, const_cast<char**> (&end_ptr));
  if (*end_ptr != '\0')
    lex.error (std::string ("Junk at end of number '") + end_ptr + "'");
  return val;
}

bool
ProgramGnuplot::Source::load (Treelog& msg)
{
  LexerData lex (filename, msg);

  // Open errors?
  if (!lex.good ())
    return false;

  // Read first line.
  const std::string type = lex.get_word ();
  if (type == "dwf-0.0")
    {
      field_sep = "";
    }
  else if (type == "dlf-0.0" || type == "ddf-0.0")
    {
      field_sep = "\t";
    }
  else
    lex.error ("Unknown file type '" + type + "'");
  lex.skip_line ();
  lex.next_line ();

  // Skip keywords.
  while (lex.good () && lex.peek () != '-')
    {
      lex.skip_line ();
      lex.next_line ();
    }

  // Skip hyphens.
  while (lex.good () && lex.peek () == '-')
    lex.get ();
  lex.skip_space ();
  
  // Read tags.
  std::map<std::string,int> tag_pos;
  const std::vector<std::string> tag_names = get_entries (lex);
  for (int count = 0; count < tag_names.size (); count++)
    {
      const std::string candidate = tag_names[count];
      if (tag_pos.find (candidate) == tag_pos.end ())
        tag_pos[candidate] = count;
      else
	lex.warning ("Duplicate tag: " + candidate);
    }

  const int tag_c = find_tag (tag_pos, tag);
  const int year_c = find_tag (tag_pos, "year");
  const int month_c = find_tag (tag_pos, "month");
  const int mday_c = find_tag (tag_pos, "mday");
  const int hour_c = find_tag (tag_pos, "hour");
  if (tag_c < 0)
    {
      lex.error ("Tag '" + tag + "' not found");
      return false;
    }

  // Read dimensions.
  const std::vector<std::string> dim_names = get_entries (lex);
  if (dim_names.size () != tag_names.size ())
    if (dim_names.size () > tag_c)
      lex.warning ("Number of dimensions does not match number of tags");
    else
      {
	lex.error ("No dimension for '" + tag + "' found");
	return false;
      }
  if (dimension == Syntax::Unknown ())
    dimension = dim_names[tag_c];

  // Read data.
  while (lex.good ())
    {
      // Read entries.
      const std::vector<std::string> entries = get_entries (lex);

      if (entries.size () != tag_names.size ())
        {
          if (entries.size () != 0 && lex.good ())
            lex.warning ("Wrong number of entries on this line");
          continue;
        }

      // Extract date.
      int year = get_date_component (lex, entries, year_c, 1000);
      int month = get_date_component (lex, entries, month_c, 1);
      int mday = get_date_component (lex, entries, mday_c, 1);
      int hour = get_date_component (lex, entries, hour_c, 0);
      
      if (!Time::valid (year, month, mday, hour))
        {
          lex.warning ("Invalid date");
          continue;
        }
      const Time time (year, month, mday, hour);

      // Extract value.
      const std::string value = entries[tag_c];
      if (std::find (missing.begin (), missing.end (), value) 
          == missing.end ())
        {
          times.push_back (time);
          const double val = convert_to_double (lex, value);
          values.push_back (val);
        }
    }

  // Done.
  return true;
}

void 
ProgramGnuplot::Source::load_syntax (Syntax& syntax, AttributeList& al)
{
  syntax.add ("file", Syntax::String, Syntax::Const, "\
Name of Daisy log file where data is found.");
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file where data is found.");
  syntax.add ("title", Syntax::String, Syntax::OptionalConst, "\
Name of data legend in plot, by default the same as 'tag'.");
  syntax.add ("dimension", Syntax::String, Syntax::OptionalConst, "\
Dimension of data for use by y-axis.\n\
By default use the name specified in data file.");
  syntax.add ("missing", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of strings indicating 'missing value'.");
  std::vector<symbol> misses;
  misses.push_back (symbol (""));
  misses.push_back (symbol ("00.00"));
  al.add ("missing", misses);
  syntax.add_submodule_sequence ("filter", Syntax::Const, "\
Only include data from rows that passes all these filters.",
                                 ProgramGnuplot::Source::Filter::load_syntax);
  al.add ("filter", std::vector<AttributeList*> ());
  syntax.add ("with", Syntax::String, Syntax::OptionalConst, "\
Specify 'points' to plot each point individually, or 'lines' to draw\n\
lines between them.  By default, data from dwf and dlf files will be\n\
drawn with lines, and data from ddf files will be drawn with points.");
  static VCheck::Enum with ("lines", "points");
  syntax.add_check ("with", with);
  syntax.add ("style", Syntax::Integer, Syntax::OptionalConst, "\
Style to use for this dataset.  By default, gnuplot will use style 1\n\
for the first source to plot with lines, style 2 for the second, and\n\
so forth until it runs out of styles and has to start over.  Points\n\
work similar, but with its own style counter.  For color plots, points\n\
and lines with the same style number also have the same color.");
}

ProgramGnuplot::Source::Source (const AttributeList& al)
  : filename (al.name ("file")),
    tag (al.name ("tag")),
    title (al.name ("title", tag)),
    dimension (al.name ("dimension", Syntax::Unknown ())),
    with (al.name ("with", "")),
    style (al.integer ("style", -1)),
    missing (al.name_sequence ("missing")),
    field_sep ("UNINITIALIZED"),
    filter (map_construct_const<Filter> (al.alist_sequence ("filter")))
{ }

ProgramGnuplot::Source::~Source ()
{ sequence_delete (filter.begin (), filter.end ()); }

std::string
ProgramGnuplot::timerange (const Time& time)
{
  std::stringstream tmp;
  tmp << "\"" << time.year () << "-" << time.month () << "-" << time.mday ()
      << "T" << time.hour () << "\"";
  return tmp.str ();
}

void 
ProgramGnuplot::run (Treelog& msg)
{ 
  for (size_t i = 0; i < source.size(); i++)
    {
      TmpStream tmp;
      tmp () << name << "[" << i << "]: " << source[i]->tag;
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
    {
      std::string escdir;
      for (size_t i = 0; i < dir.size (); i++)
	if (dir[i] == '\\')
	  escdir += "\\\\";
	else
	  escdir += dir[i];
      out << "cd \"" << escdir << "\"\n";
    }

  // Header.
  if (device != "default")
    out << "set output \"" << file << "\"\n"
	<< "set terminal " << device << "\n";
  else if (getenv ("DISPLAY"))
    out << "unset output\n"
        << "set terminal x11\n";
  else 
    out << "unset output\n"
        << "set terminal windows\n";
  out << "\
set xtics nomirror\n\
set ytics nomirror\n\
set y2tics\n\
set xdata time\n\
set format x \"%m-%y\"\n\
set timefmt \"%Y-%m-%dT%H\"\n\
set style data lines\n";

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
      out << "set y2label \"" << dims[1] << "\"\n";
      out << "set ylabel \"" << dims[0] << "\"\n";
      break;
    case 1:
      out << "unset y2label\n";
      out << "set ylabel \"" << dims[0] << "\"\n";
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
  
  daisy_assert (axis.size () == source.size ());
  for (size_t i = 0; i < source.size (); i++)
    {
      if (i != 0)
        out << ", ";
      out << "'-' using 1:2 title \"" << source[i]->title << "\"";
      if (axis[i] == 1)
	out << " axes x1y2";
      else
	daisy_assert (axis[i] == 0);
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
   return "pslatex";
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
               "Generate plot from Daisy log files with gnuplot.\n\
UNDER CONSTRUCTION, DO NOT USE!"); 
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
Data sources to plot.", ProgramGnuplot::Source::load_syntax);
    Librarian<Program>::add_type ("gnuplot", alist, syntax, &make);
  }
} ProgramGnuplot_syntax;
