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
#include "time.h"
#include "path.h"
#include "treelog.h"
#include "tmpstream.h"
#include "lexer_data.h"
#include "mathlib.h"
#include "path.h"
#include <string>

struct ProgramGnuplot : public Program
{
  // Content.
  const std::string program;
  const std::string tmpdir;
  
  // Source.
  struct Source
  {
    const std::string filename;
    const std::string tag;
    const std::string title;
    std::string dimension;
    const std::vector<std::string> missing;

    std::string field_sep;

    std::vector<Time> times;
    std::vector<double> values;

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
    static std::vector<std::string> s2s (const std::vector<symbol>&);
    Source (const AttributeList& al);
    ~Source ();
  };
  /* const */ std::vector<Source*> source;

  // Use.
  void run (Treelog& msg);
  
  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { }
  bool check (Treelog&)
  { return true; }
  ProgramGnuplot (const AttributeList& al)
    : Program (al),
      program (al.name ("program")),
      tmpdir (al.name ("directory")),
      source (map_construct<Source> (al.alist_sequence ("source")))
  { }
  ~ProgramGnuplot ()
  { sequence_delete (source.begin (), source.end ()); }
};


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
      else 
        msg.message ("Ignoring missing value '" + value + "'");
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
}

std::vector<std::string> 
ProgramGnuplot::Source::s2s (const std::vector<symbol>& v)
{
  std::vector<std::string> result;
  for (size_t i = 0; i < v.size (); i++)
    result.push_back (v[i].name ());
  return result;
}

ProgramGnuplot::Source::Source (const AttributeList& al)
  : filename (al.name ("file")),
    tag (al.name ("tag")),
    title (al.name ("title", tag)),
    dimension (al.name ("dimension", Syntax::Unknown ())),
    missing (s2s (al.identifier_sequence ("missing"))),
    field_sep ("UNINITIALIZED")
{ }

ProgramGnuplot::Source::~Source ()
{ }

void 
ProgramGnuplot::run (Treelog& msg)
{ 
  for (size_t i = 0; i < source.size(); i++)
    {
      TmpStream tmp;
      tmp () << name << "[" << i << "]: " << source[i]->tag;
      Treelog::Open nest (msg, tmp.str ());
      if (!source[i]->load (msg))
        return;
    }
  Path::InDirectory dir (tmpdir);
  if (!dir.check ())
    {
      msg.error ("Could not cd to '" + tmpdir + "'");
      return;
    }
  const std::string tmpfile = "daisy.gnuplot";
  Path::Output output (tmpfile);
  std::ostream& out = output.stream ();

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
  if (dims.size () > 0)
    out << "set ylabel \"" << dims[0] << "\"\n";

  // Header
  out << "\
set xdata time\n\
set timefmt \"%Y-%m-%d\"\n\
set style data lines\n\
plot ";

  
  // Sources.
  for (size_t i = 0; i < source.size (); i++)
    {
      if (i != 0)
        out << ", ";
      out << "'-' using 1:2 title \"" << source[i]->title << "\"";
    }
  out << "\n";
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

  out << "pause mouse\n";

  if (!output.good ())
    msg.error ("Problems writting to temporary file '" + tmpfile + "'");
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
    syntax.add ("program", Syntax::String, Syntax::Const, "\
File name of the gnuplot executable.");
    syntax.add ("directory", Syntax::String, Syntax::Const, "\
Temporary directory to run gnuplot in.");
    syntax.add_submodule_sequence ("source", Syntax::State, "\
Fertilizer application by date.", ProgramGnuplot::Source::load_syntax);
    Librarian<Program>::add_type ("gnuplot", alist, syntax, &make);
  }
} ProgramGnuplot_syntax;
