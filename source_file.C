// source_file.C -- File source for gnuplot interface 
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


#include "source_file.h"
#include "lexer_data.h"
#include "submodeler.h"
#include "vcheck.h"
#include "mathlib.h"
#include "memutils.h"
#include <sstream>

static int
find_tag (const std::map<std::string,int>& tag_pos, 
	  const std::string& tag1,
	  const std::string& tag2)
{
  int result = find_tag (tag_pos, tag1);
  return result < 0 ? find_tag (tag_pos, tag2) : result;
}

class SourceFile::Filter
{
public:
  const std::string tag;
private:
  const std::vector<std::string> allowed;
public:
  bool match (const std::string& value) const
  {
    for (size_t i = 0; i < allowed.size (); i++)
      {
        if (allowed[i] == value)
          return true;
        // Try to pad out our allowed value with spaces...
        if (value.size () <= allowed[i].size ())
          continue;
        const std::string allow
          = allowed[i] + std::string (value.size () - allowed[i].size (), ' ');
        daisy_assert (allow.size () == value.size ());
        if (allow == value)
          return true;
      }
    return false;
  }
  static void load_syntax (Syntax& syntax, AttributeList&);
  explicit Filter (Block&);
};

void 
SourceFile::Filter::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file to filter for.");
  syntax.add ("allowed", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of allowable values in filter.");
  syntax.add_check ("allowed", VCheck::min_size_1 ());
  syntax.order ("tag", "allowed");
}

SourceFile::Filter::Filter (Block& al)
  : tag (al.name ("tag")),
    allowed (al.name_sequence ("allowed"))
{ }

std::string
SourceFile::get_entry (LexerData& lex) const
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
SourceFile::get_entries (LexerData& lex) const
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
SourceFile::get_date_component (LexerData& lex,
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

Time 
SourceFile::get_time (const std::string& entry)
{
  std::istringstream in (entry);

  int val1;
  char sep1;
  int val2;
  char sep2;
  int val3;
  in >> val1 >> sep1 >> val2 >> sep2 >> val3;

  if (sep1 != sep2)
    return Time (9999, 1, 1, 1);

  int hour = 8;
  if (in.good () && !in.eof ())
    {
      char sep3;
      in >> sep3 >> hour;
      if (sep3 != 'T')
        return Time (9999, 1, 1, 1);
    }        

  int mday;
  int month;
  int year;
  if (sep1 == '-')
    {
      year = val1;
      month = val2;
      mday = val3;
    }
  else if (sep1 == '/')
    {
      mday = val1;
      month = val2;
      year = val3;
    }
  else
    return Time (9999, 1, 1, 1);

  if (!Time::valid (year, month, mday, hour))
    return Time (9999, 1, 1, 1);

  return Time (year, month, mday, hour);
}

double
SourceFile::convert_to_double (LexerData& lex,
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
SourceFile::read_header (LexerData& lex)
{
  // Open errors?
  if (!lex.good ())
    return false;

  // Read first line.
  const std::string type = lex.get_word ();
  if (type == "dwf-0.0")
    {
      field_sep = "";
      if (with_ == "")
	with_ = "lines";
    }
  else if (type == "dlf-0.0")
    {
      field_sep = "\t";
      if (with_ == "")
	with_ = "lines";
    }
  else if (type == "ddf-0.0")
    {
      field_sep = "\t";
      if (with_ == "")
	with_ = "points";
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
  tag_names = get_entries (lex);
  for (int count = 0; count < tag_names.size (); count++)
    {
      const std::string candidate = tag_names[count];
      if (tag_pos.find (candidate) == tag_pos.end ())
        tag_pos[candidate] = count;
      else
	lex.warning ("Duplicate tag: '" + candidate + "'");
    }

  // Time tags.
  year_c = find_tag (tag_pos, "year", "Year");
  month_c = find_tag (tag_pos, "month", "Month");
  mday_c = find_tag (tag_pos, "mday", "Day");
  hour_c = find_tag (tag_pos, "hour", "Hour");
  time_c = find_tag (tag_pos, "time", "Date");

  // Filter tags.
  for (size_t i = 0; i < filter.size (); i++)
    {
      int c = find_tag (tag_pos, filter[i]->tag);
      if (c < 0)
	{
	  lex.error ("Filter tag '" + filter[i]->tag + "' not found");
	  return false;
	}
      fil_col.push_back (c);
    }
  
  return lex.good ();
}

bool
SourceFile::read_entry (LexerData& lex, 
                        std::vector<std::string>& entries,
                        Time& time) const
{
  // Read entries.
  entries = get_entries (lex);

  if (entries.size () != tag_names.size ())
    {
      if (entries.size () != 0 && lex.good ())
        {
          std::ostringstream tmp;
          tmp << "Got " << entries.size () << " entries, expected "
              << tag_names.size ();
          lex.warning (tmp.str ());
          while (entries.size () < tag_names.size ())
            entries.push_back ("");
        }
      else 
        return false;
    }

  // Extract date.
  if (time_c < 0)
    {
      int year = get_date_component (lex, entries, year_c, 1000);
      int month = get_date_component (lex, entries, month_c, 1);
      int mday = get_date_component (lex, entries, mday_c, 1);
      int hour = get_date_component (lex, entries, hour_c, 0);

      if (!Time::valid (year, month, mday, hour))
        {
          std::ostringstream tmp;
          tmp << year << "-" << month << "-" << mday << "T" << hour 
              << ": invalid date";
          lex.warning (tmp.str ());
          return false;
        }
      else
        time = Time (year, month, mday, hour);

      if (time.year () == 9999)
        {
          std::ostringstream tmp;
          tmp << time.year () << "-" << time.month () << "-" << time.mday () 
              << "T" << time.hour () << ": invalid date";
          lex.warning (tmp.str ());
          return false;
        }
    }
  else
    {
      time = get_time (entries[time_c]);

      if (time.year () == 9999)
        {
          lex.warning (entries[time_c] + ": invalid time");
          return false;
        }
    }

  // Filter.
  for (size_t i = 0; i < filter.size (); i++)
    if (!filter[i]->match (entries[fil_col[i]]))
      return false;
  
  // If we survived here, everything is fine.
  return true;
}

void
SourceFile::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Source::load_syntax (syntax, alist);
  syntax.add ("file", Syntax::String, Syntax::Const, "\
Name of Daisy log file where data is found.");
  syntax.add ("missing", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of strings indicating missing values.");
  std::vector<symbol> misses;
  misses.push_back (symbol (""));
  misses.push_back (symbol ("00.00"));
  alist.add ("missing", misses);
  syntax.add_submodule_sequence ("filter", Syntax::Const, "\
Only include data from rows that passes all these filters.",
				 SourceFile::Filter::load_syntax);
  alist.add ("filter", std::vector<AttributeList*> ());
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

SourceFile::SourceFile (Block& al)
  : Source (al),
    filename (al.name ("file")),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1)),
    missing (al.name_sequence ("missing")),
    filter (map_submodel_const<Filter> (al, "filter")),
    field_sep ("UNINITIALIZED"),
    year_c (-42),
    month_c (-42),
    mday_c (-42),
    hour_c (-42),
    time_c (-42)
{ }

SourceFile::~SourceFile ()
{ sequence_delete (filter.begin (), filter.end ()); }

// source_file.h ends here.
