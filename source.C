// source.C -- Data source for gnuplot interface 
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

#include "source.h"
#include "units.h"
#include "librarian.h"
#include "vcheck.h"
#include "lexer_data.h"
#include "mathlib.h"
#include <sstream>

void 
Source::Filter::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file to filter for.");
  syntax.add ("allowed", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of allowable values in filter.");
  syntax.add_check ("allowed", VCheck::min_size_1 ());
  syntax.order ("tag", "allowed");
}

Source::Filter::Filter (const AttributeList& al)
  : tag (al.name ("tag")),
    allowed (al.name_sequence ("allowed"))
{ }

int
Source::find_tag (std::map<std::string,int>& tag_pos, const std::string& tag)
{
  if (tag_pos.find (tag) == tag_pos.end ())
    return -1;
  return tag_pos[tag];
}

int
Source::find_tag (std::map<std::string,int>& tag_pos, 
		  const std::string& tag1,
		  const std::string& tag2)
{
  int result = find_tag (tag_pos, tag1);
  return result < 0 ? find_tag (tag_pos, tag2) : result;
}

std::string
Source::get_entry (LexerData& lex) const
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
Source::get_entries (LexerData& lex) const
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
Source::get_date_component (LexerData& lex,
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
Source::get_time (const std::string& entry)
{
  int year;
  int month;
  int mday;
  int hour;
  char dummy;

  std::istringstream in (entry);
  
  in >> year >> dummy >> month >> dummy >> mday >> dummy >> hour;

  if (Time::valid (year, month, mday, hour))
    return Time (year, month, mday, hour);
  
  return Time (9999, 1, 1, 1);
}

double
Source::convert_to_double (LexerData& lex,
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
Source::load (Treelog& msg)
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
      if (with == "")
	with = "lines";
    }
  else if (type == "dlf-0.0")
    {
      field_sep = "\t";
      if (with == "")
	with = "lines";
    }
  else if (type == "ddf-0.0")
    {
      field_sep = "\t";
      if (with == "")
	with = "points";
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
  const int year_c = find_tag (tag_pos, "year", "Year");
  const int month_c = find_tag (tag_pos, "month", "Month");
  const int mday_c = find_tag (tag_pos, "mday", "Day");
  const int hour_c = find_tag (tag_pos, "hour", "Hour");
  const int time_c = find_tag (tag_pos, "time", "Date");

  if (tag_c < 0)
    {
      lex.error ("Tag '" + tag + "' not found");
      return false;
    }

  std::vector<size_t> fil_col;
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

  // Read dimensions.
  if (dim_line)
    {
      const std::vector<std::string> dim_names = get_entries (lex);
      if (dim_names.size () != tag_names.size ())
        if (dim_names.size () > tag_c)
          lex.warning ("Number of dimensions does not match number of tags");
        else
          {
            lex.error ("No dimension for '" + tag + "' found");
            return false;
          }
      if (original == Syntax::Unknown ())
        original = dim_names[tag_c];
      if (dimension == Syntax::Unknown ())
        dimension = dim_names[tag_c];
    }

  if (!has_factor && !Units::can_convert (original, dimension))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert from " << original << " to " << dimension;
      lex.error (tmp.str ());
      return false;
    }

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
      Time time (9999, 1, 1, 0);
      if (time_c < 0)
	{
	  int year = get_date_component (lex, entries, year_c, 1000);
	  int month = get_date_component (lex, entries, month_c, 1);
	  int mday = get_date_component (lex, entries, mday_c, 1);
	  int hour = get_date_component (lex, entries, hour_c, 0);

	  if (!Time::valid (year, month, mday, hour))
	    {
	      lex.warning ("Invalid date");
	      continue;
	    }
	  time = Time (year, month, mday, hour);
	}
      else
	time = get_time (entries[time_c]);

      if (time.year () == 9999)
	{
	  lex.warning ("Invalid time");
	  continue;
	}

      // Filter.
      for (size_t i = 0; i < filter.size (); i++)
	{
	  const std::vector<std::string>& allowed = filter[i]->allowed;
	  const std::string& v = entries[fil_col[i]];
	  if (std::find (allowed.begin (), allowed.end (), v) 
	      == allowed.end ())
	    goto cont;
	}

      // Extract value.
      {
        const std::string value = entries[tag_c];

        // Skip missing values.
        if (std::find (missing.begin (), missing.end (), value) 
            != missing.end ())
          continue;
        
        // Convert it.
        double val = convert_to_double (lex, value);
        if (has_factor)
          val *= factor;
        else if (Units::can_convert (original, dimension, val))
          val = Units::convert (original, dimension, val);
        else 
          {
            std::ostringstream tmp;
            tmp << "Cannot convert " << val << " from " << original 
                << " to " << dimension;
            lex.warning (tmp.str ());
          }

        // Store it.
        times.push_back (time);
        values.push_back (val);
      }
      // Next line.
    cont:;
    }

  // Done.
  return true;
}

void 
Source::limit (Time& begin, Time& end, double& ymin, double& ymax) const
{
  for (size_t i = 0; i < times.size (); i++)
    {
      if (times[i] < begin)
	begin = times[i];
      if (times[i] > end)
	end = times[i];
      if (values[i] < ymin)
	ymin = values[i];
      if (values[i] > ymax)
	ymax = values[i];
    }
}

void 
Source::distance (const Time begin, const Time end, 
		  const double ymin, const double ymax,
		  double& nw, double& ne, double& sw, double& se) const
  // Find relative distances to each corner.
{
  if (begin >= end || ymin >= ymax)
    // Null plot.
    return;
    
  for (size_t i = 0; i < times.size (); i++)
    {
      const double xr = (Time::hours_between (begin, times[i]) + 0.0)
	/ (Time::hours_between (begin, end) + 0.0);
      if (xr < 0.0 || xr > 1.0)
	// Outside graph.
	continue;
      const double yr = (values[i] - ymin) / (ymax - ymin);
      if (yr < 0.0 || yr > 1.0)
	// Outside graph.
	continue;
      
      // Distance from borders.
      const double  west = xr;
      const double  east = 1.0 - xr;
      const double north = 1.0 - yr;
      const double south = yr;
      
      // Distance from corners.
      nw = std::min (nw, std::max (north, west));
      ne = std::min (ne, std::max (north, east));
      sw = std::min (sw, std::max (south, west));
      se = std::min (se, std::max (south, east));
    }
}

void 
Source::load_syntax (Syntax& syntax, AttributeList& al)
{
  syntax.add ("file", Syntax::String, Syntax::Const, "\
Name of Daisy log file where data is found.");
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file where data is found.");
  syntax.add ("title", Syntax::String, Syntax::OptionalConst, "\
Name of data legend in plot, by default the same as 'tag'.");
  syntax.add ("original", Syntax::String, Syntax::OptionalConst, "\
Dimension of the data in the data file.\n\
By default use the name specified in data file.");
  syntax.add ("dimension", Syntax::String, Syntax::OptionalConst, "\
Dimension of data to plot.\n\
By default this is the same as 'original'.\n\
If 'factor' is not specified, Daisy will attempt to convert the data.");
  syntax.add ("dim_line", Syntax::Boolean, Syntax::OptionalConst, "\
If true, assume the line after the tags contain dimensions.\n\
By default this will be true iff 'original' is not specified.");
  syntax.add ("factor", Syntax::Unknown (), Syntax::OptionalConst, "\
Multiply all data by this number.\n\
By default will try to find a convertion from 'original' to 'dimension'.");
  syntax.add ("missing", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of strings indicating 'missing value'.");
  std::vector<symbol> misses;
  misses.push_back (symbol (""));
  misses.push_back (symbol ("00.00"));
  al.add ("missing", misses);
  syntax.add_submodule_sequence ("filter", Syntax::Const, "\
Only include data from rows that passes all these filters.",
                                 Source::Filter::load_syntax);
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

Source::Source (const AttributeList& al)
  : filename (al.name ("file")),
    tag (al.name ("tag")),
    title (al.name ("title", tag)),
    original (al.name ("dimension", Syntax::Unknown ())),
    dimension (al.name ("dimension", original)),
    dim_line (al.flag ("dim_line", !al.check ("original"))),
    has_factor (al.check ("factor")),
    factor (al.number ("factor", 1.0)),
    with (al.name ("with", "")),
    style (al.integer ("style", -1)),
    missing (al.name_sequence ("missing")),
    field_sep ("UNINITIALIZED"),
    filter (map_construct_const<Filter> (al.alist_sequence ("filter")))
{ }

Source::~Source ()
{ sequence_delete (filter.begin (), filter.end ()); }

