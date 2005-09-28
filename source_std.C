// source_std.C -- Data source for gnuplot interface 
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
#include "units.h"
#include "librarian.h"
#include "lexer_data.h"
#include "mathlib.h"
#include <sstream>

struct SourceStandard : public SourceFile
{
  // Content.
  const std::string tag;
  const std::string title_;
  std::string original;
  std::string dimension_;
  const bool dim_line;
  const bool has_factor;
  const double factor;

  // Interface.
  const std::string& title () const
  { return title_; }
  const std::string& dimension () const 
  { return dimension_; }

  // Read.
  static int find_tag (std::map<std::string,int>& tag_pos,
                       const std::string& tag);
  static int find_tag (std::map<std::string,int>& tag_pos,
                       const std::string& tag1,
                       const std::string& tag2);
  std::string get_entry (LexerData& lex) const;
  std::vector<std::string> get_entries (LexerData& lex) const;
  static int get_date_component (LexerData& lex,
                                 const std::vector<std::string>& entries, 
                                 int column, 
                                 int default_value);
  static Time get_time (const std::string& entry);
  static double convert_to_double (LexerData& lex, const std::string& value);
public:
  bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit SourceStandard (const AttributeList& al);
  ~SourceStandard ();
};

int
SourceStandard::find_tag (std::map<std::string,int>& tag_pos, const std::string& tag)
{
  if (tag_pos.find (tag) == tag_pos.end ())
    return -1;
  return tag_pos[tag];
}

int
SourceStandard::find_tag (std::map<std::string,int>& tag_pos, 
                          const std::string& tag1,
                          const std::string& tag2)
{
  int result = find_tag (tag_pos, tag1);
  return result < 0 ? find_tag (tag_pos, tag2) : result;
}

std::string
SourceStandard::get_entry (LexerData& lex) const
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
SourceStandard::get_entries (LexerData& lex) const
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
SourceStandard::get_date_component (LexerData& lex,
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
SourceStandard::get_time (const std::string& entry)
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
SourceStandard::convert_to_double (LexerData& lex,
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
SourceStandard::load (Treelog& msg)
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
      if (dimension_ == Syntax::Unknown ())
        dimension_ = dim_names[tag_c];
    }

  if (!has_factor && !Units::can_convert (original, dimension_))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert from " << original << " to " << dimension_;
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
        else if (Units::can_convert (original, dimension_, val))
          val = Units::convert (original, dimension_, val);
        else 
          {
            std::ostringstream tmp;
            tmp << "Cannot convert " << val << " from " << original 
                << " to " << dimension_;
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

SourceStandard::SourceStandard (const AttributeList& al)
  : SourceFile (al),
    tag (al.name ("tag")),
    title_ (al.name ("title", tag)),
    original (al.name ("original", Syntax::Unknown ())),
    dimension_ (al.name ("dimension", original)),
    dim_line (al.flag ("dim_line", !al.check ("original"))),
    has_factor (al.check ("factor")),
    factor (al.number ("factor", 1.0))
{ }

SourceStandard::~SourceStandard ()
{ sequence_delete (filter.begin (), filter.end ()); }


static struct SourceStandardSyntax
{
  static Source& make (const AttributeList& al)
  { return *new SourceStandard (al); }

  SourceStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SourceFile::load_syntax (syntax, alist);
    alist.add ("description", 
	       "Read a daisy log, weather or data file.");

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

    Librarian<Source>::add_type ("tag", alist, syntax, &make);
  }
} SourceStandard_syntax;

// source_std.C ends here.
