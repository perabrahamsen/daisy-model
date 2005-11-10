// lexer_data.C --- Read tabular data from a file.
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

#include "lexer_table.h"
#include "lexer_data.h"
#include "alist.h"
#include "assertion.h"
#include "mathlib.h"
#include "submodeler.h"
#include "memutils.h"
#include "time.h"
#include "vcheck.h"
#include <sstream>

class LexerTable::Filter
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
LexerTable::Filter::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file to filter for.");
  syntax.add ("allowed", Syntax::String, Syntax::Const, Syntax::Sequence, "\
List of allowable values in filter.");
  syntax.add_check ("allowed", VCheck::min_size_1 ());
  syntax.order ("tag", "allowed");
}

LexerTable::Filter::Filter (Block& al)
  : tag (al.name ("tag")),
    allowed (al.name_sequence ("allowed"))
{ }

bool 
LexerTable::good ()
{ return lex->good (); }

bool
LexerTable::read_header (Treelog& msg)
{
  lex = new LexerData (filename, msg);
  // Open errors?
  if (!lex->good ())
    return false;

  // Read first line.
  type_ = lex->get_word ();
  if (type_ == "dwf-0.0")
    field_sep = "";
  else if (type_ == "dlf-0.0")
    field_sep = "\t";
  else if (type_ == "ddf-0.0")
    field_sep = "\t";
  else
    {
      error ("Unknown file type '" + type_ + "'");
      field_sep = "\t";
    }
  lex->skip_line ();
  lex->next_line ();

  // Skip keywords.
  while (lex->good () && lex->peek () != '-')
    {
      lex->skip_line ();
      lex->next_line ();
    }

  // Skip hyphens.
  while (lex->good () && lex->peek () == '-')
    lex->get ();
  lex->skip_space ();

  // Read tags.
  get_entries_raw (tag_names);
  for (int count = 0; count < tag_names.size (); count++)
    {
      const std::string candidate = tag_names[count];
      if (tag_pos.find (candidate) == tag_pos.end ())
        tag_pos[candidate] = count;
      else
	warning ("Duplicate tag: '" + candidate + "'");
    }

  // Time tags.
  year_c = find_tag ("year", "Year");
  month_c = find_tag ("month", "Month");
  mday_c = find_tag ("mday", "Day");
  hour_c = find_tag ("hour", "Hour");
  time_c = find_tag ("time", "Date");

  // Filter tags.
  for (size_t i = 0; i < filter.size (); i++)
    {
      int c = find_tag (filter[i]->tag);
      if (c < 0)
	{
	  error ("Filter tag '" + filter[i]->tag + "' not found");
	  return false;
	}
      fil_col.push_back (c);
    }
  
  // Read dimensions.
  if (dim_line)
    {
      if (!get_entries (dim_names))
        return false;
    }
  else switch (original.size ())
    {
    case 0:
      dim_names.insert (dim_names.end (), tag_names.size (), 
                        Syntax::Unknown ());
      break;
    case 1:
      dim_names.insert (dim_names.end (), tag_names.size (),
                        original[0]);
      break;
    default:
      dim_names = original;
    }

  if (dim_names.size () != tag_names.size ())
    {
      std::ostringstream tmp;
      tmp << "Got " << tag_names.size () << " tags and " << dim_names.size ()
          << " dimensions";
      error (tmp.str ());
      return false;
    }

  return lex->good ();
}  

const std::string&
LexerTable::type () const
{ return type_; }

int
LexerTable::find_tag (const std::string& tag) const
{
  if (tag_pos.find (tag) == tag_pos.end ())
    return -1;
  return tag_pos.find (tag)->second;
}

int
LexerTable::find_tag (const std::string& tag1, const std::string& tag2) const
{
  int result = find_tag (tag1);
  return result < 0 ? find_tag (tag2) : result;
}

const std::string& 
LexerTable::dimension (size_t tag_c) const
{ 
  daisy_assert (tag_c < dim_names.size ());
  return dim_names[tag_c];
}

std::string
LexerTable::get_entry () const
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
  while (lex->good ())
    {
      int c = lex->peek ();
      if (strchr (field_term, c))
	break;
      entry += int2char (lex->get ());
    }
  return entry;
}

void
LexerTable::get_entries_raw (std::vector<std::string>& entries) const
{
  entries.clear ();
  lex->skip ("\n");
  while (lex->good ())
    {
      entries.push_back (get_entry ());

      if (lex->peek () == '\n')
        break;

      if (field_sep == "")
	lex->skip_space ();
      else
	lex->skip(field_sep.c_str ());
    }
}

bool
LexerTable::get_entries (std::vector<std::string>& entries) const
{
  get_entries_raw (entries);

  // Got the right number of entries?
  if (entries.size () != tag_names.size ())
    {
      if (entries.size () == 0 || !lex->good ())
        return false;

      std::ostringstream tmp;
      tmp << "Got " << entries.size () << " entries, expected " 
          << tag_names.size ();
      warning (tmp.str ());
      while (entries.size () < tag_names.size ())
        entries.push_back ("");
    }

  // Filter.
  for (size_t i = 0; i < filter.size (); i++)
    if (!filter[i]->match (entries[fil_col[i]]))
      return false;
  
  return true;
}

int
LexerTable::get_date_component (const std::vector<std::string>& entries, 
				const int column, 
                                const int default_value) const
{
  if (column < 0)
    return default_value;
  daisy_assert (column < entries.size ());
  const char *const str = entries[column].c_str ();
  const char* end_ptr = str;
  const long lval = strtol (str, const_cast<char**> (&end_ptr), 10);
  if (*end_ptr != '\0')
    error (std::string ("Junk at end of number '") + end_ptr + "'");
  const int ival = lval;
  if (ival != lval)
    error ("Number out of range");
  return ival;
}

bool
LexerTable::get_time (const std::string& entry, Time& time)
{
  std::istringstream in (entry);

  int val1;
  char sep1;
  int val2;
  char sep2;
  int val3;
  in >> val1 >> sep1 >> val2 >> sep2 >> val3;

  if (sep1 != sep2)
    return false;

  int hour = 8;
  if (in.good () && !in.eof ())
    {
      char sep3;
      in >> sep3 >> hour;
      if (sep3 != 'T')
        return false;
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
    return false;

  if (!Time::valid (year, month, mday, hour))
    return false;

  time = Time (year, month, mday, hour);
  return true;
}

bool
LexerTable::get_time (const std::vector<std::string>& entries,
                      Time& time) const
{
  // Extract date.
  if (time_c < 0)
    {
      int year = get_date_component (entries, year_c, 1000);
      int month = get_date_component (entries, month_c, 1);
      int mday = get_date_component (entries, mday_c, 1);
      int hour = get_date_component (entries, hour_c, 0);

      if (!Time::valid (year, month, mday, hour))
        {
          std::ostringstream tmp;
          tmp << year << "-" << month << "-" << mday << "T" << hour 
              << ": invalid date";
          warning (tmp.str ());
          return false;
        }
      else
        time = Time (year, month, mday, hour);

      if (time.year () == 9999)
        {
          std::ostringstream tmp;
          tmp << time.year () << "-" << time.month () << "-" << time.mday () 
              << "T" << time.hour () << ": invalid date";
          warning (tmp.str ());
          return false;
        }
    }
  else if (!get_time (entries[time_c], time))
    {
      warning (entries[time_c] + ": invalid time");
      return false;
    }

  // If we survived here, everything is fine.
  return true;
}

bool
LexerTable::is_missing (const std::string& value) const
{ return find (missing.begin (), missing.end (), value) != missing.end (); }

double
LexerTable::convert_to_double (const std::string& value) const
{
  const char *const str = value.c_str ();
  const char* end_ptr = str;
  const double val = strtod (str, const_cast<char**> (&end_ptr));
  if (*end_ptr != '\0')
    error (std::string ("Junk at end of number '") + end_ptr + "'");
  return val;
}

void
LexerTable::warning (const std::string& str) const
{ lex->warning (str); }

void 
LexerTable::error (const std::string& str) const
{ lex->error (str); }

void 
LexerTable::load_syntax (Syntax& syntax, AttributeList& alist)
{
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
				 LexerTable::Filter::load_syntax);
  alist.add ("filter", std::vector<AttributeList*> ());
  syntax.add ("original", Syntax::String, Syntax::OptionalConst, 
              Syntax::Sequence, "\
List of dimensions of the data in the data file.\n\
\n\
If the list has only one element, that element is used as the\n\
dimension for all columns in the file.  Otherwise, the list must have\n\
one element for each column.\n\
\n\
By default Daisy will use the names specified in data file.");
  syntax.add ("dim_line", Syntax::Boolean, Syntax::OptionalConst, "\
If true, assume the line after the tags contain dimensions.\n\
By default this will be true iff 'original' is not specified.");
}

LexerTable::LexerTable (Block& al)
  : filename (al.name ("file")),
    lex (NULL),
    field_sep ("UNINITIALIZED"),
    type_ ("UNINITIALIZED"),
    missing (al.name_sequence ("missing")),
    filter (map_submodel_const<Filter> (al, "filter")),
    year_c (-42),
    month_c (-42),
    mday_c (-42),
    hour_c (-42),
    time_c (-42),
    original (al.check ("original")
	      ? al.name_sequence ("original")
	      : std::vector<std::string> ()),
    dim_line (al.flag ("dim_line", !al.check ("original")))
{ }

LexerTable::~LexerTable ()
{ 
  delete lex; 
  sequence_delete (filter.begin (), filter.end ());
}
