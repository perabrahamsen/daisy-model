// source_std.C -- Fetch a tag source for gnuplot interface 
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
public:
  bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit SourceStandard (Block& al);
  ~SourceStandard ();
};

bool
SourceStandard::load (Treelog& msg)
{
  // Read header.
  LexerData lex (filename, msg);
  if (!read_header (lex))
    return false;

  // Tag.
  const int tag_c = find_tag (tag_pos, tag);
  if (tag_c < 0)
    {
      lex.error ("Tag '" + tag + "' not found");
      return false;
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
      tmp << "Cannot convert from [" << original << "] to [" << dimension_ 
          << "]";
      lex.error (tmp.str ());
      return false;
    }

  // Read data.
  Time last_time (9999, 12, 31, 23);
  std::vector<double> vals;

  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      Time time (9999, 1, 1, 0);
      if (!read_entry (lex, entries, time))
        continue;

      // Extract value.
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
          tmp << "Cannot convert " << val << " from [" << original 
              << "] to [" << dimension_ << "]";
          lex.warning (tmp.str ());
          // Treat as missing value.
          continue;
        }
      vals.push_back (val);
      if (time != last_time)
	{
	  last_time = time;
	  add_entry (time, vals);
	}
    }
  if (vals.size () > 0)
    add_entry (last_time, vals);

  // Done.
  return true;
}

SourceStandard::SourceStandard (Block& al)
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
{ }


static struct SourceStandardSyntax
{
  static Source& make (Block& al)
  { return *new SourceStandard (al); }

  SourceStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SourceFile::load_syntax (syntax, alist);
    alist.add ("description", "\
Read a a single column from a Daisy log, weather or data file.");

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
By default Daisy will convert from 'original' to 'dimension'.");

    Librarian<Source>::add_type ("column", alist, syntax, &make);
  }
} SourceStandard_syntax;

// source_std.C ends here.
