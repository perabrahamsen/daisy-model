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

#define BUILD_DLL
#include "source_file.h"
#include "units.h"
#include "lexer_table.h"
#include "alist.h"
#include "librarian.h"
#include <sstream>

struct SourceStandard : public SourceFile
{
  // Content.
  const symbol tag;
  const symbol title_;
  symbol dimension_;
  const bool has_factor;
  const double factor;

  // Interface.
  symbol title () const
  { return title_; }
  symbol dimension () const 
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
  if (!read_header (msg))
    return false;

  // Tag.
  const int tag_c = lex.find_tag (tag);
  if (tag_c < 0)
    {
      lex.error ("Tag '" + tag + "' not found");
      return false;
    }

  // Read dimensions.
  const symbol original (lex.dimension (tag_c));
  if (original != Syntax::unknown () && dimension_ == Syntax::unknown ())
    dimension_ = original;
  else if (!has_factor && !Units::can_convert (original, dimension_))
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
      if (!read_entry (entries, time))
        continue;

      // Extract value.
      const std::string value = entries[tag_c];

      // Skip missing values.
      if (lex.is_missing (value))
        continue;
        
      // Convert it.
      double val = lex.convert_to_double (value);
      if (has_factor)
        val *= factor;
      else if (Units::can_convert (original, dimension_, val))
        val = Units::convert (original, dimension_, val);
      else 
        {
          static bool has_warned = false;
          if (!has_warned)
            {
              std::ostringstream tmp;
              tmp << "Cannot convert " << val << " from [" << original 
                  << "] to [" << dimension_ << "]";
              lex.debug (tmp.str ());
              has_warned = true;
            }
          // Treat as missing value.
          continue;
        }

      // Store it.
      if (time != last_time)
	{
          if (vals.size () > 0)
            add_entry (last_time, vals);
	  last_time = time;
	}
      vals.push_back (val);
    }
  if (vals.size () > 0)
    add_entry (last_time, vals);

  // Done.
  return true;
}

SourceStandard::SourceStandard (Block& al)
  : SourceFile (al),
    tag (al.identifier ("tag")),
    title_ (al.identifier ("title", tag)),
    dimension_ (al.identifier ("dimension", Syntax::unknown ())),
    has_factor (al.check ("factor")),
    factor (al.number ("factor", 1.0))
{ }

SourceStandard::~SourceStandard ()
{ }


static struct SourceStandardSyntax
{
  static Model& make (Block& al)
  { return *new SourceStandard (al); }

  SourceStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SourceFile::load_style (syntax, alist, "\
By default the same as 'tag'.");
    alist.add ("description", "\
Read a a single column from a Daisy log, weather or data file.");

    syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file where data is found.");
    syntax.add ("dimension", Syntax::String, Syntax::OptionalConst, "\
Dimension of data to plot.\n\
By default this is the same as 'original'.\n\
If 'factor' is not specified, Daisy will attempt to convert the data.");
    syntax.add ("factor", Syntax::Unknown (), Syntax::OptionalConst, "\
Multiply all data by this number.\n\
By default Daisy will convert from 'original' to 'dimension'.");

    Librarian::add_type (Source::component, "column", alist, syntax, &make);
  }
} SourceStandard_syntax;

// source_std.C ends here.
