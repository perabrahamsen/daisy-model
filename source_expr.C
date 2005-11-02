// source_expr.C -- Data source for gnuplot interface 
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
#include "number.h"
#include "scope.h"
#include "units.h"
#include "librarian.h"
#include "lexer_data.h"
#include <sstream>

// ScopeScource
class ScopeSource : public Scope
{
  // Content.
private:
  const std::vector<std::string> missing;
  const std::map<std::string,int> tag_pos;
  const std::vector<std::string> dimensions;
  std::vector<std::string> values;
  
  // Interface.
public:
  bool has_number (const std::string& tag) const
  {
    const int tag_c = find_tag (tag_pos, tag);
    if (tag_c < 0)
      return false;
    if (values.size () == 0)
      // Kludge: Uninitialized, simply checking if tags are there...
      return true;
    const std::string& value = values[tag_c];
    return find (missing.begin (), missing.end (), value) == missing.end ();
  }
  double number (const std::string& tag) const
  {
    daisy_assert (tag_pos.find (tag) != tag_pos.end ());
    daisy_assert (values.size () > find_tag (tag_pos, tag));
    daisy_assert (find_tag (tag_pos, tag) >= 0);
    std::istringstream in (values[find_tag (tag_pos, tag)]);
    double value;
    in >> value;
    return value;
  }    
  const std::string& dimension (const std::string& tag) const
  {
    daisy_assert (tag_pos.find (tag) != tag_pos.end ());
    daisy_assert (dimensions.size () > find_tag (tag_pos, tag));
    daisy_assert (find_tag (tag_pos, tag) >= 0);
    return dimensions[find_tag (tag_pos, tag)];
  }

  // Use.
public:
  void set (const std::vector<std::string>& entries)
  { 
    daisy_assert (entries.size () == dimensions.size ());
    values = entries; 
  }

  // Create and Destroy.
public:
  ScopeSource (const std::vector<std::string>& miss,
	       const std::map<std::string,int>& tags,
	       const std::vector<std::string>& dims)
    : missing (miss),
      tag_pos (tags),
      dimensions (dims)
  { }
};

struct SourceExpr : public SourceFile
{
  // Content.
  const std::auto_ptr<Number> expr;
  const std::string title_;
  const std::vector<std::string> original;
  std::string dimension_;
  const bool dim_line;

  // Interface.
public:
  const std::string& title () const
  { return title_; }
  const std::string& dimension () const 
  { return dimension_; }

  // Read. 
public:
 bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit SourceExpr (Block& al);
  ~SourceExpr ();
};

bool
SourceExpr::load (Treelog& msg)
{
  // Lex it.
  LexerData lex (filename, msg);
  if (!read_header (lex))
    return false;

  // Read dimensions.
  std::vector<std::string> dim_names;
  if (dim_line)
    dim_names = get_entries (lex);
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
      lex.error (tmp.str ());
      return false;
    }

  // Scope
  ScopeSource scope (missing, tag_pos, dim_names);
  if (!expr->check (scope, msg))
    {
      lex.error ("Bad expression");
      return false;
    }
  dimension_ = expr->dimension (scope);

  // Read data.
  while (lex.good ())
    {
      // Read entries.
      Time time (9999, 1, 1, 0);
      std::vector<std::string> entries;
      if (!read_entry (lex, entries, time))
        continue;

      // Set it.
      scope.set (entries);
      
      // Missing value.
      if (expr->missing (scope))
	continue;
      
      // Store it.
      times.push_back (time);
      values.push_back (expr->value (scope));
    }

  // Done.
  return true;
}

SourceExpr::SourceExpr (Block& al)
  : SourceFile (al),
    expr (Librarian<Number>::build_item (al, "expr")),
    title_ (al.name ("title", expr->title ())),
    original (al.check ("original")
	      ? al.name_sequence ("original")
	      : std::vector<std::string> ()),
    dimension_ ("UNINITIALIZED"),
    dim_line (al.flag ("dim_line", !al.check ("original")))
{ }

SourceExpr::~SourceExpr ()
{ }


static struct SourceExprSyntax
{
  static Source& make (Block& al)
  { return *new SourceExpr (al); }

  SourceExprSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SourceFile::load_syntax (syntax, alist);
    alist.add ("description", 
	       "Read a daisy log, weather or data file.\n\
Calculate a single value for each time step, based on the value\n\
in the various columns.");
    syntax.add ("expr", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    syntax.add ("title", Syntax::String, Syntax::OptionalConst, "\
Name of data legend in plot, by default the name of the 'expr' object.");
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

    Librarian<Source>::add_type ("arithmetic", alist, syntax, &make);
  }
} SourceExpr_syntax;

// source_std.C ends here.
