// source_combine.C -- Combine data sources for gnuplot interface 
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
#include "number.h"
#include "scope_sources.h"
#include "gnuplot_utils.h"
#include "vcheck.h"

struct SourceCombine : public Source
{
  // Content.
  ScopeSources scope;
  const std::auto_ptr<Number> expr;
  const std::string title_;
  std::string dimension_;
  std::string with_;
  const int style_;
  std::vector<Time> times;
  std::vector<double> values;

  // Interface.
public:
  const std::string& with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::vector<Time>& time () const
  { return times; }
  const std::vector<double>& value () const
  { return values; }
  const std::vector<double>& ebar () const
  { daisy_assert (false); }
  const std::string& title () const
  { return title_; }
  const std::string& dimension () const 
  { return dimension_; }

  // Read. 
public:
 bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit SourceCombine (Block& al);
  ~SourceCombine ()
  { }
};

bool
SourceCombine::load (Treelog& msg)
{
  // Propagate.
  scope.load (msg);

  // Scope
  if (!expr->check (scope, msg))
    return false;

  // Extract.
  dimension_ = expr->dimension (scope);
  if (with_ == "")
    with_ = scope.with ();

  if (with_ == "errorbars")
    {
      msg.warning ("Cannot combine data sources with errorbars");
      with_ = "points";
    }

  // Read data.
  for (scope.first (); !scope.done (); scope.next ())
    {
      if (!expr->missing (scope))
        {
          times.push_back (scope.now);
          values.push_back (expr->value (scope));
        }
    }
  daisy_assert (times.size () == values.size ());

  // Done.
  return true;
}

SourceCombine::SourceCombine (Block& al)
  : Source (al),
    scope (Librarian<Source>::build_vector (al, "source")),
    expr (Librarian<Number>::build_item (al, "expr")),
    title_ (al.name ("title", expr->title ())),
    dimension_ ("UNINITIALIZED"),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1))
{ }

static struct SourceCombineSyntax
{
  static Source& make (Block& al)
  { return *new SourceCombine (al); }

  SourceCombineSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Source::load_syntax (syntax, alist);
    GnuplotUtil::load_style (syntax, alist, "\
By default, let the first source decide.", "\
By default the name of the 'expr' object.");
    alist.add ("description", 
	       "Combine data from multiple sources with a single expression.");
    syntax.add ("source", Librarian<Source>::library (), 
		Syntax::State, Syntax::Sequence, "\
List of sources for data.  The style information for the sources is\n\
ignored, but the dates, title and value is used as specified by\n\
'expr' to calculate the combines date and value pairs.");
    syntax.add ("expr", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the value for this source for each row.\n\
A row is any date found in any of the member of 'source'.  The\n\
expression may refer to the value of each source by its title.");
    
    Librarian<Source>::add_type ("combine", alist, syntax, &make);
  }
} SourceCombine_syntax;

// source_combine.C ends here.
