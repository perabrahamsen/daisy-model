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

#define BUILD_DLL
#include "source.h"
#include "block.h"
#include "alist.h"
#include "number.h"
#include "scope_sources.h"
#include "gnuplot_utils.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"

struct SourceCombine : public Source
{
  // Content.
  ScopeSources scope;
  const std::auto_ptr<Number> expr;
  const symbol title_;
  symbol dimension_;
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
  { daisy_notreached (); }
  symbol title () const
  { return title_; }
  symbol dimension () const 
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
  if (!expr->initialize (msg) || !expr->check (scope, msg))
    return false;
  expr->tick (scope, msg);

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
    scope (Librarian::build_vector<Source> (al, "source")),
    expr (Librarian::build_item<Number> (al, "expr")),
    title_ (al.identifier ("title", expr->title ())),
    dimension_ ("UNINITIALIZED"),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1))
{ }

static struct SourceCombineSyntax
{
  static Model& make (Block& al)
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
    syntax.add_object ("source", Source::component, 
                       Syntax::State, Syntax::Sequence, "\
List of sources for data.  The style information for the sources is\n\
ignored, but the dates, title and value is used as specified by\n\
'expr' to calculate the combines date and value pairs.");
    syntax.add_object ("expr", Number::component, 
                       Syntax::Const, Syntax::Singleton, "\
Expression for calculating the value for this source for each row.\n\
A row is any date found in any of the member of 'source'.  The\n\
expression may refer to the value of each source by its title.");
    
    Librarian::add_type (Source::component, "combine", alist, syntax, &make);
  }
} SourceCombine_syntax;

// source_combine.C ends here.
