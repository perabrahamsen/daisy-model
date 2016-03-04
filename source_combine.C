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
#include "block_model.h"
#include "number.h"
#include "scope_sources.h"
#include "gnuplot_utils.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

struct SourceCombine : public Source
{
  const Units& units;

  // Content.
  ScopeSources scope;
  const std::unique_ptr<Number> expr;
  const symbol title_;
  symbol dimension_;
  symbol with_;
  const int style_;
  const bool accumulate_;
  std::vector<Time> times;
  std::vector<double> values;

  // Interface.
public:
  symbol with () const
  { return with_; }
  int style () const 
  { return style_; }
  bool accumulate () const
  { return accumulate_; }
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
  explicit SourceCombine (const BlockModel& al);
  ~SourceCombine ()
  { }
};

bool
SourceCombine::load (Treelog& msg)
{
  // Propagate.
  scope.load (msg);

  // Scope
  if (!expr->initialize (units, scope, msg) || !expr->check (units, scope, msg))
    return false;
  expr->tick (units, scope, msg);

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

SourceCombine::SourceCombine (const BlockModel& al)
  : Source (al),
    units (al.units ()),
    scope (Librarian::build_vector<Source> (al, "source")),
    expr (Librarian::build_item<Number> (al, "expr")),
    title_ (al.name ("title", expr->title ())),
    dimension_ ("UNINITIALIZED"),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1)),
    accumulate_ (al.flag ("accumulate"))
{ }

static struct SourceCombineSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SourceCombine (al); }

  SourceCombineSyntax ()
    : DeclareModel (Source::component, "combine", 
	       "Combine data from multiple sources with a single expression.")
  { }
  void load_frame (Frame& frame) const
  { 
    GnuplotUtil::load_style (frame, "\
By default, let the first source decide.", "\
By default the name of the 'expr' object.");
    frame.declare_boolean ("accumulate", Attribute::Const, "\
Accumulate values.");
    frame.set ("accumulate", false);
    frame.declare_object ("source", Source::component, 
                       Attribute::State, Attribute::Variable, "\
List of sources for data.  The style information for the sources is\n\
ignored, but the dates, title and value is used as specified by\n\
'expr' to calculate the combines date and value pairs.");
    frame.declare_object ("expr", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression for calculating the value for this source for each row.\n\
A row is any date found in any of the member of 'source'.  The\n\
expression may refer to the value of each source by its title.");
    
  }
} SourceCombine_syntax;

// source_combine.C ends here.
