// xysource_xycombine.C -- Combine data sources for gnuplot interface 
// 
// Copyright 2005 Per Abrahamsen and KVL.
// Copyright 2010 KU
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
#include "xysource.h"
#include "block_model.h"
#include "gnuplot_utils.h"
#include "number.h"
#include "scope_xysources.h"
#include "xysource.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

struct XYSourceXYCombine : public XYSource
{
  // Content.
  ScopeXYSources scope;
  const std::unique_ptr<Number> expr;
  const symbol title_;
  symbol x_dimension_;
  symbol y_dimension_;
  symbol with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;

  // Interface.
public:
  symbol with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  symbol title () const
  { return title_; }
  symbol x_dimension () const 
  { return x_dimension_; }
  symbol y_dimension () const 
  { return y_dimension_; }

  // Read. 
public:
  bool load (const Units&, Treelog& msg);
  void limit (double& xmin, double& xmax, double& ymin, double& ymax) const;

  // Create and Destroy.
public:
  explicit XYSourceXYCombine (const BlockModel& al);
  ~XYSourceXYCombine ()
  { }
};

bool
XYSourceXYCombine::load (const Units& units, Treelog& msg)
{
  // Propagate.
  scope.load (units, msg);

  // Scope
  {
    bool ok = true;
    if (!expr->initialize (units, scope, msg) 
        || !expr->check (units, scope, msg))
      ok = false;
    expr->tick (units, scope, msg);
    if (!ok)
      return false;
  }

  // Extract.
  if (scope.range_is_x ())
    {
      x_dimension_ = scope.range_dimension ();
      y_dimension_ = expr->dimension (scope);
    }
  else
    {
      y_dimension_ = scope.range_dimension ();
      x_dimension_ = expr->dimension (scope);
    }
  if (with_ == Attribute::Unknown ())
    with_ = scope.with ();
  if (with_ == "errorbars")
    with_ = "points";

  // Read data.
  for (scope.first (); !scope.done (); scope.next ())
    {
      if (expr->missing (scope))
        {
          std::ostringstream tmp;
          tmp << scope.current () << " missing";
          msg.error (tmp.str ());
          continue;
        }

      if (scope.range_is_x ())
        {
          xs.push_back (scope.current ());
          ys.push_back (expr->value (scope));
        }
      else
        {
          ys.push_back (scope.current ());
          xs.push_back (expr->value (scope));
        }
    }
  daisy_assert (xs.size () == ys.size ());

  // Done.
  return true;
}

void 
XYSourceXYCombine::limit (double& xmin, double& xmax,
                          double& ymin, double& ymax) const
{
  XYSource::limit (xmin, xmax, ymin, ymax);
  scope.limit_range (xmin, xmax, ymin, ymax);
}


XYSourceXYCombine::XYSourceXYCombine (const BlockModel& al)
  : XYSource (al),
    scope (Librarian::build_vector<XYSource> (al, "source")),
    expr (Librarian::build_item<Number> (al, "expr")),
    title_ (al.name ("title", expr->title ())),
    x_dimension_ (Attribute::Unknown ()),
    y_dimension_ (Attribute::Unknown ()),
    with_ (al.name ("with", Attribute::Unknown ())),
    style_ (al.integer ("style", -1))
{ }

static struct XYSourceXYCombineSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceXYCombine (al); }
  
  XYSourceXYCombineSyntax ()
    : DeclareModel (XYSource::component, "xycombine", "\
Combine data from multiple time series with a single expression.\n\
Data from times series are matched by date.")
  { }
  void load_frame (Frame& frame) const
  { 
    GnuplotUtil::load_style (frame, "\
By default, let the first source decide.", "\
By default use the expression.");
    frame.declare_object ("source", XYSource::component, 
                          Attribute::Const, Attribute::Variable, "\
List of sources for data.\n\
The style information for the sources is ignored, but the title\n\
and value is used as specified by 'expr' to calculate the combined\n\
x and y pairs.\n\
Either the x or y for all sourcses must be identical.");
    frame.declare_object ("expr", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression for calculating the value for this source. The\n\
expression may refer to the value of each source by its title.");
  }
} XYSourceXYCombine_syntax;

// xysource_xycombine.C ends here.
