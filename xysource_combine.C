// xysource_combine.C -- Combine data sources for gnuplot interface 
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

#include "xysource.h"
#include "gnuplot_utils.h"
#include "number.h"
#include "scope_sources.h"

struct XYSourceCombine : public XYSource
{
  // Content.
  ScopeSources scope;
  const std::auto_ptr<Number> x_expr;
  const std::auto_ptr<Number> y_expr;
  const std::string title_;
  std::string x_dimension_;
  std::string y_dimension_;
  std::string with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;

  // Interface.
public:
  const std::string& with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  const std::string& title () const
  { return title_; }
  const std::string& x_dimension () const 
  { return x_dimension_; }
  const std::string& y_dimension () const 
  { return y_dimension_; }

  // Read. 
public:
 bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit XYSourceCombine (Block& al);
  ~XYSourceCombine ()
  { }
};

bool
XYSourceCombine::load (Treelog& msg)
{
  // Propagate.
  scope.load (msg);

  // Scope
  {
    bool ok = true;
    if (!x_expr->check (scope, msg))
      ok = false;
    if (!y_expr->check (scope, msg))
      ok = false;
    if (!ok)
      return false;
  }

  // Extract.
  x_dimension_ = x_expr->dimension (scope);
  y_dimension_ = y_expr->dimension (scope);
  if (with_ == "")
    with_ = scope.with ();
  if (with_ == "errorbars")
    with_ = "points";

  // Read data.
  for (scope.first (); !scope.done (); scope.next ())
    {
      if (!x_expr->missing (scope) && !y_expr->missing (scope))
        {
          xs.push_back (x_expr->value (scope));
          ys.push_back (y_expr->value (scope));
        }
    }
  daisy_assert (xs.size () == ys.size ());

  // Done.
  return true;
}

XYSourceCombine::XYSourceCombine (Block& al)
  : XYSource (al),
    scope (Librarian<Source>::build_vector (al, "source")),
    x_expr (Librarian<Number>::build_item (al, "x")),
    y_expr (Librarian<Number>::build_item (al, "y")),
    title_ (al.name ("title", y_expr->title () + " vs " + x_expr->title ())),
    x_dimension_ ("UNINITIALIZED"),
    y_dimension_ ("UNINITIALIZED"),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1))
{ }

static struct XYSourceCombineSyntax
{
  static XYSource& make (Block& al)
  { return *new XYSourceCombine (al); }

  XYSourceCombineSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    XYSource::load_syntax (syntax, alist);
    GnuplotUtil::load_style (syntax, alist, "\
By default, let the first source decide.", "\
By default a combination of the x and y objects.");
    alist.add ("description", "\
Combine data from multiple time series with a single expression.\n\
Data from times series are matched by date.");
    syntax.add ("source", Librarian<Source>::library (), 
		Syntax::State, Syntax::Sequence, "\
List of sources for data.\n\
The style information for the sources is ignored, but the dates, title\n\
and value is used as specified by 'expr' to calculate the combined\n\
date and value pairs.");
    syntax.add ("x", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the x value for this source for each row.\n\
A row is any date found in any of the member of 'source'.  The\n\
expression may refer to the value of each source by its title.");
    syntax.add ("y", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the y value for this source for each row.\n\
A row is any date found in any of the member of 'source'.  The\n\
expression may refer to the value of each source by its title.");
    Librarian<XYSource>::add_type ("combine", alist, syntax, &make);
  }
} XYSourceCombine_syntax;

// xysource_combine.C ends here.
