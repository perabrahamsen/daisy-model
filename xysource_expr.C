// xysource_expr.h -- Table source for gnuplot interface 
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
#include "lexer_table.h"
#include "scope_table.h"
#include "number.h"

class XYSourceExpr : public XYSource
{
  // Content.
  LexerTable lex;
  std::string with_;
  const bool explicit_with;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  const std::auto_ptr<Number> x_expr;
  const std::auto_ptr<Number> y_expr;
  const std::string title_;
  std::string x_dimension_;
  std::string y_dimension_;
  
  // Interface.
public:
  const std::string& title () const
  { return title_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  const std::string& with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::string& x_dimension () const 
  { return x_dimension_; }
  const std::string& y_dimension () const 
  { return y_dimension_; }

  // Read.
private:
  bool read_header (Treelog&);
public:
 bool load (Treelog& msg);

  // Create.
public:
  explicit XYSourceExpr (Block&);
private:
  XYSourceExpr (const XYSourceExpr&);
  XYSourceExpr& operator= (const XYSourceExpr&);
  ~XYSourceExpr ();
};

bool
XYSourceExpr::read_header (Treelog& msg)
{
  // Read header.
  if (!lex.read_header (msg))
    return false;

  // Choose lines or points from type.
  if (with_ == "")
    {
      const std::string type = lex.type ();
      if (type == "dwf-0.0")
	with_ = "lines";
      else if (type == "dlf-0.0")
	with_ = "lines";
      else if (type == "ddf-0.0")
	with_ = "points";
    }
  
  return lex.good ();
}

bool
XYSourceExpr::load (Treelog& msg)
{
  // Lex it.
  if (!read_header (msg))
    return false;

  // Scope
  ScopeTable scope (lex);
  {
    bool ok = true;
    if (!x_expr->check (scope, msg))
      {
        lex.error ("Bad x expression");
        ok = false;
      }
    x_dimension_ = x_expr->dimension (scope);
    if (!y_expr->check (scope, msg))
      {
        lex.error ("Bad y expression");
        ok = false;
      }
    y_dimension_ = y_expr->dimension (scope);
    if (!ok)
      return false;
  }

  // Read data.
  daisy_assert (xs.size () == ys.size ());
  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      if (!lex.get_entries (entries))
        continue;

      // Set it.
      scope.set (entries);
      
      // Missing value.
      if (x_expr->missing (scope) || y_expr->missing (scope))
	continue;
      
      // Store it.
      xs.push_back (x_expr->value (scope));
      ys.push_back (y_expr->value (scope));
    }
  daisy_assert (xs.size () == ys.size ());

  // Done.
  return true;
}

XYSourceExpr::XYSourceExpr (Block& al)
  : XYSource (al),
    lex (al),
    with_ (al.name ("with", "")),
    explicit_with (al.check ("with")),
    style_ (al.integer ("style", -1)),
    x_expr (Librarian<Number>::build_item (al, "x")),
    y_expr (Librarian<Number>::build_item (al, "y")),
    title_ (al.name ("title", y_expr->title () + " vs " + x_expr->title ())),
    x_dimension_ ("UNINITIALIZED"),
    y_dimension_ ("UNINITIALIZED")
{ }

XYSourceExpr::~XYSourceExpr ()
{ }


static struct XYSourceExprSyntax
{
  static XYSource& make (Block& al)
  { return *new XYSourceExpr (al); }

  XYSourceExprSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    XYSource::load_syntax (syntax, alist);
    alist.add ("description", 
	       "Read a daisy log, weather or data file.\n\
Calculate an x and an y value for each time step, based on the value\n\
in the various columns.");
    LexerTable::load_syntax (syntax, alist);

    syntax.add ("with", Syntax::String, Syntax::OptionalConst, "\
Specify 'points' to plot each point individually, or 'lines' to draw\n\
lines between them.  By default, data from dwf and dlf files will be\n\
drawn with lines, and data from ddf files will be drawn with points.");
    static VCheck::Enum with ("lines", "points");
    syntax.add_check ("with", with);
    syntax.add ("style", Syntax::Integer, Syntax::OptionalConst, "\
Style to use for this dataset.  By default, gnuplot will use style 1\n\
for the first source to plot with lines, style 2 for the second, and\n\
so forth until it runs out of styles and has to start over.  Points\n\
work similar, but with its own style counter.  For color plots, points\n\
and lines with the same style number also have the same color.");
    syntax.add ("x", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the x value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    syntax.add ("y", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the x value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    syntax.add ("title", Syntax::String, Syntax::OptionalConst, "\
Name of data legend in plot, by default the name of the 'x' and 'y' objects.");

    Librarian<Source>::add_type ("arithmetic", alist, syntax, &make);
  }
} XYSourceExpr_syntax;

// xysource_expr.C ends here.
