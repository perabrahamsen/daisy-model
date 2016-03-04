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

#define BUILD_DLL
#include "xysource.h"
#include "lexer_table.h"
#include "scope_table.h"
#include "gnuplot_utils.h"
#include "number.h"
#include "boolean.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"

class XYSourceExpr : public XYSource
{
  // Content.
  LexerTable lex;
  symbol with_;
  const bool explicit_with;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  std::vector<double> xbars;
  std::vector<double> ybars;
  const std::unique_ptr<Number> x_expr;
  const std::unique_ptr<Number> y_expr;
  const std::unique_ptr<Number> xbar_expr;
  const std::unique_ptr<Number> ybar_expr;
  const std::unique_ptr<Boolean> valid;
  const symbol title_;
  symbol x_dimension_;
  symbol y_dimension_;
  
  // Interface.
public:
  symbol title () const
  { return title_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  const std::vector<double>& xbar () const
  { return xbars; }
  const std::vector<double>& ybar () const
  { return ybars; }
  symbol with () const
  { return with_; }
  int style () const 
  { return style_; }
  symbol x_dimension () const 
  { return x_dimension_; }
  symbol y_dimension () const 
  { return y_dimension_; }

  // Read.
private:
  bool read_header (Treelog&);
public:
 bool load (const Units&, Treelog& msg);

  // Create.
public:
  explicit XYSourceExpr (const BlockModel&);
private:
  XYSourceExpr (const XYSourceExpr&);
  XYSourceExpr& operator= (const XYSourceExpr&);
public:
  ~XYSourceExpr ();
};

bool
XYSourceExpr::read_header (Treelog& msg)
{
  // Read header.
  if (!lex.read_header (msg))
    return false;

  // Choose lines or points from type.
  if (with_ != "")
    /* Use it */;
  else if (xbar_expr.get () && ybar_expr.get ())
    with_ = "xyerrorbars";
  else if (xbar_expr.get ())
    with_ = "xerrorbars";
  else if (ybar_expr.get ())
    with_ = "yerrorbars";
  else
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
XYSourceExpr::load (const Units& units, Treelog& msg)
{
  // Lex it.
  if (!read_header (msg))
    return false;

  // Scope
  ScopeTable scope (lex);
  {
    bool ok = true;
    if (!x_expr->initialize (units, scope, msg)
        || !x_expr->check (units, scope, msg))
      {
        lex.error ("Bad x expression");
        ok = false;
      }
    x_expr->tick (units, scope, msg);
    x_dimension_ = x_expr->dimension (scope);
    if (!y_expr->initialize (units, scope, msg)
        || !y_expr->check (units, scope, msg))
      {
        lex.error ("Bad y expression");
        ok = false;
      }
    y_expr->tick (units, scope, msg);
    y_dimension_ = y_expr->dimension (scope);
    if (xbar_expr.get ())
      {
      if (!xbar_expr->initialize (units, scope, msg)
          || !xbar_expr->check (units, scope, msg))
        {
          lex.error ("Bad y expression");
          ok = false;
        }
      xbar_expr->tick (units, scope, msg);
      const symbol xbar_dim = xbar_expr->dimension (scope);
      if (xbar_dim != x_dimension_)
        {
          lex.error ("errorbar mismmatch on x axes: " 
                     + xbar_dim + " != " + x_dimension_);
          ok = false;
        }
      }
    if (ybar_expr.get ())
      {
      if (!ybar_expr->initialize (units, scope, msg)
          || !ybar_expr->check (units, scope, msg))
        {
          lex.error ("Bad y expression");
          ok = false;
        }
      ybar_expr->tick (units, scope, msg);
      const symbol ybar_dim = ybar_expr->dimension (scope);
      if (ybar_dim != y_dimension_)
        {
          lex.error ("errorbar mismmatch on y ayes: " 
                     + ybar_dim + " != " + y_dimension_);
          ok = false;
        }
      }
    if (!valid->initialize (units, scope, msg)
        || !valid->check (units, scope, msg))
      {
        lex.error ("Bad 'valid' expression");
        ok = false;
      }
    valid->tick (units, scope, msg);

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
      if (x_expr->missing (scope) || y_expr->missing (scope)
          || (xbar_expr.get () && xbar_expr->missing (scope))
          || (ybar_expr.get () && ybar_expr->missing (scope))
          )
	continue;

      if (valid->missing (scope))
	continue;
      if (!valid->value (scope))
        continue;
      
      // Store it.
      xs.push_back (x_expr->value (scope));
      ys.push_back (y_expr->value (scope));
      if (xbar_expr.get ())
        xbars.push_back (xbar_expr->value (scope));
      if (ybar_expr.get ())
        ybars.push_back (ybar_expr->value (scope));
    }
  daisy_assert (xs.size () == ys.size ());

  // Done.
  return true;
}

XYSourceExpr::XYSourceExpr (const BlockModel& al)
  : XYSource (al),
    lex (al),
    with_ (al.name ("with", "")),
    explicit_with (al.check ("with")),
    style_ (al.integer ("style", -1)),
    x_expr (Librarian::build_item<Number> (al, "x")),
    y_expr (Librarian::build_item<Number> (al, "y")),
    xbar_expr (al.check ("xbar")
               ? Librarian::build_item<Number> (al, "xbar")
               : NULL),
    ybar_expr (al.check ("ybar")
               ? Librarian::build_item<Number> (al, "ybar")
               : NULL),
    valid (Librarian::build_item<Boolean> (al, "valid")),
    title_ (al.name ("title", y_expr->title () + " vs " + x_expr->title ())),
    x_dimension_ ("UNINITIALIZED"),
    y_dimension_ ("UNINITIALIZED")
{ }

XYSourceExpr::~XYSourceExpr ()
{ }


static struct XYSourceExprSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceExpr (al); }

  XYSourceExprSyntax ()
    : DeclareModel (XYSource::component, "arithmetic", 
	       "Read a daisy log, weather or data file.\n\
Calculate an x and an y value for each time step, based on the value\n\
in the various columns.")
  { }
  void load_frame (Frame& frame) const
  { 
    LexerTable::load_syntax (frame);
    GnuplotUtil::load_style (frame, "\
By default, data from dwf and dlf files will be\n\
drawn with lines, and data from ddf files will be drawn with points.", "\
By default the name of the 'x' and 'y' objects.");
    frame.declare_object ("x", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression for calculating the x value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    frame.declare_object ("y", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression for calculating the y value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    frame.declare_object ("xbar", Number::component, 
                          Attribute::OptionalConst, Attribute::Singleton, "\
Expression for calculating x errorbar for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    frame.declare_object ("ybar", Number::component, 
                          Attribute::OptionalConst, Attribute::Singleton, "\
Expression for calculating y errorbar for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    frame.declare_object ("valid", Boolean::component, 
                          Attribute::Const, Attribute::Singleton, "\
Ignore entries if this boolean expression is false.");
    frame.set ("valid", "true");
  }
} XYSourceExpr_syntax;

// xysource_expr.C ends here.
