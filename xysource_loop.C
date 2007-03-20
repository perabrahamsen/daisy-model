// xysource_loop.h -- A traditional for-loop.
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
#include "block.h"
#include "alist.h"
#include "gnuplot_utils.h"
#include "scope_id.h"
#include "number.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"

class XYSourceLoop : public XYSource
{
  // Content.
  const std::string with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  const std::auto_ptr<Number> x_expr;
  const std::auto_ptr<Number> y_expr;
  const std::string title_;
  symbol x_dimension_;
  symbol y_dimension_;

  const double begin;
  const double end;
  const double step;
  const symbol tag;
  ScopeID scope;
  
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
  symbol x_dimension () const 
  { return x_dimension_; }
  symbol y_dimension () const 
  { return y_dimension_; }

  // Read.
public:
 bool load (Treelog& msg);

  // Create.
public:
  explicit XYSourceLoop (Block&);
private:
  XYSourceLoop (const XYSourceLoop&);
  XYSourceLoop& operator= (const XYSourceLoop&);
public:
  ~XYSourceLoop ();
};

bool
XYSourceLoop::load (Treelog& msg)
{
  bool ok = true;
  if (!x_expr->initialize (msg) || !x_expr->check (scope, msg))
    {
      msg.error ("Bad x expression");
      ok = false;
    }
  x_expr->tick (scope, msg);
  x_dimension_ = x_expr->dimension (scope);
  if (!y_expr->initialize (msg) || !y_expr->check (scope, msg))
    {
      msg.error ("Bad y expression");
      ok = false;
    }
  y_expr->tick (scope, msg);
  y_dimension_ = y_expr->dimension (scope);
  if (!ok)
    return false;

  // Read data.
  daisy_assert (xs.size () == ys.size ());
  for (scope.set_number (tag, begin); 
       (step > 0.0) ? (scope.number (tag) < end) : (scope.number (tag) > end); 
       scope.set_number (tag, scope.number (tag) + step))
    {
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

XYSourceLoop::XYSourceLoop (Block& al)
  : XYSource (al),
    with_ (al.name ("with")),
    style_ (al.integer ("style", -1)),
    x_expr (Librarian<Number>::build_item (al, "x")),
    y_expr (Librarian<Number>::build_item (al, "y")),
    title_ (al.name ("title", y_expr->title () + " vs " + x_expr->title ())),
    x_dimension_ ("UNINITIALIZED"),
    y_dimension_ ("UNINITIALIZED"),
    begin (al.number ("begin")),
    end (al.number ("end")),
    step (al.number ("step")),
    tag (al.identifier ("tag")),
    scope (tag, al.identifier ("begin"))
{ }

XYSourceLoop::~XYSourceLoop ()
{ }


static struct XYSourceLoopSyntax
{
  static Model& make (Block& al)
  { return *new XYSourceLoop (al); }

  static bool check_alist (const AttributeList& alist, Treelog& msg)
  {
    bool ok = true;
    if (alist.name ("end") != alist.name ("begin"))
      {
        msg.error ("'begin' and 'end' should have the same dimension.");
        ok = false;
      }
    if (alist.name ("step") != alist.name ("begin"))
      {
        msg.error ("'begin' and 'step' should have the same dimension.");
        ok = false;
      }
    const double step = alist.number ("step");
    const double begin = alist.number ("begin");
    const double end = alist.number ("end");
    
    if (step < 0 && begin <= end)
      msg.warning ("Empty loop");
    if (step > 0 && begin >= end)
      msg.warning ("Empty loop");

    return ok;
  }

  XYSourceLoopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    XYSource::load_syntax (syntax, alist);
    GnuplotUtil::load_style (syntax, alist, "", "\
By default the name of the 'x' and 'y' objects.");
    alist.add ("with", "lines");
    
    syntax.add_check (check_alist);
    alist.add ("description", 
	       "Calculate x and y pairs based on a single variable.\n\
\n\
The variable cover an interval from 'begin' to 'end' in fixed steps\n\
'step'.  The name of the variable is specified by 'tag'.  The x and y\n\
expressions may refer to the variable.");
    syntax.add ("x", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the x value.");
    AttributeList x_alist;
    x_alist.add ("type", "fetch");
    x_alist.add ("name", "x");
    alist.add ("x", x_alist);
    syntax.add ("y", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the y value.");
    syntax.add ("begin", Syntax::User (), Syntax::Const, "\
Start of interval.");
    syntax.add ("end", Syntax::User (), Syntax::Const, "\
End of interval.");
    syntax.add ("step", Syntax::User (), Check::non_zero (), Syntax::Const, "\
Disretization within interval.");
    
    syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of free variable to calculate the 'x' and 'y' expressions from.");
    alist.add ("tag", "x");

    Librarian<XYSource>::add_type ("loop", alist, syntax, &make);
  }
} XYSourceLoop_syntax;

// xysource_loop.C ends here.
