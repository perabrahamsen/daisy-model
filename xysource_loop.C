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

#define BUILD_DLL
#include "xysource.h"
#include "block_model.h"
#include "gnuplot_utils.h"
#include "scope_id.h"
#include "number.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

class XYSourceLoop : public XYSource
{
  // Content.
  const symbol with_;
  const int style_;
  std::vector<double> xs;
  std::vector<double> ys;
  const std::unique_ptr<Number> x_expr;
  const std::unique_ptr<Number> y_expr;
  const symbol title_;
  symbol x_dimension_;
  symbol y_dimension_;

  const double begin;
  const double end;
  const double step;
  const symbol tag;
  ScopeID scope;
  
  // Interface.
public:
  symbol title () const
  { return title_; }
  const std::vector<double>& x () const
  { return xs; }
  const std::vector<double>& y () const
  { return ys; }
  symbol with () const
  { return with_; }
  int style () const 
  { return style_; }
  symbol x_dimension () const 
  { return x_dimension_; }
  symbol y_dimension () const 
  { return y_dimension_; }

  // Read.
public:
  bool load (const Units&, Treelog& msg);

  // Create.
public:
  explicit XYSourceLoop (const BlockModel&);
private:
  XYSourceLoop (const XYSourceLoop&);
  XYSourceLoop& operator= (const XYSourceLoop&);
public:
  ~XYSourceLoop ();
};

bool
XYSourceLoop::load (const Units& units, Treelog& msg)
{
  scope.set (tag, begin);

  bool ok = true;
  if (!x_expr->initialize (units, scope, msg)
      || !x_expr->check (units, scope, msg))
    {
      msg.error ("Bad x expression");
      ok = false;
    }
  x_expr->tick (units, scope, msg);
  x_dimension_ = x_expr->dimension (scope);
  if (!y_expr->initialize (units, scope, msg)
      || !y_expr->check (units, scope, msg))
    {
      msg.error ("Bad y expression");
      ok = false;
    }
  y_expr->tick (units, scope, msg);
  y_dimension_ = y_expr->dimension (scope);
  if (!ok)
    return false;

  // Read data.
  daisy_assert (xs.size () == ys.size ());
  for (; 
       (step > 0.0) ? (scope.number (tag) <= end) : (scope.number (tag) >= end); 
       scope.set (tag, scope.number (tag) + step))
    {
      x_expr->tick (units, scope, msg);
      y_expr->tick (units, scope, msg);
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

XYSourceLoop::XYSourceLoop (const BlockModel& al)
  : XYSource (al),
    with_ (al.name ("with")),
    style_ (al.integer ("style", -1)),
    x_expr (Librarian::build_item<Number> (al, "x")),
    y_expr (Librarian::build_item<Number> (al, "y")),
    title_ (al.name ("title", y_expr->title () + " vs " + x_expr->title ())),
    x_dimension_ ("UNINITIALIZED"),
    y_dimension_ ("UNINITIALIZED"),
    begin (al.number ("begin")),
    end (al.number ("end")),
    step (al.number ("step")),
    tag (al.name ("tag")),
    scope (tag, al.name ("begin"))
{ }

XYSourceLoop::~XYSourceLoop ()
{ }


static struct XYSourceLoopSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceLoop (al); }

  static bool check_alist (const Metalib&, const Frame& alist, Treelog& msg)
  {
    bool ok = true;
    if (alist.name ("end") != alist.name ("begin"))
      {
        msg.error ("'begin' and 'end' should have the same dimension");
        ok = false;
      }
    if (alist.name ("step") != alist.name ("begin"))
      {
        msg.error ("'begin' and 'step' should have the same dimension");
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
    : DeclareModel (XYSource::component, "loop", 
	       "Calculate x and y pairs based on a single variable.\n\
\n\
The variable cover an interval from 'begin' to 'end' in fixed steps\n\
'step'.  The name of the variable is specified by 'tag'.  The x and y\n\
expressions may refer to the variable.")
  { }
  void load_frame (Frame& frame) const
  { 
    GnuplotUtil::load_style (frame, "", "\
By default the name of the 'x' and 'y' objects.");
    frame.set ("with", "lines");
    
    frame.add_check (check_alist);
    frame.declare_object ("x", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression for calculating the x value.");
    frame.set ("x", "x");
    frame.declare_object ("y", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression for calculating the y value.");
    frame.declare ("begin", Attribute::User (), Attribute::Const, "\
Start of interval.");
    frame.declare ("end", Attribute::User (), Attribute::Const, "\
End of interval.");
    frame.declare ("step", Attribute::User (), Check::non_zero (),
                   Attribute::Const, "\
Disretization within interval.");
    frame.declare_string ("tag", Attribute::Const, "\
Name of free variable to calculate the 'x' and 'y' expressions from.");
    frame.set ("tag", "x");

  }
} XYSourceLoop_syntax;

// xysource_loop.C ends here.
