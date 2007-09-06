// number_plf.C -- A piecewise linear function.
// 
// Copyright 2004, 2005, 2007 Per Abrahamsen and KVL.
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

#include "number.h"
#include "syntax.h"
#include "alist.h"
#include "plf.h"
#include "units.h"
#include "memutils.h"
#include "block.h"
#include "librarian.h"
#include "submodeler.h"
#include <sstream>
#include <memory>

struct NumberPLF : public Number
{
  // Point.
  struct Point
  {
    const double x_value;
    const symbol x_dimension;
    const double y_value;
    const symbol y_dimension;

    static void load_syntax (Syntax& syntax, AttributeList& alist)
    {
      syntax.add ("x", Syntax::User (), Syntax::Const, "Operand.");
      syntax.add ("y", Syntax::User (), Syntax::Const, "Value.");
      syntax.order ("x", "y");
    }
    
    Point (AttributeList& al)
      : x_value (al.number ("x")),
	x_dimension (al.identifier ("x")),
	y_value (al.number ("y")),
	y_dimension (al.identifier ("y"))
    { }
  };

  // Parameters.
  const std::auto_ptr<Number> operand;
  const symbol domain;
  const symbol range;
  const PLF plf;

  // State.
  double operand_value;
  bool operand_missing;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { operand_missing = !operand->tick_value (operand_value, domain, scope, msg); }
  bool missing (const Scope& scope) const 
  { return operand_missing; }
  double value (const Scope& scope) const
  { return plf (operand_value); }
  symbol dimension (const Scope&) const
  { return range; }

  // Create.
  bool initialize (Treelog& err)
  { 
    Treelog::Open nest (err, name);
    return operand->initialize (err); 
  }
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    return operand->check_dim (scope, domain, err); 
  }
  static bool check_alist (const AttributeList& al, Treelog& msg) 
  {
    const symbol domain (al.identifier ("domain"));
    const symbol range (al.identifier ("range"));
    const auto_vector<const Point*> points 
      = map_construct_const<Point> (al.alist_sequence ("points"));

    if (points.size () < 1)
      {
	msg.error ("Need at least one point for a line");
	return false;
      }

    bool ok = true;
    double last_x = 42.42e42;
    for (size_t i = 0; i < points.size (); i++)
      {
	const Point& point = *points[i];

	double x = point.x_value;
	const symbol x_dim = point.x_dimension;

	if (domain != Syntax::unknown () && x_dim != Syntax::unknown ())
	  try
	    { x = Units::convert (x_dim, domain, x); }
	  catch (const std::string& err)
	    { 
	      msg.error (err);
	      ok = false;
	    }
	if (ok && i > 0 && x <= last_x)
	  {
	    std::ostringstream tmp;
	    tmp << x << " <= " << last_x << ", x values should be increasing";
	    msg.error (tmp.str ());
	    ok = false;
	  }
	last_x = x;

	const symbol y_dim = point.y_dimension;
	if (domain != Syntax::unknown () && y_dim != Syntax::unknown ())
	  try
	    { (void) Units::convert (x_dim, domain, x); }
	  catch (const std::string& err)
	    { 
	      msg.error (err);
	      ok = false;
	    }
      }
    return ok;
  }

  static const PLF build_plf (Block& al) 
  {
    const symbol domain (al.identifier ("domain"));
    const symbol range (al.identifier ("range"));
    const auto_vector<const Point*> points 
      = map_construct_const<Point> (al.alist_sequence ("points"));

    PLF plf;

    for (size_t i = 0; i < points.size (); i++)
      {
	const Point& point = *points[i];

	double x = point.x_value;
	const symbol x_dim = point.x_dimension;
	if (domain != Syntax::unknown () && x_dim != Syntax::unknown ())
	  x = Units::convert (x_dim, domain, x);
	double y = point.y_value;
	const symbol y_dim = point.y_dimension;
	if (range != Syntax::unknown () && y_dim != Syntax::unknown ())
	  y = Units::convert (y_dim, range, y);
	
	plf.add (x, y);
      }
    return plf;
  }

  NumberPLF (Block& al)
    : Number (al),
      operand (Librarian::build_item<Number> (al, "operand")),
      domain (al.identifier ("domain")),
      range (al.identifier ("range")),
      plf (build_plf (al))
  { }
};

static struct NumberPLFSyntax
{
  static Model& make (Block& al)
  { return *new NumberPLF (al); }

  NumberPLFSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (NumberPLF::check_alist);
    alist.add ("description", 
	       "Look up argumen in a piecewise linear function.");

    syntax.add_object ("operand", Number::component,
                       "Operand for this function.");
    syntax.add ("domain", Syntax::String, Syntax::Const, "\
Unit for the operand of the function.");
    alist.add ("domain", Syntax::Unknown ());
    syntax.add ("range", Syntax::String, Syntax::Const, "\
Unit for the operand of the function.");
    alist.add ("range", Syntax::Unknown ());
    syntax.add_submodule_sequence ("points", Syntax::Const, "\
List of points (x y) defining the piecewise linear function.\n\
The x values must be ordered lowest first.", NumberPLF::Point::load_syntax);

    syntax.order ("operand");
    Librarian::add_type (Number::component, "plf", alist, syntax, &make);
  }
} NumberPLF_syntax;

// number_plf.C ends here.
