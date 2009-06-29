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
#include "plf.h"
#include "units.h"
#include "memutils.h"
#include "block.h"
#include "librarian.h"
#include "submodeler.h"
#include "treelog.h"
#include "frame_submodel.h"
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

    static void load_syntax (Frame& frame)
    {
      frame.declare ("x", Value::User (), Value::Const, "Operand.");
      frame.declare ("y", Value::User (), Value::Const, "Value.");
      frame.order ("x", "y");
    }
    
    Point (const FrameSubmodel& al)
      : x_value (al.number ("x")),
	x_dimension (al.name ("x")),
	y_value (al.number ("y")),
	y_dimension (al.name ("y"))
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
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { operand_missing = !operand->tick_value (units, operand_value,
                                            domain, scope, msg); }
  bool missing (const Scope& scope) const 
  { return operand_missing; }
  double value (const Scope& scope) const
  { return plf (operand_value); }
  symbol dimension (const Scope&) const
  { return range; }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    Treelog::Open nest (msg, name);
    return operand->initialize (units, scope, msg); 
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    return operand->check_dim (units, scope, domain, msg); 
  }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg) 
  {
    const symbol domain (al.name ("domain"));
    const symbol range (al.name ("range"));
    const auto_vector<const Point*> points 
      = map_construct_const<Point> (al.submodel_sequence ("points"));

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
#ifdef HAS_METALIB
	const symbol x_dim = point.x_dimension;

	if (domain != Value::Unknown () && x_dim != Value::Unknown ())
	  try
	    { x = units.convert (x_dim, domain, x); }
	  catch (const std::string& err)
	    { 
	      msg.error (err);
	      ok = false;
	    }
#endif // HAS_METALIB
	if (ok && i > 0 && x <= last_x)
	  {
	    std::ostringstream tmp;
	    tmp << x << " <= " << last_x << ", x values should be increasing";
	    msg.error (tmp.str ());
	    ok = false;
	  }
	last_x = x;

#ifdef HAS_METALIB
	const symbol y_dim = point.y_dimension;
	if (domain != Value::Unknown () && y_dim != Value::Unknown ())
	  try
	    { (void) units.convert (x_dim, domain, x); }
	  catch (const std::string& err)
	    { 
	      msg.error (err);
	      ok = false;
	    }
#endif // HAS_METALIB
      }
    return ok;
  }

  static const PLF build_plf (Block& al) 
  {
    const Units& units = al.units ();
    const symbol domain (al.name ("domain"));
    const symbol range (al.name ("range"));
    const auto_vector<const Point*> points 
      = map_construct_const<Point> (al.submodel_sequence ("points"));

    PLF plf;

    for (size_t i = 0; i < points.size (); i++)
      {
	const Point& point = *points[i];

	double x = point.x_value;
	const symbol x_dim = point.x_dimension;
	if (domain != Value::Unknown () && x_dim != Value::Unknown ())
	  x = units.convert (x_dim, domain, x);
	double y = point.y_value;
	const symbol y_dim = point.y_dimension;
	if (range != Value::Unknown () && y_dim != Value::Unknown ())
	  y = units.convert (y_dim, range, y);
	
	plf.add (x, y);
      }
    return plf;
  }

  NumberPLF (Block& al)
    : Number (al),
      operand (Librarian::build_item<Number> (al, "operand")),
      domain (al.name ("domain")),
      range (al.name ("range")),
      plf (build_plf (al))
  { }
};

static struct NumberPLFSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NumberPLF (al); }

  NumberPLFSyntax ()
    : DeclareModel (Number::component, "plf", 
	       "Look up argumen in a piecewise linear function.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (NumberPLF::check_alist);

    frame.declare_object ("operand", Number::component,
                       "Operand for this function.");
    frame.declare_string ("domain", Value::Const, "\
Unit for the operand of the function.");
    frame.set ("domain", Value::Unknown ());
    frame.declare_string ("range", Value::Const, "\
Unit for the operand of the function.");
    frame.set ("range", Value::Unknown ());
    frame.declare_submodule_sequence ("points", Value::Const, "\
List of points (x y) defining the piecewise linear function.\n\
The x values must be ordered lowest first.", NumberPLF::Point::load_syntax);

    frame.order ("operand");
  }
} NumberPLF_syntax;

// number_plf.C ends here.
