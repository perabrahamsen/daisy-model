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
#include "block_model.h"
#include "librarian.h"
#include "submodeler.h"
#include "treelog.h"
#include "frame_submodel.h"
#include "metalib.h"
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
      frame.declare ("x", Attribute::User (), Attribute::Const, "Operand.");
      frame.declare ("y", Attribute::User (), Attribute::Const, "Value.");
      frame.order ("x", "y");
    }
    
    Point (const BlockSubmodel& al)
      : x_value (al.number ("x")),
	x_dimension (al.name ("x")),
	y_value (al.number ("y")),
	y_dimension (al.name ("y"))
    { }
  };

  // Parameters.
  const std::unique_ptr<Number> operand;
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
    TREELOG_MODEL (msg);
    return operand->initialize (units, scope, msg); 
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    return operand->check_dim (units, scope, domain, msg); 
  }
  static bool check_alist (const Metalib& metalib, 
                           const Frame& al, Treelog& msg) 
  {
#if 1
    return true;
#else
    const Units& units = metalib.units ();
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
	const symbol x_dim = point.x_dimension;

	if (domain != Attribute::Unknown () && x_dim != Attribute::Unknown ())
	  try
	    { x = units.convert (x_dim, domain, x); }
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
	if (domain != Attribute::Unknown () && y_dim != Attribute::Unknown ())
	  try
	    { (void) units.convert (x_dim, domain, x); }
	  catch (const std::string& err)
	    { 
	      msg.error (err);
	      ok = false;
	    }
      }
    return ok;
#endif
  }

  static const PLF build_plf (const BlockModel& al) 
  {
    const Units& units = al.units ();
    const symbol domain (al.name ("domain"));
    const symbol range (al.name ("range"));
    const auto_vector<const Point*> points 
      = map_submodel_const<Point> (al, "points");

    PLF plf;

    for (size_t i = 0; i < points.size (); i++)
      {
	const Point& point = *points[i];

	double x = point.x_value;
	const symbol x_dim = point.x_dimension;
	if (domain != Attribute::Unknown () && x_dim != Attribute::Unknown ())
	  x = units.convert (x_dim, domain, x);
	double y = point.y_value;
	const symbol y_dim = point.y_dimension;
	if (range != Attribute::Unknown () && y_dim != Attribute::Unknown ())
	  y = units.convert (y_dim, range, y);
	
	plf.add (x, y);
      }
    return plf;
  }

  NumberPLF (const BlockModel& al)
    : Number (al),
      operand (Librarian::build_item<Number> (al, "operand")),
      domain (al.name ("domain")),
      range (al.name ("range")),
      plf (build_plf (al))
  { }
};

static struct NumberPLFSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
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
    frame.declare_string ("domain", Attribute::Const, "\
Unit for the operand of the function.");
    frame.set ("domain", Attribute::Unknown ());
    frame.declare_string ("range", Attribute::Const, "\
Unit for the operand of the function.");
    frame.set ("range", Attribute::Unknown ());
    frame.declare_submodule_sequence ("points", Attribute::Const, "\
List of points (x y) defining the piecewise linear function.\n\
The x values must be ordered lowest first.", NumberPLF::Point::load_syntax);

    frame.order ("operand");
  }
} NumberPLF_syntax;

// number_plf.C ends here.
