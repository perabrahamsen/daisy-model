// program_optimize.C -- Optimize parameters.
// 
// Copyright 2004 Per Abrahamsen and KVL.
// Copyright 2011 KU.
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

#include "program.h"
#include "block_top.h"
#include "block_model.h"
#include "block_submodel.h"
#include "treelog_child.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"
#include "scopesel.h"
#include "number.h"
#include "boolean.h"
#include "metalib.h"
#include "mathlib.h"
#include "iterative.h"
#include "check.h"
#include "vcheck.h"
#include <vector>
#include <sstream>
#include <limits>

struct ProgramOptimize : public Program
{
  // Types.
  struct MyPoint : public Iterative::Point
  {
    static void load_syntax (Frame& frame)
    {
      frame.declare ("value", Attribute::Unknown (), Attribute::Const, 
                     Attribute::Variable, "\
Value of each parameter.");
      frame.order ("value");
    }
    MyPoint (const BlockSubmodel& al)
      : Iterative::Point (al.number_sequence ("value"))
    { }
  };

  struct LimScope : public Scope
  {
    const FrameModel& frame;
    const std::vector<symbol>& parameter;
    Iterative::Point point;

    void entries (std::set<symbol>& all) const
    {
      for (size_t i = 0; i < parameter.size (); i++)
        all.insert (parameter[i]);
    }
    Attribute::type lookup (const symbol key) const
    { 
      if (std::find (parameter.begin (), parameter.end (), key) 
          == parameter.end ())
        return Attribute::Error;
      else
        return Attribute::Number;
    }
    symbol dimension (const symbol key) const
    { return frame.dimension (key); }
    symbol description (const symbol key) const
    { return frame.description (key); }
    bool check (const symbol key) const
    { return lookup (key) != Attribute::Error; }
    double number (const symbol key) const
    {
      for (size_t i = 0; i < parameter.size (); i++)
        if (key == parameter[i])
          return point[i];
      daisy_notreached ();
    }
    
    LimScope (const FrameModel& f, const std::vector<symbol>& par)
      : frame (f),
        parameter (par)
    { }
  };

  // Content.
  const Metalib& metalib;
  const Units& units;
  const std::vector<symbol> parameter;
  const std::unique_ptr<Boolean> limit;
  boost::shared_ptr<const FrameModel> original;
  const std::unique_ptr<Scopesel> scopesel;
  const std::unique_ptr<Number> expr;
  Iterative::Simplex simplex;
  LimScope lim_scope;
  const double epsilon;
  const size_t min_iter;
  const size_t max_iter;

  // Use.
  bool find_value (double& value, Treelog& original_msg, 
                   const Iterative::Point& point)
  {
    TreelogSilent msg (original_msg);
    
    // Test limit.
    lim_scope.point = point;
    limit->tick (units, lim_scope, msg);
    if (limit->missing (lim_scope))
      return false;
    if (!limit->value (lim_scope))
      {
        value = std::numeric_limits<double>::max ();
        return true;
      }
    
    // Run simulation.
    FrameModel modified (*original, Frame::parent_link);
    daisy_assert (point.size () == parameter.size ());
    for (size_t i = 0; i < point.size (); i++)
      modified.set (parameter[i], point[i]);

    const std::unique_ptr<Program> program
      (Librarian::build_frame<Program> (metalib, msg, modified, "run"));

    const Scope *const scope = scopesel->lookup (program->scopes (), msg);
    if (!scope)
      {
        msg.error ("Scope not found");
        return false;
      }

    if (!ui_running ())
      return false;

    BlockTop block (metalib, msg, modified);
    program->initialize (block);
    if (!block.ok ())
      return false;
    if (!program->check (msg))
      return false;

    propagate_ui (program.get ());

    if (!program->run (msg))
      return false;

    // Find value.
    if (!expr->initialize (units, *scope, msg) 
        // BUG: Called multiple time. Ok?
        || !expr->check (units, *scope, msg))
      {
        msg.error ("Bad expression");
        return false;
      }
    expr->tick (units, *scope, msg);
    // const symbol dimension = expr->dimension (*scope);

    if (expr->missing (*scope))
      return false;

    value = expr->value (*scope);
    return true;
  }

  struct MyFun : public Iterative::PointFunction
  {
    ProgramOptimize& program;
    Treelog& msg;

    double value (const Iterative::Point& point) const
    {
      double result = NAN;
      try 
        {
          if (program.find_value (result, msg, point))
            return result;
        }
      catch (...)
        { }
      return std::numeric_limits<double>::max ();
    }

    MyFun (ProgramOptimize& p, Treelog& m)
      : program (p),
        msg (m)
    { }
  };

    
  bool run (Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    msg.touch ();

    MyFun fun (*this, msg);
    Iterative::Point result;
    const bool solved = Iterative::NelderMead (min_iter, max_iter, epsilon,
                                               fun, simplex, result, msg);
    std::ostringstream tmp;
    tmp << (solved ? "Solved." : "No solution.");
    for (size_t i = 0; i < result.size (); i++)
      tmp << "\n(" << parameter[i] << " " << result[i] << " [" 
          << original->dimension (parameter[i]) << "])";
    msg.message (tmp.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog& msg)
  { 
    bool ok = true;
    TREELOG_MODEL (msg);
    if (parameter.size () + 1 != simplex.size ())
      {
        ok = false;
        msg.error ("\
The simplex must have one more point than the amount of parameters");
      }
    for (size_t i = 0; i < simplex.size (); i++)
      if (simplex[i].size () != parameter.size ())
        {
          ok = false;
          Treelog::Open nest (msg, "simplex", i, "point");
          msg.error ("\
Each point in the simplex must have a value for each parameter");
        }

    if (!limit->initialize (units, lim_scope, msg) 
        || !limit->check (units, lim_scope, msg))
      {
        ok = false;
        msg.error ("Bad limit expression");
      }
    std::set<symbol> unique (parameter.begin (), parameter.end ());
    if (unique.size () != parameter.size ())
      {
	ok = false;
	msg.error ("parameter names should be unique");
      }
    return ok; 
  }

  static std::vector<symbol> build_parameter (const BlockModel& al)
  {
    if (al.check ("parameter"))
      return al.name_sequence ("parameter");

    daisy_assert (al.check ("parameters"));
    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& p
      = al.submodel_sequence ("parameters");
    std::vector<symbol> names;
    for (auto i: p)
	names.push_back (i->name ("name"));
    return names;
  }
  static Iterative::Simplex build_simplex (const BlockModel& al)
  {
    if (al.check ("simplex"))
      {
	Iterative::Simplex result;
	const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& p
	  = al.submodel_sequence ("simplex");
	for (size_t i = 0; i < p.size (); i++)
	  result.push_back (p[i]->number_sequence ("value"));
	return result;
      }
    daisy_assert (al.check ("parameters"));
    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& p
      = al.submodel_sequence ("parameters");
    std::vector<double> val1;
    std::vector<double> val2;

    for (auto i: p)
      {
	std::vector<double> vals = i->number_sequence ("value");
	daisy_assert (vals.size () == 2);
	daisy_assert (!isequal (vals[0], vals[1]));
	val1.push_back (vals[0]);
	val2.push_back (vals[1]);
      }
    Iterative::Simplex result;
    result.push_back (val1);
    for (size_t i = 0; i < val1.size (); i++)
      {
	std::vector<double> copy = val1;
	copy[i] = val2[i];
	result.push_back (copy);
      }
    return result;
  }

  ProgramOptimize (const BlockModel& al)
    : Program (al),
      metalib (al.metalib ()),
      units (al.units ()),
      parameter (build_parameter (al)),
      limit (Librarian::build_item<Boolean> (al, "limit")),
      original (al.model_ptr ("run")),
      scopesel (Librarian::build_item<Scopesel> (al, "scope")),
      expr (Librarian::build_item<Number> (al, "expr")),
      simplex (build_simplex (al)),
      lim_scope (*original, parameter),
      epsilon (al.number ("epsilon")),
      min_iter (al.integer ("min_iter")),
      max_iter (al.integer ("max_iter"))
  { }

  ~ProgramOptimize ()
  { }
};

static struct ProgramOptimizeSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramOptimize (al); }
  ProgramOptimizeSyntax ()
    : DeclareModel (Program::component, "minimize", 
                    "Find local minimum for program.\n\
\n\
The optimization will stop of the worst guess fail to improve more than\n\
'epsilon' within 'min_iter' iterations, or of the total number of\n\
iterations exceed 'max_iter'.")
  { }
  static bool check_alist (const Metalib&, const Frame& frame, Treelog& msg)
  {
    bool ok = true;
    const int min_iter = frame.integer ("min_iter");
    const int max_iter = frame.integer ("max_iter");
    if (min_iter > max_iter)
      {
        ok = false;
        msg.error ("'min_iter' should be less than 'max_iter'");
      }
    if (frame.check ("parameter") == frame.check ("parameters"))
      {
	ok = false;
	msg.error ("Exactly one of 'parameter' or 'parameters' must be present");
      }
    if (frame.check ("simplex") == frame.check ("parameters"))
      {
	ok = false;
	msg.error ("Exactly one of 'simplex' or 'parameters' must be present");
      }
    return ok;
  }
  static void load_parameter (Frame& frame)
  {
    frame.declare_string ("name", Attribute::Const, "Name of parameter.");
    frame.declare ("value", Attribute::User (), Check::none (), Attribute::Const,
		   2,  "Two values.");
    frame.set_check ("value", VCheck::unique ());
    frame.order ("name", "value");
  }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "nelder1965simplex");
    frame.add_check (check_alist);
    frame.declare_submodule_sequence ("parameters", Attribute::OptionalConst, "\
List of (NAME VAL1 VAL2).\n\
NAME is the name of a parameter to optimize, VAL1 and VAL2 are two\n\
different legal values for the parameter.", load_parameter);
    frame.declare_string ("parameter", Attribute::OptionalConst,
			  Attribute::Variable, "\
List of parameters to optimize.");
    static VCheck::All multi (VCheck::unique (), VCheck::min_size_1 ());
    frame.set_check ("parameter", multi);
    frame.declare_object ("limit", Boolean::component, "\
Limit parameter values so this expression is true.");
    frame.declare_submodule_sequence ("simplex", Attribute::OptionalConst, "\
List of points defining the initial simplex.\n\
You must define one more point than you have parameters.", 
                                      ProgramOptimize::MyPoint::load_syntax);
    frame.declare_object ("run", Program::component, "Program to optimize.");
    frame.declare_object ("scope", Scopesel::component, "\
Scope to evaluate expessions in.");
    frame.declare_object ("expr", Number::component, "\
Expression to minimize.");
    frame.declare ("epsilon", Attribute::Unknown (), Check::non_negative (), 
                   Attribute::Const, "\
Minimal improvement of worst point to be considered for 'min_iter'.");
    frame.declare_integer ("min_iter", Attribute::Const, "\
Maximal number of iterations with no improvement of worst point.");
    frame.set_check ("min_iter", VCheck::positive ());
    frame.set ("min_iter", 10);
    frame.declare_integer ("max_iter", Attribute::Const, "\
Stop after this number of iterations.");
    frame.set_check ("max_iter", VCheck::positive ());
  }
} ProgramOptimize_syntax;

// program_optimize.C ends here.
