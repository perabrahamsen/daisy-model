// equil_goal.C --- Goal based model for calculating equilibrium.
// 
// Copyright 2002, 2004 Per Abrahamsen and KVL.
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

#include "equil.h"
#include "block_model.h"
#include "scope_soil.h"
#include "number.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <memory>
#include <sstream>

struct EquilibriumGoal_A : public Equilibrium
{
  static const symbol goal_unit;

  // Parameters.
  std::unique_ptr<Number> goal_A_expr;
  std::unique_ptr<Number> min_B_expr;
  const bool A_solute;
  const bool B_solute;
  const int debug_cell;

  // Simulation.
  void find (const Units& units, const Scope&, int cell, double has_A, double has_B, 
	     double& want_A, double& want_B, Treelog&) const;

  // Create and Destroy.
  void initialize (const Units&, const Scope&, Treelog&);
  bool check (const Units& units, const Scope&, Treelog&) const;
  EquilibriumGoal_A (const BlockModel& al)
    : Equilibrium (al),
      goal_A_expr (Librarian::build_item<Number> (al, "goal_A")),
      min_B_expr (Librarian::build_item<Number> (al, "min_B")),
      A_solute (al.flag ("A_solute")),
      B_solute (al.flag ("B_solute")),
      debug_cell (al.integer ("debug_cell"))
  { }
  ~EquilibriumGoal_A ()
  { }
};

const symbol
EquilibriumGoal_A::goal_unit ("g/cm^3");

void
EquilibriumGoal_A::find (const Units& units, const Scope& scope, int cell, 
                         const double has_A, const double has_B, 
                         double& want_A, double& want_B, Treelog& msg) const
{
  TREELOG_SUBMODEL (msg, "goal_A");
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  double goal_A = 1.0;
  if (!goal_A_expr->tick_value (units, goal_A, goal_unit, scope, msg))
    msg.error ("Could not evaluate 'goal_A'");
  daisy_assert (std::isfinite (goal_A));
  if (goal_A < 0.0)
    {
      std::ostringstream tmp;
      tmp << "Goal A is " << goal_A << ", should be non-negative";
      msg.error (tmp.str ());
      goal_A = 0.0;
    }
  double min_B = 1.0;
  if (!min_B_expr->tick_value (units, min_B, goal_unit, scope, msg))
    msg.error ("Could not evaluate 'min_B'");
  daisy_assert (min_B >= 0.0);

  static const symbol Theta_name ("Theta");
  const double Theta = scope.number (Theta_name);
  daisy_assert (Theta > 0.0);
  daisy_assert (Theta < 1.0);

  const double goal_A_dry = goal_A * (A_solute ? Theta : 1.0);
  const double min_B_dry = min_B * (B_solute ? Theta : 1.0);

  std::ostringstream tmp;

  if (has_A > goal_A_dry)
    {
      tmp << "A->B";
      // We have too much A, convert surplus to B.
      want_A = goal_A_dry;
      want_B = M - want_A;
    }
  else if (has_B < min_B_dry)
    {
      tmp << "min_B";
      // We have too little A and too little B, do nothing.
      want_A = has_A;
      want_B = has_B;
    }
  else
    {
      tmp << "B->A";
      
      // We have too little A and too much B, convert just enough to fit one.
      want_B = std::max (min_B_dry, M - goal_A_dry);
      want_A = M - want_B;
    }
  tmp << "\t" << has_A << "\t" << goal_A_dry << "\t" << want_A << "\t"
      << has_B << "\t" << min_B_dry << "\t" << want_B << "\t" << M;
  if (cell == debug_cell)
    msg.message (tmp.str ());

  daisy_assert (want_A >= 0.0);
  daisy_assert (want_B >= 0.0);
  daisy_assert (approximate (want_A + want_B, M));
}

void
EquilibriumGoal_A::initialize (const Units& units, const Scope& scope, 
                               Treelog& msg)
{ 
  goal_A_expr->initialize (units, scope, msg);
  min_B_expr->initialize (units, scope, msg);
}

bool 
EquilibriumGoal_A::check (const Units& units, 
                          const Scope& scope, Treelog& msg) const
{
  bool ok = true;
  if (!goal_A_expr->check_dim (units, scope, goal_unit, msg))
    ok = false;
  if (!min_B_expr->check_dim (units, scope, goal_unit, msg))
    ok = false;
  return ok;
}

static struct EquilibriumGoal_ASyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new EquilibriumGoal_A (al); }

  EquilibriumGoal_ASyntax ()
    : DeclareModel (Equilibrium::component, "goal_A", "Attempt to maintain A at at fixed level.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("goal_A", Number::component, Attribute::Const, 
                       Attribute::Singleton, "The desired level of A [g/cm^3].");
    frame.declare_boolean ("A_solute", Attribute::Const, 
                "True iff 'goal_A' is in solute (mass per volume water).\n\
If false, the unit is assumed to be mass per volume space.");
    frame.declare_object ("min_B", Number::component, Attribute::Const, 
                       Attribute::Singleton, "\
Do not convert B to A if B is smaller than this [g/cm^3].");
    frame.declare_boolean ("B_solute", Attribute::Const, 
                "True iff 'min_B' is in solute (mass per volume water).\n\
If false, the unit is assumed to be mass per volume space.");
    frame.declare_integer ("debug_cell", Attribute::Const,
                "Print debug information for this cell.\n\
Set it to a negative number to disable it.");
    frame.set ("debug_cell", -1);
  }
} EquilibriumGoal_A_syntax;

