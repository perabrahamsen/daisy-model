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
#include "block.h"
#include "alist.h"
#include "scope_soil.h"
#include "number.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include <memory>
#include <sstream>

struct EquilibriumGoal_A : public Equilibrium
{
  static const symbol goal_unit;

  // Parameters.
  std::auto_ptr<Number> goal_A_expr;
  std::auto_ptr<Number> min_B_expr;
  const bool A_solute;
  const bool B_solute;
  const int debug_cell;

  // Simulation.
  void find (const Scope&, double has_A, double has_B, 
	     double& want_A, double& want_B, Treelog&) const;

  // Create and Destroy.
  void initialize (Treelog&);
  bool check (const Scope&, Treelog&) const;
  EquilibriumGoal_A (Block& al)
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
EquilibriumGoal_A::find (const Scope& scope,
                         const double has_A, const double has_B, 
                         double& want_A, double& want_B, Treelog& msg) const
{
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  double goal_A = 1.0;
  if (!goal_A_expr->tick_value (goal_A, goal_unit, scope, msg))
    msg.error ("Could not evaluate 'goal_A'");
  daisy_assert (goal_A >= 0.0);
  double min_B = 1.0;
  if (!min_B_expr->tick_value (min_B, goal_unit, scope, msg))
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
  if (const ScopeSoil *const scope_soil 
      = dynamic_cast<const ScopeSoil*> (&scope))
    if (scope_soil->cell == debug_cell)
      msg.message (tmp.str ());

  daisy_assert (want_A >= 0.0);
  daisy_assert (want_B >= 0.0);
  daisy_assert (approximate (want_A + want_B, M));
}

void
EquilibriumGoal_A::initialize (Treelog& msg)
{ 
  goal_A_expr->initialize (msg);
  min_B_expr->initialize (msg);
}

bool 
EquilibriumGoal_A::check (const Scope& scope, Treelog& msg) const
{
  Treelog::Open nest (msg, "Equilibrium: '" + name + "'");
  bool ok = true;
  if (!goal_A_expr->check_dim (scope, goal_unit, msg))
    ok = false;
  if (!min_B_expr->check_dim (scope, goal_unit, msg))
    ok = false;
  return ok;
}

static struct EquilibriumGoal_ASyntax
{
  static Model& make (Block& al)
  { return *new EquilibriumGoal_A (al); }

  EquilibriumGoal_ASyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "Attempt to maintain A at at fixed level.");
    syntax.add_object ("goal_A", Number::component, Syntax::Const, 
                       Syntax::Singleton, "The desired level of A [g/cm^3].");
    syntax.add ("A_solute", Syntax::Boolean, Syntax::Const, 
                "True iff 'goal_A' is in solute (mass per volume water).\n\
If false, the unit is assumed to be mass per volume space.");
    syntax.add_object ("min_B", Number::component, Syntax::Const, 
                       Syntax::Singleton, "\
Do not convert B to A if B is smaller than this [g/cm^3].");
    syntax.add ("B_solute", Syntax::Boolean, Syntax::Const, 
                "True iff 'min_B' is in solute (mass per volume water).\n\
If false, the unit is assumed to be mass per volume space.");
    syntax.add ("debug_cell", Syntax::Integer, Syntax::Const,
                "Print debug information for this cell.\n\
Set it to a negative number to disable it.");
    alist.add ("debug_cell", -1);
    Librarian::add_type (Equilibrium::component, "goal_A",
			 alist, syntax, &make);
  }
} EquilibriumGoal_A_syntax;

