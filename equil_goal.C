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


#include "equil.h"
#include "block.h"
#include "alist.h"
#include "soil_water.h"
#include "pedo.h"
#include "soil.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include <memory>
#include <sstream>
#include <iostream>

using namespace std;

struct EquilibriumGoal_A : public Equilibrium
{
  // Parameters.
  /* const */ vector<double> goal_A;
  const bool A_solute;
  /* const */ vector<double> min_B;
  const bool B_solute;
  const int debug_cell;

  // Simulation.
  void find (const Soil&, const SoilWater&, unsigned int i,
	     double has_A, double has_B, 
	     double& want_A, double& want_B) const;

  // Create and Destroy.
  enum { uninitialized, init_succes, init_failure } initialize_state;
  void initialize (Block&, const Soil&);
  bool check (const Soil&, Treelog& err) const;
  EquilibriumGoal_A (Block& al)
    : Equilibrium (al),
      A_solute (al.flag ("A_solute")),
      B_solute (al.flag ("B_solute")),
      debug_cell (al.integer ("debug_cell")),
      initialize_state (uninitialized)
  { }
  ~EquilibriumGoal_A ()
  { }
};

void
EquilibriumGoal_A::find (const Soil&, const SoilWater& soil_water,
                         unsigned int i,
                         const double has_A, const double has_B, 
                         double& want_A, double& want_B) const
{
  daisy_assert (goal_A.size () > i);
  daisy_assert (goal_A[i] >= 0.0);
  daisy_assert (min_B.size () > i);
  daisy_assert (min_B[i] >= 0.0);
  
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  const double Theta = soil_water.Theta (i);
  daisy_assert (Theta > 0.0);
  daisy_assert (Theta < 1.0);
  const double goal_A_dry = goal_A[i] * (A_solute ? Theta : 1.0);
  const double min_B_dry = min_B[i] * (B_solute ? Theta : 1.0);

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
      want_B = max (min_B_dry, M - goal_A_dry);
      want_A = M - want_B;
    }
  tmp << "\t" << has_A << "\t" << goal_A_dry << "\t" << want_A << "\t"
      << has_B << "\t" << min_B_dry << "\t" << want_B << "\t" << M;
  if (i == debug_cell)
    cout << tmp.str () << "\n";

  daisy_assert (want_A >= 0.0);
  daisy_assert (want_B >= 0.0);
  daisy_assert (approximate (want_A + want_B, M));
}

void
EquilibriumGoal_A::initialize (Block& block, const Soil& soil)
{ 
  daisy_assert (initialize_state == uninitialized);
  initialize_state = init_succes;

  auto_ptr<Pedotransfer> pedo_goal_A 
    (Librarian::build_alist<Pedotransfer> (block, alist.alist ("goal_A"),
                                          "goal_A"));
  if (pedo_goal_A->check (soil, "g/cm^3", block.msg ()))
    pedo_goal_A->set (soil, goal_A, "g/cm^3");
  else
    initialize_state = init_failure;
  Pedotransfer::debug_message ("goal_A", goal_A, "g/cm^3", block.msg ());

  auto_ptr<Pedotransfer> pedo_min_B 
    (Librarian::build_alist<Pedotransfer> (block, alist.alist ("min_B"),
                                          "min_B"));
  if (pedo_min_B->check (soil, "g/cm^3", block.msg ()))
    pedo_min_B->set (soil, min_B, "g/cm^3");
  else
    initialize_state = init_failure;
  Pedotransfer::debug_message ("min_B", min_B, "g/cm^3",block.msg ());

  if (debug_cell >= 0 && debug_cell < soil.size ())
    cout << "type\thas_A\tgoal_A\twant_A\thas_B\tgoal_B\twant_B\ttotal\n"
         << "\tg/cm^3\tg/cm^3\tg/cm^3\tg/cm^3\tg/cm^3\tg/cm^3\tg/cm^3\n";
}

bool 
EquilibriumGoal_A::check (const Soil&, Treelog& err) const
{
  if (initialize_state == init_succes)
    return true;

  Treelog::Open nest (err, name);
  err.error ("Initialize failed");
  return false;
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
    syntax.add_object ("goal_A", Pedotransfer::component, Syntax::Const, 
                       Syntax::Singleton, "The desired level of A [g/cm^3].");
    syntax.add ("A_solute", Syntax::Boolean, Syntax::Const, 
                "True iff 'goal_A' is in solute (mass per volume water).\n\
If false, the unit is assumed to be mass per volume space.");
    syntax.add_object ("min_B", Pedotransfer::component, Syntax::Const, 
                       Syntax::Singleton, 
                       "Do not convert B to A if B is smaller than this [g/cm^3].");
    syntax.add ("B_solute", Syntax::Boolean, Syntax::Const, 
                "True iff 'min_B' is in solute (mass per volume water).\n\
If false, the unit is assumed to be mass per volume space.");
    syntax.add ("debug_cell", Syntax::Integer, Syntax::Const,
                "Print debug information for this cell.\n\
Set it to a negative number to disable it.");
    alist.add ("debug_cell", -1);
    Librarian::add_type (Equilibrium::component, "goal_A", alist, syntax, &make);
  }
} EquilibriumGoal_A_syntax;

