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
#include "pedo.h"
#include "soil.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include <memory>

using namespace std;

struct EquilibriumGoal_A : public Equilibrium
{
  // Parameters.
  /* const */ vector<double> goal_A;
  /* const */ vector<double> min_B;

  // Simulation.
  void find (const Soil&, const SoilWater&, unsigned int i,
	     double has_A, double has_B, 
	     double& want_A, double& want_B) const;

  // Create and Destroy.
  enum { uninitialized, init_succes, init_failure } initialize_state;
  void initialize (const Soil&, Treelog& err);
  bool check (const Soil&, Treelog& err) const;
  EquilibriumGoal_A (const AttributeList& al)
    : Equilibrium (al),
      initialize_state (uninitialized)
  { }
  ~EquilibriumGoal_A ()
  { }
};

void
EquilibriumGoal_A::find (const Soil&, const SoilWater&, unsigned int i,
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

  if (has_A > goal_A[i] || has_B > min_B[i])
    {
      want_B = max (min_B[i], M - goal_A[i]);
      want_A = M - want_B;
    }
  else
    {
      want_A = has_A;
      want_B = has_B;
    }
  daisy_assert (want_A >= 0.0);
  daisy_assert (want_B >= 0.0);
}

void
EquilibriumGoal_A::initialize (const Soil& soil, Treelog& err)
{ 
  daisy_assert (initialize_state == uninitialized);
  initialize_state = init_succes;

  auto_ptr<Pedotransfer> pedo_goal_A 
    (&Librarian<Pedotransfer>::create (alist.alist ("goal_A")));
  if (pedo_goal_A->check (soil, "g/cm^3", err))
    pedo_goal_A->set (soil, goal_A, "g/cm^3");
  else
    initialize_state = init_failure;
  Pedotransfer::debug_message ("goal_A", goal_A, err);

  auto_ptr<Pedotransfer> pedo_min_B 
    (&Librarian<Pedotransfer>::create (alist.alist ("min_B")));
  if (pedo_min_B->check (soil, "g/cm^3", err))
    pedo_min_B->set (soil, min_B, "g/cm^3");
  else
    initialize_state = init_failure;
  Pedotransfer::debug_message ("min_B", min_B, err);
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
  static Equilibrium& make (const AttributeList& al)
  { return *new EquilibriumGoal_A (al); }

  EquilibriumGoal_ASyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "Attempt to maintain A at at fixed level.");
    syntax.add ("goal_A", Librarian<Pedotransfer>::library (), Syntax::Const, 
                Syntax::Singleton, "The desired level of A [g/cm^3].");
    syntax.add ("min_B", Librarian<Pedotransfer>::library (), Syntax::Const, 
                Syntax::Singleton, 
                "Do not convert B to A if B is smaller than this [g/cm^3].");

    Librarian<Equilibrium>::add_type ("goal_A", alist, syntax, &make);
  }
} EquilibriumGoal_A_syntax;

struct EquilibriumGoal_B : public Equilibrium
{
  // Parameters.
  /* const */ vector<double> goal_B;
  /* const */ vector<double> min_A;

  // Simulation.
  void find (const Soil&, const SoilWater&, unsigned int i,
	     double has_A, double has_B, 
	     double& want_A, double& want_B) const;

  // Create and Destroy.
  enum { uninitialized, init_succes, init_failure } initialize_state;
  void initialize (const Soil&, Treelog& err);
  bool check (const Soil&, Treelog& err) const;
  EquilibriumGoal_B (const AttributeList& al)
    : Equilibrium (al),
      initialize_state (uninitialized)
  { }
  ~EquilibriumGoal_B ()
  { }
};

void
EquilibriumGoal_B::find (const Soil&, const SoilWater&, unsigned int i,
                         const double has_A, const double has_B, 
                         double& want_A, double& want_B) const
{
  daisy_assert (goal_B.size () > i);
  daisy_assert (goal_B[i] >= 0.0);
  daisy_assert (min_A.size () > i);
  daisy_assert (min_A[i] >= 0.0);
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  if (has_B > goal_B[i] || has_A > min_A[i])
    {
      want_A = max (min_A[i], M - goal_B[i]);
      want_B = M - want_A;
    }
  else
    {
      want_A = has_A;
      want_B = has_B;
    }
  daisy_assert (want_A >= 0.0);
  daisy_assert (want_B >= 0.0);
}

void
EquilibriumGoal_B::initialize (const Soil& soil, Treelog& err)
{ 
  daisy_assert (initialize_state == uninitialized);
  initialize_state = init_succes;

  auto_ptr<Pedotransfer> pedo_goal_B 
    (&Librarian<Pedotransfer>::create (alist.alist ("goal_B")));
  if (pedo_goal_B->check (soil, "g/cm^3", err))
    pedo_goal_B->set (soil, goal_B, "g/cm^3");
  else
    initialize_state = init_failure;
  Pedotransfer::debug_message ("goal_B", goal_B, err);

  auto_ptr<Pedotransfer> pedo_min_A 
    (&Librarian<Pedotransfer>::create (alist.alist ("min_A")));
  if (pedo_min_A->check (soil, "g/cm^3", err))
    pedo_min_A->set (soil, min_A, "g/cm^3");
  else
    initialize_state = init_failure;
  Pedotransfer::debug_message ("min_A", min_A, err);
}

bool 
EquilibriumGoal_B::check (const Soil&, Treelog& err) const
{
  if (initialize_state == init_succes)
    return true;

  Treelog::Open nest (err, name);
  err.error ("Initialize failed");
  return false;
}

static struct EquilibriumGoal_BSyntax
{
  static Equilibrium& make (const AttributeList& al)
  { return *new EquilibriumGoal_B (al); }

  EquilibriumGoal_BSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "Attempt to maintain B at at fixed level.");
    syntax.add ("goal_B", Librarian<Pedotransfer>::library (), Syntax::Const, 
                Syntax::Singleton, "The desired level of B [g/cm^3].");
    syntax.add ("min_A", Librarian<Pedotransfer>::library (), Syntax::Const, 
                Syntax::Singleton, 
                "Do not convert A to B if A is smaller than this [g/cm^3].");

    Librarian<Equilibrium>::add_type ("goal_B", alist, syntax, &make);
  }
} EquilibriumGoal_B_syntax;
