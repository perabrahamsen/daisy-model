// equil_linear.C --- Linear model for calculating equilibrium.
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

struct EquilibriumLinear : public Equilibrium
{
  // Parameters.
  /* const */ vector<double> K;

  // Simulation.
  void find (const Soil&, const SoilWater&, unsigned int i,
	     double has_A, double has_B, 
	     double& want_A, double& want_B) const;

  // Create and Destroy.
  enum { uninitialized, init_succes, init_failure } initialize_state;
  void initialize (const Soil&, Treelog& err);
  bool check (const Soil&, Treelog& err) const;
  EquilibriumLinear (const AttributeList& al)
    : Equilibrium (al),
      initialize_state (uninitialized)
  { }
  ~EquilibriumLinear ()
  { }
};

void
EquilibriumLinear::find (const Soil&, const SoilWater&, unsigned int i,
                         const double has_A, const double has_B, 
                         double& want_A, double& want_B) const
{
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  // We need to solve the following equation w.r.t. B
  //
  //     M = A + B
  // ==>
  //     M = K B + B
  // ==>
  //     M = (1 + K) B
  // ==>
  //     B = M / (1 + K)

  daisy_assert (K.size () > i);

  want_B = M / (1.0 + K[i]);
  want_A = M - want_B;
  daisy_assert (want_A >= 0.0);
}

void
EquilibriumLinear::initialize (const Soil& soil, Treelog& err)
{ 
  daisy_assert (initialize_state == uninitialized);
  initialize_state = init_succes;

  auto_ptr<Pedotransfer> pedo_K 
    (Librarian<Pedotransfer>::create (alist.alist ("K")));
  if (pedo_K->check (soil, Syntax::None (), err))
    pedo_K->set (soil, K, Syntax::None ());
  else
    initialize_state = init_failure;
  Pedotransfer::debug_message ("K", K, "", err);
}

bool 
EquilibriumLinear::check (const Soil&, Treelog& err) const
{
  if (initialize_state == init_succes)
    return true;

  Treelog::Open nest (err, name);
  err.error ("Initialize failed");
  return false;
}

static struct EquilibriumLinearSyntax
{
  static Equilibrium& make (const AttributeList& al)
  { return *new EquilibriumLinear (al); }

  EquilibriumLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "A = K B");
    syntax.add ("K", Librarian<Pedotransfer>::library (), Syntax::Const, 
                Syntax::Singleton, "The ratio A/B at equilibrium [].");

    Librarian<Equilibrium>::add_type ("linear", alist, syntax, &make);
  }
} EquilibriumLinear_syntax;
