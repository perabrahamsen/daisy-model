// equil_langmuir.C --- Lagmuir model for calculating equilibrium (doh).
// 
// Copyright 2002 Per Abrahamsen and KVL.
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

struct EquilibriumLangmuir : public Equilibrium
{
  // Parameters.
  /* const */ vector<double> K;
  /* const */ vector<double> my_max;

  // Simulation.
  void find (const Soil&, const SoilWater&, unsigned int i,
	     double has_A, double has_B, 
	     double& want_A, double& want_B) const;

  // Create and Destroy.
  enum { uninitialized, init_succes, init_failure } initialize_state;
  void initialize (const Soil&, Treelog&);
  bool check (const Soil&, Treelog& err) const;
  EquilibriumLangmuir (const AttributeList& al)
    : Equilibrium (al),
      initialize_state (uninitialized)
  { }
  ~EquilibriumLangmuir ()
  { }
};

void
EquilibriumLangmuir::find (const Soil&, const SoilWater&, unsigned int i,
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
  //     M = (my_max B) / (K + B) + B
  // ==>
  //     M (K + B) = my_max B + B (K + B)
  // ==>
  //     M K + B M = my_max B + B K + B^2
  // ==> 
  //     0 = B^2 + (my_max + K - M) B - M K
  //
  // So we get a square equation.  We use the positive solution.

  daisy_assert (K.size () > i);
  daisy_assert (my_max.size () > i);

  const double a = 1.0;
  const double b = my_max[i] + K[i] - M;
  const double c = - M * K[i];

  want_B = single_positive_root_of_square_equation (a, b, c);
  want_A = M - want_B;
  daisy_assert (want_A >= 0.0);
}

void
EquilibriumLangmuir::initialize (const Soil& soil, Treelog& err)
{ 
  daisy_assert (initialize_state == uninitialized);
  initialize_state = init_succes;

  // K
  {
    auto_ptr<Pedotransfer> pedo_K 
      (&Librarian<Pedotransfer>::create (alist.alist ("K")));
    if (pedo_K->check (soil, "g/cm^3", err))
      pedo_K->set (soil, K, "g/cm^3");
    else 
      initialize_state = init_failure;
    Pedotransfer::debug_message ("K", K, err);
  }

  // my_max
  {
    auto_ptr<Pedotransfer> pedo_my_max 
      (&Librarian<Pedotransfer>::create (alist.alist ("my_max")));
    if (pedo_my_max->check (soil, "g/cm^3", err))
      pedo_my_max->set (soil, my_max, "g/cm^3");
    else 
      initialize_state = init_failure;
    Pedotransfer::debug_message ("my_max", my_max, err);
  }
}

bool 
EquilibriumLangmuir::check (const Soil&, Treelog& err) const
{
  if (initialize_state == init_succes)
    return true;

  Treelog::Open nest (err, name);
  err.error ("Initialize failed");
  return false;
}

static struct EquilibriumLangmuirSyntax
{
  static Equilibrium& make (const AttributeList& al)
  { return *new EquilibriumLangmuir (al); }

  EquilibriumLangmuirSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "A = (my_max B) / (K + B)");
    syntax.add ("K", Librarian<Pedotransfer>::library (), 
                Syntax::Const, Syntax::Singleton,
                "Half saturation constant [g/cm^3].");
    syntax.add ("my_max", Librarian<Pedotransfer>::library (), 
                Syntax::Const, Syntax::Singleton,
                "Max equilibrium capacity [g/cm^3].");
    Librarian<Equilibrium>::add_type ("Langmuir", alist, syntax, &make);
  }
} EquilibriumLangmuir_syntax;
