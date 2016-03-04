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

#define BUILD_DLL

#include "equil.h"
#include "block_model.h"
#include "number.h"
#include "soil.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <memory>

struct EquilibriumLangmuir : public Equilibrium
{
  static const symbol base_unit;

  // Parameters.
  std::unique_ptr<Number> K_expr;
  std::unique_ptr<Number> my_max_expr;

  // Simulation.
  void find (const Units& units, const Scope&, int, double has_A, double has_B, 
	     double& want_A, double& want_B, Treelog&) const;

  // Create and Destroy.
  void initialize (const Units& units, const Scope&, Treelog&);
  bool check (const Units& units, const Scope&, Treelog&) const;
  EquilibriumLangmuir (const BlockModel& al)
    : Equilibrium (al),
      K_expr (Librarian::build_item<Number> (al, "K")),
      my_max_expr (Librarian::build_item<Number> (al, "my_max"))
  { }
  ~EquilibriumLangmuir ()
  { }
};

const symbol 
EquilibriumLangmuir::base_unit ("g/cm^3");

void
EquilibriumLangmuir::find (const Units& units, const Scope& scope, int, 
			   const double has_A, const double has_B, 
			   double& want_A, double& want_B, Treelog& msg) const
{
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  double K = 1.0;
  if (!K_expr->tick_value (units, K, base_unit, scope, msg))
    msg.error ("Could not evaluate 'K'");
  double my_max = 1.0;
  if (!my_max_expr->tick_value (units, my_max, base_unit, scope, msg))
    msg.error ("Could not evaluate 'my_max'");

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

  const double a = 1.0;
  const double b = my_max + K - M;
  const double c = - M * K;

  want_B = single_positive_root_of_square_equation (a, b, c);
  want_A = M - want_B;
  daisy_assert (want_A >= 0.0);
}

void
EquilibriumLangmuir::initialize (const Units& units, const Scope& scope,
                                 Treelog& msg)
{ 
  K_expr->initialize (units, scope, msg);
  my_max_expr->initialize (units, scope, msg);
}

bool 
EquilibriumLangmuir::check (const Units& units, 
                            const Scope& scope, Treelog& msg) const
{
  Treelog::Open nest (msg, "Langmuir");
  bool ok = true;
  if (!K_expr->check_dim (units, scope, base_unit, msg))
    ok = false;
  if (!my_max_expr->check_dim (units, scope, base_unit, msg))
    ok = false;
  return ok;
}

static struct EquilibriumLangmuirSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new EquilibriumLangmuir (al); }

  EquilibriumLangmuirSyntax ()
    : DeclareModel (Equilibrium::component,
			 "Langmuir", "A = (my_max B) / (K + B)")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("K", Number::component, 
                       Attribute::Const, Attribute::Singleton,
                       "Half saturation constant [g/cm^3].");
    frame.declare_object ("my_max", Number::component, 
                       Attribute::Const, Attribute::Singleton,
                       "Max equilibrium capacity [g/cm^3].");
  }
} EquilibriumLangmuir_syntax;
