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
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "number.h"
#include "soil.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include <memory>

struct EquilibriumLangmuir : public Equilibrium
{
  static const symbol base_unit;

  // Parameters.
  std::auto_ptr<Number> K_expr;
  std::auto_ptr<Number> my_max_expr;

  // Simulation.
  void find (const Scope&, double has_A, double has_B, 
	     double& want_A, double& want_B, Treelog&) const;

  // Create and Destroy.
  void initialize (Treelog&);
  bool check (const Scope&, Treelog&) const;
  EquilibriumLangmuir (Block& al)
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
EquilibriumLangmuir::find (const Scope& scope,
			   const double has_A, const double has_B, 
			   double& want_A, double& want_B, Treelog& msg) const
{
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  double K = 1.0;
  if (!K_expr->tick_value (K, base_unit, scope, msg))
    msg.error ("Could not evaluate 'K'");
  double my_max = 1.0;
  if (!my_max_expr->tick_value (my_max, base_unit, scope, msg))
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
EquilibriumLangmuir::initialize (Treelog& msg)
{ 
  K_expr->initialize (msg);
  my_max_expr->initialize (msg);
}

bool 
EquilibriumLangmuir::check (const Scope& scope, Treelog& msg) const
{
  Treelog::Open nest (msg, "Langmuir");
  bool ok = true;
  if (!K_expr->check_dim (scope, base_unit, msg))
    ok = false;
  if (!my_max_expr->check_dim (scope, base_unit, msg))
    ok = false;
  return ok;
}

static struct EquilibriumLangmuirSyntax
{
  static Model& make (Block& al)
  { return *new EquilibriumLangmuir (al); }

  EquilibriumLangmuirSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "A = (my_max B) / (K + B)");
    syntax.add_object ("K", Number::component, 
                       Syntax::Const, Syntax::Singleton,
                       "Half saturation constant [g/cm^3].");
    syntax.add_object ("my_max", Number::component, 
                       Syntax::Const, Syntax::Singleton,
                       "Max equilibrium capacity [g/cm^3].");
    Librarian::add_type (Equilibrium::component,
			 "Langmuir", alist, syntax, &make);
  }
} EquilibriumLangmuir_syntax;
