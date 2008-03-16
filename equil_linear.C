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

#define BUILD_DLL

#include "equil.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "number.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include <memory>

struct EquilibriumLinear : public Equilibrium
{
  // Parameters.
  std::auto_ptr<Number> K_expr;

  // Simulation.
  void find (const Scope&, double has_A, double has_B, 
	     double& want_A, double& want_B, Treelog&) const;

  // Create and Destroy.
  void initialize (Treelog&);
  bool check (const Scope&, Treelog&) const;
  EquilibriumLinear (Block& al)
    : Equilibrium (al),
      K_expr (Librarian::build_item<Number> (al, "K"))
  { }
  ~EquilibriumLinear ()
  { }
};

void
EquilibriumLinear::find (const Scope& scope,
                         const double has_A, const double has_B, 
                         double& want_A, double& want_B, Treelog& msg) const
{
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;

  double K = 1.0;
  if (!K_expr->tick_value (K, Syntax::none (), scope, msg))
    msg.error ("Could not evaluate 'K'");

  // We need to solve the following equation w.r.t. B
  //
  //     M = A + B
  // ==>
  //     M = K B + B
  // ==>
  //     M = (1 + K) B
  // ==>
  //     B = M / (1 + K)

  want_B = M / (1.0 + K);
  want_A = M - want_B;
  daisy_assert (want_A >= 0.0);
}

void
EquilibriumLinear::initialize (Treelog& msg)
{ 
  K_expr->initialize (msg);
}

bool 
EquilibriumLinear::check (const Scope& scope, Treelog& msg) const
{
  Treelog::Open nest (msg, "linear");
  bool ok = true;
  if (!K_expr->check_dim (scope, Syntax::none (), msg))
    ok = false;
  return ok;
}

static struct EquilibriumLinearSyntax
{
  static Model& make (Block& al)
  { return *new EquilibriumLinear (al); }

  EquilibriumLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "A = K B");
    syntax.add_object ("K", Number::component, Syntax::Const, 
                       Syntax::Singleton, "The ratio A/B at equilibrium [].");

    Librarian::add_type (Equilibrium::component, "linear",
			 alist, syntax, &make);
  }
} EquilibriumLinear_syntax;
