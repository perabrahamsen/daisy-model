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
#include "block_model.h"
#include "number.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <memory>

struct EquilibriumLinear : public Equilibrium
{
  // Parameters.
  std::unique_ptr<Number> K_expr;

  // Simulation.
  void find (const Units& units, const Scope&, int, double has_A, double has_B, 
	     double& want_A, double& want_B, Treelog&) const;

  // Create and Destroy.
  void initialize (const Units& units, const Scope&, Treelog&);
  bool check (const Units& units, const Scope&, Treelog&) const;
  EquilibriumLinear (const BlockModel& al)
    : Equilibrium (al),
      K_expr (Librarian::build_item<Number> (al, "K"))
  { }
  ~EquilibriumLinear ()
  { }
};

void
EquilibriumLinear::find (const Units& units, const Scope& scope, int, 
                         const double has_A, const double has_B, 
                         double& want_A, double& want_B, Treelog& msg) const
{
  Treelog::Open nest (msg, "equil:linear:find");
  daisy_assert (has_A >= 0.0);
  daisy_assert (has_B >= 0.0);
  const double M = has_A + has_B;
  daisy_assert (std::isfinite (M));

  double K = 1.0;
  if (!K_expr->tick_value (units, K, Attribute::None (), scope, msg))
    msg.error ("Could not evaluate 'K'");
  daisy_assert (K >= 0.0);

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
  daisy_assert (std::isfinite (want_B));
  want_A = M - want_B;
  daisy_assert (std::isfinite (want_A));
  if (want_A < 0.0)
    {
      want_A = 0.0;
      want_B = M;
    }
}

void
EquilibriumLinear::initialize (const Units& units, const Scope& scope, Treelog& msg)
{ 
  Treelog::Open nest (msg, "equil:linear:initialize");
  K_expr->initialize (units, scope, msg);
}

bool 
EquilibriumLinear::check (const Units& units, const Scope& scope, Treelog& msg) const
{
  Treelog::Open nest (msg, "equil:linear:check");
  bool ok = true;
  if (!K_expr->check_dim (units, scope, Attribute::None (), msg))
    ok = false;
  return ok;
}

static struct EquilibriumLinearSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new EquilibriumLinear (al); }

  EquilibriumLinearSyntax ()
    : DeclareModel (Equilibrium::component, "linear", "A = K B")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("K", Number::component, Attribute::Const, 
                       Attribute::Singleton, "The ratio A/B at equilibrium [].");

  }
} EquilibriumLinear_syntax;
