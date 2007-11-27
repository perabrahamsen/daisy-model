// transform_equil.C -- Two soil components reching for equilibrium.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL:
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

#include "transform.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "soil.h"
#include "soil_water.h"
#include "equil.h"
#include "scope_soil.h"
#include "number.h"
#include "units.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include <memory>
#include <sstream>

struct TransformEquilibrium : public Transform
{
  // Parameters.
  std::auto_ptr<Equilibrium> equilibrium;
  std::auto_ptr<Number> k_AB_expr;
  std::auto_ptr<Number> k_BA_expr;

  // Simulation.
  void tick (const Soil&, const SoilWater&, const SoilHeat&,
             const std::vector<double>& A, const std::vector<double>& B,
             std::vector<double>& S_AB, Treelog&) const;

  // Create.
  void initialize (const Soil&, Treelog&);
  bool check (const Soil&, const SoilWater&, const SoilHeat&, Treelog&) const;
  TransformEquilibrium (Block& al)
    : Transform (al),
      equilibrium (Librarian::build_item<Equilibrium> (al, "equilibrium")),
      k_AB_expr (Librarian::build_item<Number> (al, "k_AB")),
      k_BA_expr (al.check ("k_BA")
		 ? Librarian::build_item<Number> (al, "k_BA")
		 : Librarian::build_item<Number> (al, "k_AB"))
  { }
};

void 
TransformEquilibrium::tick (const Soil& soil, const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    const std::vector<double>& A, 
			    const std::vector<double>& B,
                            std::vector<double>& S_AB, Treelog& msg) const
{ 
  const size_t cell_size = soil.size ();
  daisy_assert (A.size () == cell_size);
  daisy_assert (B.size () == cell_size);
  daisy_assert (S_AB.size () == cell_size);

  ScopeSoil scope (soil, soil_water, soil_heat);
  for (size_t c = 0; c < cell_size; c++)
    { 
      scope.set_cell (c);
      const double has_A = A[c];
      const double has_B = B[c];
      double want_A;
      double want_B;
      equilibrium->find (scope, has_A, has_B, want_A, want_B, msg);

      daisy_assert (approximate (has_A + has_B, want_A + want_B));

      double convert = 0.0;
      if (has_A > want_A)
	{
	  if (!k_AB_expr->tick_value (convert, Units::per_h (), scope, msg))
	    msg.error ("Could not evaluate 'k_AB'");

	  convert *= (has_A - want_A);
	}
      else
	{
	  if (!k_BA_expr->tick_value (convert, Units::per_h (), scope, msg))
	    msg.error ("Could not evaluate 'k_BA'");

	  convert *= -(has_B - want_B);
	}
      S_AB[c] = convert;
    }
}

bool
TransformEquilibrium::check (const Soil& soil, const SoilWater& soil_water,
			     const SoilHeat& soil_heat, Treelog& msg) const
{ 
  bool ok = true;

  ScopeSoil scope (soil, soil_water, soil_heat);
  const size_t cell_size = soil.size ();
  for (size_t c = 0; ok && c < cell_size; c++)
    {
      std::ostringstream tmp;
      tmp << "Transform '" + name + "' cell " << c;
      Treelog::Open nest (msg, tmp.str ());
      scope.set_cell (c);
      if (!equilibrium->check (scope, msg))
	ok = false;
      if (!k_AB_expr->check_dim (scope, Units::per_h (), msg))
	ok = false;
      if (!k_BA_expr->check_dim (scope, Units::per_h (), msg))
	ok = false;
    }
  return ok;
}

void
TransformEquilibrium::initialize (const Soil& soil, Treelog& msg)
{ 
  Treelog::Open nest (msg, name);
  equilibrium->initialize (msg);
  k_AB_expr->initialize (msg); 
  k_BA_expr->initialize (msg); 
}

static struct TransformEquilibriumSyntax
{
  static Model& make (Block& al)
  { return *new TransformEquilibrium (al); }
  TransformEquilibriumSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Transform::load_syntax (syntax, alist);

    alist.add ("description", 
	       "Two soil components reching for equilibrium.");
    syntax.add_object ("equilibrium", Equilibrium::component,
                       "Function for calculating equilibrium between A and B.");
    syntax.add_object ("k_AB", Number::component,
                       Syntax::Const, Syntax::Singleton, "\
Tranformation rate from soil component 'A' to 'B' [h^-1].");
    syntax.add_object ("k_BA", Number::component,
                       Syntax::OptionalConst, Syntax::Singleton, "\
Tranformation rate from soil component 'B' to 'A' [h^-1].\n\
By default, this is identical to 'k_AB'.");
    Librarian::add_type (Transform::component, "equilibrium",
			 alist, syntax, &make);
  }
} TransformEquilibrium_syntax;
