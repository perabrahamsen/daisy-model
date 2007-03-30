// transform_equil.C -- Two soil components reching for equilibrium.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "transform.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "soil.h"
#include "soil_water.h"
#include "pedo.h"
#include "equil.h"
#include "check.h"
#include "mathlib.h"
#include <memory>

using namespace std;

struct TransformEquilibrium : public Transform
{
  // Parameters.
  /* const */ auto_ptr<Equilibrium> equilibrium;
  /* const */ vector<double> k_AB;
  /* const */ vector<double> k_BA;

  // Simulation.
  void tick (const Soil&, const SoilWater&, 
             const vector<double>& A, const vector<double>& B,
             vector<double>& S_AB, Treelog&) const;

  // Create.
  enum { uninitialized, init_succes, init_failure } initialize_state;
  void initialize (Block&, const Soil&);
  bool check (const Soil&, Treelog& err) const;
  TransformEquilibrium (Block& al)
    : Transform (al),
      equilibrium (Librarian<Equilibrium>::build_item (al, "equilibrium")),
      initialize_state (uninitialized)
  { }
};

void 
TransformEquilibrium::tick (const Soil& soil, const SoilWater& soil_water,
			    const vector<double>& A, const vector<double>& B,
                            vector<double>& S_AB, Treelog&) const
{ 
  daisy_assert (k_AB.size () == soil.size ());
  daisy_assert (k_BA.size () == soil.size ());
  daisy_assert (A.size () == soil.size ());
  daisy_assert (B.size () == soil.size ());
  daisy_assert (S_AB.size () == soil.size ());

  for (unsigned int i = 0; i < soil.size (); i++)
    { 
      const double has_A = A[i];
      const double has_B = B[i];
      double want_A;
      double want_B;
      equilibrium->find (soil, soil_water, i, has_A, has_B, want_A, want_B);
      daisy_assert (approximate (has_A + has_B, want_A + want_B));

      const double convert = (has_A > want_A)
	? k_AB[i] * (has_A - want_A)
	: -k_BA[i] * (has_B - want_B);
      
      S_AB[i] = convert;
	
    }
}

bool
TransformEquilibrium::check (const Soil& soil, Treelog& err) const
{ 
  Treelog::Open nest (err, name);
  bool ok = true;

  if (!equilibrium->check (soil, err))
    ok = false;

  if (initialize_state != init_succes)
    { 
      err.error ("Initialize failed");
      ok = false;
    }
  return ok;
}

void
TransformEquilibrium::initialize (Block& block, const Soil& soil)
{ 
  Treelog::Open nest (block.msg (), name);
  equilibrium->initialize (block, soil);

  daisy_assert (initialize_state == uninitialized);
  initialize_state = init_succes;

  // k_AB
  {
    Treelog::Open nest (block.msg (), "k_AB");
    auto_ptr<Pedotransfer> pedo_AB 
      (Librarian<Pedotransfer>::build_alist (block, alist.alist ("k_AB"), 
                                             "k_AB"));
    if (pedo_AB->check (soil, "h^-1", block.msg ()))
      pedo_AB->set (soil, k_AB, "h^-1");
    else
      initialize_state = init_failure;

    Pedotransfer::debug_message ("k_AB", k_AB, "h^-1", block.msg ());
  }
  
  // k_BA
  if (alist.check ("k_BA"))
    {
      Treelog::Open nest (block.msg (), "k_BA");
      auto_ptr<Pedotransfer> pedo_BA 
        (Librarian<Pedotransfer>::build_alist (block, alist.alist ("k_BA"),
                                               "k_BA"));
      if (pedo_BA->check (soil, "h^-1", block.msg ()))
        pedo_BA->set (soil, k_BA, "h^-1");
      else
        initialize_state = init_failure;
      Pedotransfer::debug_message ("k_BA", k_BA, "h^-1", block.msg ());
    }
  else
    k_BA = k_AB;
  
  if (alist.check ("debug"))
    {
      Treelog::Open nest (block.msg (), "debug");
      const vector<AttributeList*> alists = alist.alist_sequence ("debug");
      for (unsigned int i = 0; i < alists.size (); i++)
        {
          vector<double> debug;
          auto_ptr<Pedotransfer> pedo_debug 
            (Librarian<Pedotransfer>::build_alist (block, *alists[i],
                                                   sequence_id ("debug", i)));
          if (pedo_debug->check (soil, pedo_debug->dimension (), block.msg ()))
            pedo_debug->set (soil, debug, pedo_debug->dimension ());
          else
            initialize_state = init_failure;
          Pedotransfer::debug_message (pedo_debug->name.name (), 
                                       debug, pedo_debug->dimension (), block.msg ());
        }
    }
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
                       "Function for calculating equilibrioum between A and B.");
    syntax.add_object ("k_AB", Pedotransfer::component,
                       Syntax::Const, Syntax::Singleton, 
                       "Tranformation rate from soil component 'A' to 'B' [h^-1].");
    syntax.add_object ("k_BA", Pedotransfer::component,
                       Syntax::OptionalConst, Syntax::Singleton,
                       "Tranformation rate from soil component 'B' to 'A' [h^-1].\n\
By default, this is identical to 'k_AB'.");
    syntax.add_object ("debug", Pedotransfer::component,
                       Syntax::OptionalConst, Syntax::Sequence, "\
Extra pedotransfer function to include in 'daisy.log' for debugging.");
    BuildBase::add_type (Transform::component, "equilibrium", alist, syntax, &make);
  }
} TransformEquilibrium_syntax;
