// equil_langmuir.C
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


#include "equil.h"
#include "pedo.h"
#include "soil.h"
#include "treelog.h"
#include "check.h"
#include "mathlib.h"

class EquilibriumLangmuir : public Equilibrium
{
  // Parameters.
  /* const */ vector<double> K;
  /* const */ vector<double> my_max;
  const Pedotransfer *const pedo_K;
  const Pedotransfer *const pedo_my_max;

  // Simulation.
public:
  void find (const Soil&, const SoilWater&, unsigned int i,
	     double has_A, double has_B, 
	     double& want_A, double& want_B) const;
  bool check (const Soil&, Treelog& err) const;

  // Create and Destroy.
public:
  void initialize (const Soil&);
public:
  EquilibriumLangmuir (const AttributeList& al);
};

void
EquilibriumLangmuir::find (const Soil&, const SoilWater&, unsigned int i,
			   const double has_A, const double has_B, 
			   double& want_A, double& want_B) const
{
  assert (has_A >= 0.0);
  assert (has_B >= 0.0);
  const double M = has_A + has_B;

  // We need to solve the following equation w.r.t. B
  //
  //     M = A + B
  // ==>
  //     M = (my_max B) / (K + B) + B
  // ==>
  //     M (K + B) = my_max B + B (K + B)
  // ==> 
  //     0 = B^2 + (my_max + K - M) B - M K
  //
  // So we get a square equation.  We use the positive solution.

  assert (K.size () > i);
  assert (my_max.size () > i);

  const double a = 1.0;
  const double b = my_max[i] + K[i] - M;
  const double c = - M * K[i];

  want_B = single_positive_root_of_square_equation (a, b, c);
  want_A = M - want_B;
  assert (want_A >= 0.0);
}

bool
EquilibriumLangmuir::check (const Soil& soil, Treelog& err) const
{ 
  bool ok = true;
  if (pedo_K)
    {
      Treelog::Open nest (err, "pedo_K");
      if (!pedo_K->check (soil, err))
	ok = false;
    }
  if (pedo_my_max)
    {
      Treelog::Open nest (err, "pedo_my_max");
      if (!pedo_my_max->check (soil, err))
	ok = false;
    }
  return ok;
}

void
EquilibriumLangmuir::initialize (const Soil& soil)
{ 
  if (K.size () > 0)
    K.insert (K.end (), soil.size () - K.size (), K.back ());
  else
    {
      assert (pedo_K);
      pedo_K->initialize (soil);
      pedo_K->set (soil, K);
    }

  if (my_max.size () > 0)
    my_max.insert (my_max.end (), 
		   soil.size () - my_max.size (), my_max.back ());
  else
    {
      assert (pedo_my_max);
      pedo_my_max->initialize (soil);
      pedo_my_max->set (soil, my_max);
    }
}

EquilibriumLangmuir::EquilibriumLangmuir (const AttributeList& al)
  : Equilibrium (al),
    pedo_K (al.check ("pedo_K")
	    ? &Librarian<Pedotransfer>::create (al.alist ("pedo_K"))
	    : NULL),
    pedo_my_max (al.check ("pedo_my_max")
		 ? &Librarian<Pedotransfer>::create (al.alist ("pedo_my_max"))
		 : NULL)
{
  if (al.check ("K"))
    K = al.number_sequence ("K");
  if (al.check ("my_max"))
    my_max = al.number_sequence ("my_max");
}

static struct EquilibriumLangmuirSyntax
{
  static Equilibrium& make (const AttributeList& al)
  { return *new EquilibriumLangmuir (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    bool ok = true;

    if ((!al.check ("K") || al.number_sequence ("K").size () < 1)
	&& al.check ("pedo_K"))
      {
	err.entry ("You must specify either 'K' or 'pedo_K'");
	ok = false;
      }
    if ((!al.check ("my_max") || al.number_sequence ("my_max").size () < 1)
	&& al.check ("pedo_my_max"))
      {
	err.entry ("You must specify either 'my_max' or 'pedo_my_max'");
	ok = false;
      }

    return ok;

  }
  EquilibriumLangmuirSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    Equilibrium::load_syntax (syntax, alist);
    alist.add ("description", "A = (my_max B) / (K + B)");
    syntax.add ("K", "g/cm^3", Check::non_negative (), 
		Syntax::OptionalConst, Syntax::Sequence,
		"Half saturation constant.");
    syntax.add ("pedo_K", Librarian<Pedotransfer>::library (),
		Syntax::OptionalConst, Syntax::Singleton,
		"Function to calculate 'K' if not specified.");
    syntax.add ("my_max", "g/cm^3", Check::non_negative (), 
		Syntax::OptionalConst, Syntax::Sequence,
		"Max equilibrium capacity.");
    syntax.add ("pedo_my_max", Librarian<Pedotransfer>::library (),
		Syntax::OptionalConst, Syntax::Singleton,
		"Function to calculate 'my_max' if not specified.");

    Librarian<Equilibrium>::add_type ("Langmuir", alist, syntax, &make);
  }
} EquilibriumLangmuir_syntax;
