// transform_equil.C -- Two soil chemicals reching for equilibrium.
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
#include "soil.h"
#include "soil_water.h"
#include "soil_chemicals.h"
#include "pedo.h"
#include "equil.h"
#include "check.h"
#include "log.h"
#include "mathlib.h"

struct TransformEquilibrium : public Transform
{
  // Parameters.
  const string& name_A;
  const string& name_B;
  /* const */ Equilibrium& equilibrium;
  vector<double> k_AB;
  vector<double> k_BA;
  const Pedotransfer* pedo_AB;
  const Pedotransfer* pedo_BA;

  // Log variables.
  vector<double> S_AB;
  void output (Log&) const;

  // Simulation.
  void tick (const Soil&, const SoilWater&, SoilChemicals&);

  // Create.
  bool check (const Soil&, Treelog& err) const;
  void initialize (const Soil&);
  TransformEquilibrium (const AttributeList& al);
};

void
TransformEquilibrium::output (Log& log) const
{ 
  log.output ("S_AB", S_AB);
}

void 
TransformEquilibrium::tick (const Soil& soil, const SoilWater& soil_water,
			    SoilChemicals& soil_chemicals)
{ 
  assert (k_AB.size () == soil.size ());
  assert (k_BA.size () == soil.size ());

  SoilChemical& A = soil_chemicals.find (soil, soil_water, name_A);
  SoilChemical& B = soil_chemicals.find (soil, soil_water, name_B);

  for (unsigned int i = 0; i < soil.size (); i++)
    { 
      const double has_A = A.M (i);
      const double has_B = B.M (i);
      double want_A;
      double want_B;
      equilibrium.find (soil, soil_water, i, has_A, has_B, want_A, want_B);
      assert (approximate (has_A + has_B, want_A + want_B));

      const double convert = (has_A > want_A)
	? k_AB[i] * (has_A - want_A)
	: -k_BA[i] * (has_B - want_B);
      
      S_AB[i] = convert;
	
    }
  A.add_to_sink (S_AB);
  B.add_to_source (S_AB);
}

bool
TransformEquilibrium::check (const Soil& soil, Treelog& err) const
{ 
  bool ok = true;
  {
    Treelog::Open nest (err, "equilibrium");
    if (!equilibrium.check (soil, err))
      ok = false;
  }
  if (pedo_AB)
    {
      Treelog::Open nest (err, "pedo_AB");
      if (!pedo_AB->check (soil, err))
	ok = false;
    }
  if (pedo_BA)
    {
      Treelog::Open nest (err, "pedo_BA");
      if (!pedo_BA->check (soil, err))
	ok = false;
    }
  return ok;
}

void
TransformEquilibrium::initialize (const Soil& soil)
{ 
  equilibrium.initialize (soil);

  if (k_AB.size () > 0)
    k_AB.insert (k_AB.end (), soil.size () - k_AB.size (), k_AB.back ());
  else
    {
      assert (pedo_AB);
      pedo_AB->initialize (soil);
      pedo_AB->set (soil, k_AB);
    }

  if (k_BA.size () > 0)
    k_BA.insert (k_BA.end (), soil.size () - k_BA.size (), k_BA.back ());
  else if (pedo_BA)
    {
      pedo_BA->initialize (soil);
      pedo_BA->set (soil, k_BA);
    }
  else
    k_BA = k_AB;

  S_AB.insert (S_AB.end (), soil.size (), 0.0);
}

TransformEquilibrium::TransformEquilibrium (const AttributeList& al)
  : Transform (al),
    name_A (al.name ("A")),
    name_B (al.name ("B")),
    equilibrium (Librarian<Equilibrium>::create (al.alist ("equilibrium"))),
    pedo_AB (al.check ("pedo_AB") 
	     ? &Librarian<Pedotransfer>::create (al.alist ("pedo_AB"))
	     : NULL),
    pedo_BA (al.check ("pedo_BA")
	     ? &Librarian<Pedotransfer>::create (al.alist ("pedo_BA"))
	     : NULL)
{
  if (al.check ("k_AB"))
    k_AB = al.number_sequence ("k_AB");
  if (al.check ("k_BA"))
    k_BA = al.number_sequence ("k_BA");
}

static struct TransformEquilibriumSyntax
{
  static Transform& make (const AttributeList& al)
  { return *new TransformEquilibrium (al); }
  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    bool ok = true;

    if ((!al.check ("k_AB") || al.number_sequence ("k_AB").size () < 1)
	&& !al.check ("pedo_AB"))
      {
	err.entry ("You must specify either 'k_AB' or 'pedo_AB'");
	ok = false;
      }
    return ok;
  }
  TransformEquilibriumSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    Transform::load_syntax (syntax, alist);

    alist.add ("description", 
	       "Two soil chemicals reching for equilibrium.");
    syntax.add ("A", Syntax::String, Syntax::Const,
		"Name of first soil chemical in equilibrium.");
    syntax.add ("B", Syntax::String, Syntax::Const,
		"Name of second soil chemical in equilibrium.");
    syntax.add ("equilibrium", Librarian<Equilibrium>::library (),
		"Function for calculating equilibrioum between A and B.");
    syntax.add ("k_AB", "h^-1", Syntax::OptionalConst, Syntax::Sequence,
		"Tranformation rate from soil chemical 'A' to 'B'.");
    syntax.add ("pedo_AB", Librarian<Pedotransfer>::library (),
		Syntax::OptionalConst, Syntax::Singleton, 
		"Function to calculate 'k_AB' if not specified.");
    syntax.add ("k_BA", "h^-1", Syntax::OptionalConst, Syntax::Sequence,
		"Tranformation rate from soil chemical 'B' to 'A'.");
    syntax.add ("pedo_BA", Librarian<Pedotransfer>::library (),
		Syntax::OptionalConst, Syntax::Singleton,
		"Function to calculate 'k_BA' if not specified.\n\
If neither this, nor 'k_BA' are specified, 'k_AB' will be used.");
    syntax.add ("S_AB", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Converted from A to B this timestep (may be negative).");

    syntax.order ("A", "B");
    Librarian<Transform>::add_type ("equilibrium", alist, syntax, &make);
  }
} TransformEquilibrium_syntax;
