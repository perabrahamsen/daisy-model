// chemistry_std.C -- Transformation between two soil chemicals.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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


#include "chemistry.h"
#include "transform.h"
#include "soil_chemicals.h"
#include "soil.h"
#include "log.h"
#include <memory>

using namespace std;

struct ChemistryStandard : public Chemistry
{
  // Parameters.
  const symbol name_A;
  const symbol name_B;
  const auto_ptr<Transform> transform;
  
  // Output.
  vector<double> S_AB;
  void output (Log& log) const
  { output_variable (S_AB, log); }

  // Simulation.
  void tick (const Soil& soil, const SoilWater& soil_water, 
             SoilChemicals& soil_chemicals, Treelog& msg)
  { 
    SoilChemical& A = soil_chemicals.find (soil, soil_water, name_A, msg);
    SoilChemical& B = soil_chemicals.find (soil, soil_water, name_B, msg);
    transform->tick (soil, soil_water, A.M (), B.M (), S_AB, msg);
    A.add_to_sink (S_AB);
    B.add_to_source (S_AB);
  }

  // Create.
  bool check (const Soil& soil, Treelog& msg) const
  { return transform->check (soil, msg); }
  void initialize (const Soil& soil, Treelog& msg)
  { 
    transform->initialize (soil, msg); 
    S_AB.insert (S_AB.begin (), soil.size (), 0.0);
    daisy_assert (S_AB.size () == soil.size ());
  }
  ChemistryStandard (const AttributeList& al)
    : Chemistry (al),
      name_A (al.identifier ("A")),
      name_B (al.identifier ("B")),
      transform (Librarian<Transform>::create (al.alist ("transform")))
  { }
};

static struct ChemistryStandardSyntax
{
  static Chemistry& make (const AttributeList& al)
  { return *new ChemistryStandard (al); }
  ChemistryStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Chemistry::load_syntax (syntax, alist);

    alist.add ("description", 
	       "Transformation between two soil chemicals.");
    syntax.add ("transform", Librarian<Transform>::library (),
		"Tranformation process between 'A' to 'B'.");
    syntax.add ("A", Syntax::String, Syntax::Const,
		"Name of first soil component in equilibrium.");
    syntax.add ("B", Syntax::String, Syntax::Const,
		"Name of second soil component in equilibrium.");
    syntax.add ("S_AB", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Converted from A to B this timestep (may be negative).");

    Librarian<Chemistry>::add_type ("default", alist, syntax, &make);
  }
} ChemistryStandard_syntax;
