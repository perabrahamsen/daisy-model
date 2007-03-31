// reaction_std.C -- Transformation between two soil chemicals.
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


#include "reaction.h"
#include "block.h"
#include "transform.h"
#include "chemistry.h"
#include "chemical.h"
#include "soil.h"
#include "log.h"
#include "assertion.h"
#include <memory>

struct ReactionStandard : public Reaction
{
  // Parameters.
  const symbol name_A;
  const symbol name_B;
  const std::auto_ptr<Transform> transform;
  
  // Output.
  std::vector<double> S_AB;
  void output (Log& log) const
  { output_variable (S_AB, log); }

  // Simulation.
  void tick (const Soil& soil, const SoilWater& soil_water, 
             Chemistry& chemistry, const double dt, Treelog& msg)
  { 
    Solute& A = chemistry.find (name_A);
    Solute& B = chemistry.find (name_B);
    transform->tick (soil, soil_water, A.M (), B.M (), S_AB, msg);
    A.add_to_sink (S_AB, dt);
    B.add_to_source (S_AB, dt);
  }

  // Create.
  bool check (const Soil& soil, const Chemistry& chemistry, Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (name_A))
      {
        msg.error ("'" + name_A.name () + "' not traced");
        ok = false;
      }
    if (!chemistry.know (name_B))
      {
        msg.error ("'" + name_B.name () + "' not traced");
        ok = false;
      }
    if (!transform->check (soil, msg))
      ok = false;

    return ok;
  }
  void initialize (Block& block, const Soil& soil)
  { 
    transform->initialize (block, soil); 
    S_AB.insert (S_AB.begin (), soil.size (), 0.0);
    daisy_assert (S_AB.size () == soil.size ());
  }
  explicit ReactionStandard (Block& al)
    : Reaction (al),
      name_A (al.identifier ("A")),
      name_B (al.identifier ("B")),
      transform (BuildBase::build_item<Transform> (al, "transform"))
  { }
};

static struct ReactionStandardSyntax
{
  static Model& make (Block& al)
  { return *new ReactionStandard (al); }
  ReactionStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Transformation between two soil chemicals.");
    syntax.add_object ("transform", Transform::component,
                       "Tranformation process between 'A' to 'B'.");
    syntax.add ("A", Syntax::String, Syntax::Const,
		"Name of first soil component in equilibrium.");
    syntax.add ("B", Syntax::String, Syntax::Const,
		"Name of second soil component in equilibrium.");
    syntax.add ("S_AB", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Converted from A to B this timestep (may be negative).");

    BuildBase::add_type (Reaction::component, "default", alist, syntax, &make);
  }
} ReactionStandard_syntax;
