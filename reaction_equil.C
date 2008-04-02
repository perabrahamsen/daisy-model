// reaction_equil.C -- Equilibrium between two soil chemicals.
// 
// Copyright 2004, 2007 Per Abrahamsen and KVL.
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

#include "reaction.h"
#include "block.h"
#include "number.h"
#include "equil.h"
#include "chemistry.h"
#include "chemical.h"
#include "geometry.h"
#include "soil.h"
#include "scope_soil.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "mathlib.h"
#include <memory>

struct ReactionEquilibrium : public Reaction
{
  static const symbol k_unit;
  
  // Parameters.
  const symbol name_A;
  const symbol name_B;
  const std::auto_ptr<Equilibrium> equilibrium;
  const std::auto_ptr<Number> k_AB;
  const std::auto_ptr<Number> k_BA;
  
  // Output.
  std::vector<double> S_AB;
  void output (Log& log) const
  { output_variable (S_AB, log); }

  // Simulation.
  void tick (const Geometry& geo, 
	     const Soil& soil, const SoilWater& soil_water, 
	     const SoilHeat& soil_heat, const OrganicMatter&,
             Chemistry& chemistry, const double dt, Treelog& msg)
  { 
    const size_t cell_size = geo.cell_size ();
    Chemical& A = chemistry.find (name_A);
    Chemical& B = chemistry.find (name_B);
    
    ScopeSoil scope (soil, soil_water, soil_heat);
    for (size_t c = 0; c < cell_size; c++)
      { 
	scope.set_cell (c);
	const double has_A = A.M_immobile (c);
	const double has_B = B.M_immobile (c);
	double want_A;
	double want_B;
	equilibrium->find (scope, has_A, has_B, want_A, want_B,
			   msg);
	daisy_assert (approximate (has_A + has_B, want_A + want_B));
	
	double convert = 0.0;

	if (has_A > want_A)
	  {
	    if (!k_AB->tick_value (convert, k_unit, scope, msg))
	      msg.error ("Could not evaluate k_AB");
	    
	    convert *= (has_A - want_A);
          }
	else
	  {
	    if (!k_BA->tick_value (convert, k_unit, scope, msg))
	      msg.error ("Could not evaluate k_BA");
	    
	    convert *= (has_B - want_B);
            convert *= -1.0;
	  }
      
      S_AB[c] = convert;
	
    }
    A.add_to_transform_sink (S_AB, dt);
    B.add_to_transform_source (S_AB, dt);
  }

  // Create.
  bool check (const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const Chemistry& chemistry, Treelog& msg) const
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
    ScopeSoil scope (soil, soil_water, soil_heat);
    if (!equilibrium->check (scope, msg))
      ok = false;
    if (!k_AB->check_dim (scope, k_unit, msg))
      ok = false;
    if (!k_BA->check_dim (scope, k_unit, msg))
      ok = false;

    return ok;
  }
  void initialize (const Soil& soil, Treelog& msg)
  { 
    equilibrium->initialize (msg); 
    S_AB.insert (S_AB.begin (), soil.size (), 0.0);
    daisy_assert (S_AB.size () == soil.size ());
    k_AB->initialize (msg); 
    k_BA->initialize (msg); 
  }
  explicit ReactionEquilibrium (Block& al)
    : Reaction (al),
      name_A (al.identifier ("A")),
      name_B (al.identifier ("B")),
      equilibrium (Librarian::build_item<Equilibrium> (al, "equilibrium")),
      k_AB (Librarian::build_item<Number> (al, "k_AB")),
      k_BA (al.check ("k_BA")
	    ? Librarian::build_item<Number> (al, "k_BA")
	    : Librarian::build_item<Number> (al, "k_AB"))
  { }
};

const symbol
ReactionEquilibrium::k_unit ("h^-1");

static struct ReactionEquilibriumSyntax
{
  static Model& make (Block& al)
  { return *new ReactionEquilibrium (al); }
  ReactionEquilibriumSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Equilibrium between two soil chemicals.");
    syntax.add ("A", Syntax::String, Syntax::Const,
		"Name of first soil component in equilibrium.");
    syntax.add ("B", Syntax::String, Syntax::Const,
		"Name of second soil component in equilibrium.");
    syntax.add_object ("equilibrium", Equilibrium::component,
                       "Function for calculating equilibrium between A and B.");
    syntax.add_object ("k_AB", Number::component,
                       Syntax::Const, Syntax::Singleton, 
                       "Tranformation rate from soil component 'A' to 'B'.");
    syntax.add_object ("k_BA", Number::component,
                       Syntax::OptionalConst, Syntax::Singleton,
                       "Tranformation rate from soil component 'B' to 'A'.\n\
By default, this is identical to 'k_AB'.");
    syntax.add ("S_AB", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Converted from A to B this timestep (may be negative).");

    Librarian::add_type (Reaction::component, "equilibrium",
			 alist, syntax, &make);
  }
} ReactionEquilibrium_syntax;

// reaction_equil.C ends here.
