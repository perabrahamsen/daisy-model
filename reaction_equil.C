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
#include "soil_water.h"
#include "scope_soil.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "mathlib.h"
#include "treelog.h"
#include "scope_id.h"
#include "scope_multi.h"
#include "vcheck.h"
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
  const symbol name_colloid;
  const bool secondary;

  // Output.
  std::vector<double> S_AB;
  void output (Log& log) const
  { output_variable (S_AB, log); }

  // Simulation.
  void tick (const Units& units, const Geometry& geo, 
	     const Soil& soil, const SoilWater& soil_water, 
	     const SoilHeat& soil_heat, const OrganicMatter&,
             Chemistry& chemistry, const double dt, Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    const size_t cell_size = geo.cell_size ();
    Chemical& A = chemistry.find (name_A);
    Chemical& B = chemistry.find (name_B);
    const Chemical *const colloid = (name_colloid == Value::None ())
      ? NULL
      : &chemistry.find (name_colloid);
    
    ScopeSoil scope_soil (soil, soil_water, soil_heat);
    scope_soil.set_old_water (true); // Theta at start of timestep.
    scope_soil.set_domain (secondary
                           ? ScopeSoil::secondary 
                           : ScopeSoil::primary);
    ScopeID scope_id (ScopeSoil::rho_b, ScopeSoil::rho_b_unit);
    if (secondary && !colloid)
      // No soil in secondary domain.
      scope_id.add (ScopeSoil::rho_b, 0.0);
    ScopeMulti scope (scope_id, scope_soil);
    for (size_t c = 0; c < cell_size; c++)
      { 
	scope_soil.set_cell (c);
        double has_A;
        double has_B;

        if (secondary)
          {
            const double Theta_old = soil_water.Theta_secondary_old (c);
            
            if (Theta_old < 1e-4 || soil_water.Theta_secondary (c) < 1e-4)
              // Nothing to do.
              {
                S_AB[c] = 0.0;
                continue;
              }
            if (colloid)
              scope_id.add (ScopeSoil::rho_b, 
                            colloid->C_secondary (c) * Theta_old);
            has_A = A.C_secondary (c) * Theta_old;
            has_B = B.C_secondary (c) * Theta_old;
          }
        else
          {
            if (colloid)
              scope_id.add (ScopeSoil::rho_b, colloid->M_primary (c));
            else
              scope_id.add (ScopeSoil::rho_b, soil.dry_bulk_density (c));

            has_A = A.M_primary (c);
            has_B = B.M_primary (c);
          }

	double want_A;
	double want_B;
	equilibrium->find (units, scope, has_A, has_B, want_A, want_B,
			   msg);
	daisy_assert (approximate (has_A + has_B, want_A + want_B));
	
	double convert = 0.0;

	if (has_A > want_A)
	  {
	    if (!k_AB->tick_value (units, convert, k_unit, scope, msg))
	      msg.error ("Could not evaluate k_AB");
	    
	    convert *= (has_A - want_A);
          }
	else
	  {
	    if (!k_BA->tick_value (units, convert, k_unit, scope, msg))
	      msg.error ("Could not evaluate k_BA");
	    
	    convert *= (has_B - want_B);
            convert *= -1.0;
	  }
      
      S_AB[c] = convert;
	
    }
    A.add_to_transform_sink (S_AB);
    B.add_to_transform_source (S_AB);
  }

  // Create.
  bool check (const Units& units, const Geometry& geo, 
              const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const Chemistry& chemistry, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    bool ok = true;
    if (!chemistry.know (name_A))
      {
        msg.error ("'" + name_A + "' not traced");
        ok = false;
      }
    if (!chemistry.know (name_B))
      {
        msg.error ("'" + name_B + "' not traced");
        ok = false;
      }
    if (name_colloid != Value::None ()
        && !chemistry.know (name_colloid))
      {
        msg.error ("'" + name_colloid + "' not traced");
        ok = false;
      }

    const size_t cell_size = geo.cell_size ();
    ScopeSoil scope (soil, soil_water, soil_heat);
    for (size_t c = 0; c < cell_size && ok; c++)
      {
        scope.set_cell (c);
        if (!equilibrium->check (units, scope, msg))
          ok = false;
        if (!k_AB->check_dim (units, scope, k_unit, msg))
          ok = false;
        if (!k_BA->check_dim (units, scope, k_unit, msg))
          ok = false;
      }
    return ok;
  }
  void initialize (const Units& units, const Geometry&, const Soil& soil, 
                   const SoilWater& soil_water, const SoilHeat& soil_heat,
                   Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    ScopeSoil scope (soil, soil_water, soil_heat);
    scope.set_cell (0);
    equilibrium->initialize (units, scope, msg); 
    S_AB.insert (S_AB.begin (), soil.size (), 0.0);
    daisy_assert (S_AB.size () == soil.size ());
    k_AB->initialize (units, scope, msg); 
    k_BA->initialize (units, scope, msg); 
  }
  explicit ReactionEquilibrium (Block& al)
    : Reaction (al),
      name_A (al.name ("A")),
      name_B (al.name ("B")),
      equilibrium (Librarian::build_item<Equilibrium> (al, "equilibrium")),
      k_AB (Librarian::build_item<Number> (al, "k_AB")),
      k_BA (al.check ("k_BA")
	    ? Librarian::build_item<Number> (al, "k_BA")
	    : Librarian::build_item<Number> (al, "k_AB")),
      name_colloid (al.name ("colloid", Value::None ())),
      secondary (al.flag ("secondary"))
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
    syntax.add ("A", Value::String, Value::Const,
		"Name of first soil component in equilibrium.");
    syntax.add ("B", Value::String, Value::Const,
		"Name of second soil component in equilibrium.");
    syntax.add_object ("equilibrium", Equilibrium::component,
                       "Function for calculating equilibrium between A and B.");
    syntax.add_object ("k_AB", Number::component,
                       Value::Const, Value::Singleton, 
                       "Tranformation rate from soil component 'A' to 'B'.");
    syntax.add_object ("k_BA", Number::component,
                       Value::OptionalConst, Value::Singleton,
                       "Tranformation rate from soil component 'B' to 'A'.\n\
By default, this is identical to 'k_AB'.");
    syntax.add ("S_AB", "g/cm^3/h", Value::LogOnly, Value::Sequence,
		"Converted from A to B this timestep (may be negative).");
    Librarian::add_type (Reaction::component, "equilibrium",
			 alist, syntax, &make);
    syntax.add ("colloid", Value::String, Value::OptionalConst,
		"Let 'rho_b' denote content of specified chemical.\n\
This miht affect the evaluation of the 'k_AB' and 'k_BA' parameter\n\
expressions, as well as the 'equilibrium' model.\n\
By default, 'rho_b' will be the soil dry bulk density.");
    syntax.add ("secondary", Value::Boolean, Value::Const,
                "Equilibrium should happen in the secondary domain.\n\
There will only be a reaction when there is water in the secondary domain\n\
(inter-aggregate pores), at both the beginning and end of the timestep.\n\
By default, only the content of the primary domain (soil-bound and\n\
intra-aggregate pores), will be included in the reaction.\n\
There is no way to use this model to specify an equilibrium reaction in\n\
the tertiary domain (biopores).");
    alist.add ("secondary", false);
  }
} ReactionEquilibrium_syntax;

// reaction_equil.C ends here.
