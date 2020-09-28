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
#include "block_model.h"
#include "number.h"
#include "equil.h"
#include "chemistry.h"
#include "chemical.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "scope_soil.h"
#include "surface.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "mathlib.h"
#include "treelog.h"
#include "scope_id.h"
#include "scope_multi.h"
#include "vcheck.h"
#include "frame.h"
#include <memory>

struct ReactionEquilibrium : public Reaction
{
  static const symbol k_unit;
  const Units& units;

  // Parameters.
  const symbol name_A;
  const symbol name_B;
  const std::unique_ptr<Equilibrium> equilibrium;
  const std::unique_ptr<Number> k_AB;
  const std::unique_ptr<Number> k_BA;
  const symbol name_colloid;
  const bool primary;
  const bool secondary;
  const bool surface;

  // Output.
  double surface_AB;       // [g/cm^2/h]
  std::vector<double> S_AB;
  void output (Log& log) const
  { 
    output_variable (surface_AB, log); 
    output_variable (S_AB, log); 
  }

  // Simulation.
  void tick_soil (const Geometry& geo, 
                  const Soil& soil, const SoilWater& soil_water, 
                  const SoilHeat& soil_heat,
                  OrganicMatter&, Chemistry& chemistry,
		  const double /* dt */, Treelog& msg)
  { 
    TREELOG_MODEL (msg);

    // Find chemicals.
    Chemical& A = chemistry.find (name_A);
    Chemical& B = chemistry.find (name_B);
    const Chemical *const colloid 
      =  (name_colloid == Attribute::None ())
      ? NULL
      : &chemistry.find (name_colloid);
    
    // Set up scope.
    ScopeSoil scope (geo, soil, soil_water, soil_heat);
    scope.set_old_water (true); // Theta at start of timestep.

    // Clear source/sink term.
    std::fill (S_AB.begin (), S_AB.end (), 0.0);

    if (primary)
      // Primary soil domain (intra-aggregate pores).
      tick_domain (units, scope, colloid, ScopeSoil::primary, A, B, msg);

    if (secondary)
      // Secondary soil domain (inter-aggregate pores).
      tick_domain (units, scope, colloid, ScopeSoil::secondary, A, B, msg);

    // Make the source/sink official.
    A.add_to_transform_sink (S_AB);
    B.add_to_transform_source (S_AB);
  }

  void tick_domain (const Units& units, ScopeSoil& scope,
                    const Chemical *const colloid, ScopeSoil::domain_t domain,
                    const Chemical& A, const Chemical& B, Treelog& msg)
  { 
    TREELOG_MODEL (msg);

    scope.set_domain (domain);

    const size_t cell_size = S_AB.size ();
    for (size_t c = 0; c < cell_size; c++)
      { 
        scope.set_cell (c);
        if (colloid)
          {
            if (domain == ScopeSoil::primary)
              scope.set_dry_bulk_density (colloid->M_primary (c));
            else
              {
                daisy_assert (domain == ScopeSoil::secondary);
                const double M_total = colloid->M_total (c);
                const double M_primary = colloid->M_primary (c);
                const double M_secondary = M_total - M_primary;
                scope.set_dry_bulk_density (M_secondary);
              }
          }
                                      
        // What we have.
        const double has_A = A.M_primary (c);
        const double has_B = B.M_primary (c);

        S_AB[c] += find_rate (units, scope, c, has_A, has_B, msg);
      }
  }

  double find_rate (const Units& units, ScopeSoil& scope, const int cell,
                    const double has_A, const double has_B, Treelog& msg) const
   {
    // What we want.
    double want_A;
    double want_B;
    equilibrium->find (units, scope, cell, has_A, has_B, want_A, want_B,
                       msg);
    daisy_approximate (has_A + has_B, want_A + want_B);

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

    return convert;
   }

  void tick_surface (const Geometry& geo, 
                     const Soil& soil, const SoilWater& soil_water, 
                     const SoilHeat& soil_heat, const Surface& surf,
                     OrganicMatter&, Chemistry& chemistry,
		     const double dt, Treelog& msg)
  { 
    if (!surface)
      // Nothing to do.
      return;

    TREELOG_MODEL (msg);

    const double m2_per_cm2 = 0.01 * 0.01 ; // [m^2/cm^2]
    const double pond = std::max (surf.ponding_average (), 0.0);
    const double z_mixing = surf.mixing_depth ();
    // Find A 
    Chemical& A = chemistry.find (name_A);
    const double has_A = A.surface_storage_amount ()
      * m2_per_cm2 / z_mixing; // [g/cm^3]
    daisy_assert (has_A >= 0.0);

    // Find B
    Chemical& B = chemistry.find (name_B);
    const double has_B = B.surface_storage_amount ()
      * m2_per_cm2 / z_mixing; // [g/cm^3]
    daisy_assert (has_B >= 0.0);

    // Set up scope.
    ScopeSoil scope (geo, soil, soil_water, soil_heat);
    scope.set_old_water (true); // Theta at start of timestep.
    const double Theta_pond = std::max (pond, 0.0) / z_mixing; // []
    daisy_assert (Theta_pond >= 0.0);
    scope.set_extra_water (Theta_pond);
    if (name_colloid != Attribute::None ())
      {
        const Chemical& colloid = chemistry.find (name_colloid);
        const double rho_b = colloid.surface_storage_amount ()
          * m2_per_cm2 / z_mixing; // [g/cm^3]
        daisy_assert (rho_b >= 0.0);
        scope.set_dry_bulk_density (rho_b);
      }
    scope.set_domain (ScopeSoil::matrix);
    scope.set_cell (Geometry::cell_above);

    // Find source/sink term.
    surface_AB = find_rate (units, scope, Geometry::cell_above,
                            has_A, has_B, msg) * z_mixing; // [g/cm^2/h]
    A.add_to_surface_transform_source (-surface_AB);
    B.add_to_surface_transform_source (surface_AB);
  }

  // Create.
  bool check (const Geometry& geo, 
              const Soil& soil, const SoilWater& soil_water, 
              const SoilHeat& soil_heat,
              const OrganicMatter&,
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
    if (name_colloid != Attribute::None ()
        && !chemistry.know (name_colloid))
      {
        msg.error ("'" + name_colloid + "' not traced");
        ok = false;
      }

    const size_t cell_size = geo.cell_size ();
    ScopeSoil scope (geo, soil, soil_water, soil_heat);
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
  void initialize (const Geometry& geo, const Soil& soil, 
                   const SoilWater& soil_water, const SoilHeat& soil_heat,
                   const OrganicMatter&, const Surface&, Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    ScopeSoil scope (geo, soil, soil_water, soil_heat);
    scope.set_cell (0);
    equilibrium->initialize (units, scope, msg); 
    const size_t cell_size = geo.cell_size ();
    S_AB.insert (S_AB.begin (), cell_size, 0.0);
    daisy_assert (S_AB.size () == cell_size);
    k_AB->initialize (units, scope, msg); 
    k_BA->initialize (units, scope, msg); 
  }
  explicit ReactionEquilibrium (const BlockModel& al)
    : Reaction (al),
      units (al.units ()),
      name_A (al.name ("A")),
      name_B (al.name ("B")),
      equilibrium (Librarian::build_item<Equilibrium> (al, "equilibrium")),
      k_AB (Librarian::build_item<Number> (al, "k_AB")),
      k_BA (al.check ("k_BA")
            ? Librarian::build_item<Number> (al, "k_BA")
            : Librarian::build_item<Number> (al, "k_AB")),
      name_colloid (al.name ("colloid", Attribute::None ())),
      primary (al.flag ("primary", !al.flag ("secondary"))),
      secondary (al.flag ("secondary")),
      surface (al.flag ("surface")),
      surface_AB (0.0)
  { }
};

const symbol
ReactionEquilibrium::k_unit ("h^-1");

static struct ReactionEquilibriumSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionEquilibrium (al); }
  ReactionEquilibriumSyntax ()
    : DeclareModel (Reaction::component, "equilibrium", 
                    "Equilibrium between two soil chemicals.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_string ("A", Attribute::Const,
                   "Name of first soil component in equilibrium.");
    frame.declare_string ("B", Attribute::Const,
                   "Name of second soil component in equilibrium.");
    frame.declare_object ("equilibrium", Equilibrium::component,
                          "Function for calculating equilibrium between A and B.");
    frame.declare_object ("k_AB", Number::component,
                          Attribute::Const, Attribute::Singleton, 
                          "Tranformation rate from soil component 'A' to 'B'.");
    frame.declare_object ("k_BA", Number::component,
                          Attribute::OptionalConst, Attribute::Singleton,
                          "Tranformation rate from soil component 'B' to 'A'.\n\
By default, this is identical to 'k_AB'.");
    frame.declare ("surface_AB", "g/cm^2/h", Attribute::LogOnly, "\
Converted from A to B on surface this timestep (may be negative).");
    frame.declare ("S_AB", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, "\
Converted from A to B in soil this timestep (may be negative).");
    frame.declare_string ("colloid", Attribute::OptionalConst,
                   "Let 'rho_b' denote content of specified chemical.\n\
This might affect the evaluation of the 'k_AB' and 'k_BA' parameter\n\
expressions, as well as the 'equilibrium' model.\n\
By default, 'rho_b' will be the soil dry bulk density.");
    frame.declare_boolean ("primary", Attribute::OptionalConst,
                   "Equilibrium should happen in the primary domain.\n\
If true, the content of the primary soil domain (soil-bound and\n\
intra-aggregate pores), will be included in the reaction.\n\
By default, this will be true if 'secondary' is false, and be false if\n\
'secondary' is true.");
    frame.declare_boolean ("secondary", Attribute::Const,
                   "Equilibrium should happen in the secondary domain.\n\
There will only be a reaction when there is water in the secondary domain\n\
(inter-aggregate pores), at both the beginning and end of the timestep.\n\
By default, only the content of the primary domain (soil-bound and\n\
intra-aggregate pores), will be included in the reaction.\n\
There is no way to use this model to specify an equilibrium reaction in\n\
the tertiary domain (biopores).");
    frame.set ("secondary", false);
    frame.declare_boolean ("surface", Attribute::Const,
                   "Equilibrium should happen in the surface.");
    frame.set ("surface", false);
  }
} ReactionEquilibrium_syntax;

// reaction_equil.C ends here.
