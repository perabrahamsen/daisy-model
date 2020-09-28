// reaction_sorption.C -- Kinetic linear sorption equilibrium.
// 
// Copyright 2004, 2007 Per Abrahamsen and KVL.
// Copyright 2010 KU.
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
#include "chemistry.h"
#include "chemical.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "surface.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "mathlib.h"
#include "treelog.h"
#include "check.h"
#include "frame.h"
#include <memory>
#include <sstream>

static const double c_fraction_in_humus = 0.587;

struct ReactionSorption : public Reaction
{
  static const symbol k_unit;

  // Parameters.
  const symbol name_solute;
  const symbol name_sorbed;
  const double K_d;
  const double K_clay;
  const double K_humus;

  const double k_sorption;
  const double k_desorption;
  const symbol name_colloid;
  const double soil_enrichment_factor;

  // Output.
  double surface_sorption;       // [g/cm^2/h]
  std::vector<double> S_sorption;
  std::vector<double> S_sorption_primary;
  std::vector<double> S_sorption_secondary;
  void output (Log& log) const
  { 
    output_variable (surface_sorption, log); 
    output_variable (S_sorption, log); 
    output_variable (S_sorption_primary, log); 
    output_variable (S_sorption_secondary, log); 
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
    Chemical& solute = chemistry.find (name_solute);
    Chemical& sorbed = chemistry.find (name_sorbed);
    const Chemical *const colloid 
      =  (name_colloid == Attribute::None ())
      ? NULL
      : &chemistry.find (name_colloid);
    
    // Clear source/sink term.
    // Soil.
    const size_t cell_size = S_sorption.size ();
    for (size_t c = 0; c < cell_size; c++)
      { 
        const double clay = soil.clay (c);
        const double humus = soil.humus (c);
        const double rho_f1 = soil.primary_sorption_fraction (c);
        
        // Primary domain.
        {
          const double Theta_old = soil_water.Theta_primary_old (c);
          daisy_assert (Theta_old > 0.0);
          const double rho_b = colloid
            ? colloid->M_primary (c) * soil_enrichment_factor
            : soil.dry_bulk_density (c);

          const double C = solute.C_primary (c);
	  const double A = sorbed.M_primary (c);
          const double S
	    = find_rate (rho_f1, rho_b, clay, humus, Theta_old, A, C);
          S_sorption_primary[c] = S;
          S_sorption[c] = S;
        }
        // Secondary domain.
        {
          const double Theta_old = soil_water.Theta_secondary_old (c);
          const double Theta_new = soil_water.Theta_secondary (c);
          if (Theta_old > 1e-9 && Theta_new > 1e-9)
            {
              const double rho_b = colloid
                ? colloid->M_secondary (c)  * soil_enrichment_factor
                : soil.dry_bulk_density (c);
              const double C = solute.C_secondary (c);
              const double A = sorbed.M_secondary (c);
              const double S = find_rate (1.0 - rho_f1, 
					  rho_b, clay, humus, Theta_old, 
                                              A, C);
              S_sorption_secondary[c] = S;
              S_sorption[c] += S;
            }
          else 
            S_sorption_secondary[c] = 0.0;
        }
      }
    // Make the source/sink official.
    solute.add_to_transform_sink (S_sorption_primary);
    sorbed.add_to_transform_source (S_sorption_primary);
    solute.add_to_transform_sink_secondary (S_sorption_secondary);
    sorbed.add_to_transform_source_secondary (S_sorption_secondary);
  }

  void tick_surface (const Geometry& geo, 
                     const Soil& soil, const SoilWater& soil_water, 
                     const SoilHeat& , const Surface& surf,
                     OrganicMatter&, Chemistry& chemistry,
		     const double /* dt */, Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    const double m2_per_cm2 = 0.01 * 0.01 ; // [m^2/cm^2]
    const double pond = std::max (surf.ponding_average (), 0.0);
    const double z_mixing = surf.mixing_depth ();

    // Find solute 
    Chemical& solute = chemistry.find (name_solute);
    const double has_solute = solute.surface_storage_amount ()
      * m2_per_cm2 / z_mixing; // [g/cm^3]
    daisy_assert (has_solute >= 0.0);

    // Find sorbed
    Chemical& sorbed = chemistry.find (name_sorbed);
    const double has_sorbed = sorbed.surface_storage_amount ()
      * m2_per_cm2 / z_mixing; // [g/cm^3]
    daisy_assert (has_sorbed >= 0.0);

    // Find water
    const double Theta_soil
      = geo.content_cell_or_hood (soil_water, 
                                  &SoilWater::Theta_old, Geometry::cell_above);
    const double Theta_pond = std::max (pond, 0.0) / z_mixing; // []
    daisy_assert (Theta_pond >= 0.0);
    const double Theta = Theta_soil + Theta_pond;
    daisy_assert (Theta > 0.0);
    
    // Find soil particles.
    double rho_b;
    if (name_colloid != Attribute::None ())
      {
        const Chemical& colloid = chemistry.find (name_colloid);
        rho_b = colloid.surface_storage_amount () * soil_enrichment_factor
          * m2_per_cm2 / z_mixing; // [g/cm^3]
      }
    else
      rho_b = geo.content_cell_or_hood (soil, &Soil::dry_bulk_density, 
                                        Geometry::cell_above);
    daisy_assert (rho_b >= 0.0);

    // Find humus and clay.
    const double clay 
      = geo.content_cell_or_hood (soil, &Soil::clay, Geometry::cell_above);
    const double humus
      = geo.content_cell_or_hood (soil, &Soil::humus, Geometry::cell_above);
    
    // Find source/sink term.
    const double A = has_sorbed;
    const double C = has_solute / Theta;
    surface_sorption 
      = find_rate (1.0, rho_b, clay, humus, Theta, A, C) * z_mixing;
    solute.add_to_surface_transform_source (-surface_sorption);
    sorbed.add_to_surface_transform_source (surface_sorption);
  }

  double find_rate (const double f, const double rho_b, 
                    const double clay, const double humus, const double Theta, 
                    const double A, const double C)
  {
    daisy_assert (Theta > 0.0);
    daisy_assert (f >= 0.0);
    daisy_assert (f <= 1.0);
    const double has_sorbed = A;
    const double has_solute = C * Theta;

    const double M = has_solute + has_sorbed;
    const double K = (K_d > 0.0)
      ? K_d : clay * K_clay + humus * K_humus;
    // M = C (K rho_b + Theta) => C = M / (K rho_b + Theta)
    const double equil_C = M / (f * K * rho_b + Theta);
    const double want_solute = equil_C * Theta;
    daisy_assert (want_solute >= 0.0);
    const double want_sorbed = M - want_solute;
    daisy_assert (std::isfinite (want_sorbed));
    if (want_sorbed < 0.0)
      {
        if (!approximate (M, want_solute))
          {
            std::ostringstream tmp;
            tmp << "want_sorbed = " << want_sorbed
                << ", has_sorbed = " << has_sorbed
                << ", f = " << f
                << ", rho_b = " << rho_b
                << ", clay = " << clay
                << ", humus = " << humus
                << ", Theta = " << Theta
                << ", A = " << A
                << ", C = " << C
                << ", equil_C = " << equil_C
                << ", has_solute = " << has_solute
                << ", want_solute = " << want_solute
                << ", M = " << M
                << ", K = " << K;
            daisy_bug (tmp.str ());
          }
      }
    return (has_solute > want_solute)
      ? k_sorption * (has_solute - want_solute)
      : k_desorption * (has_solute - want_solute);
  }


  // Create.
  bool check (const Geometry&, 
              const Soil&, const SoilWater&, 
              const SoilHeat&,
              const OrganicMatter&, const Chemistry& chemistry,
	      Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    bool ok = true;
    if (!chemistry.know (name_solute))
      {
        msg.error ("'" + name_solute + "' not traced");
        ok = false;
      }
    if (!chemistry.know (name_sorbed))
      {
        msg.error ("'" + name_sorbed + "' not traced");
        ok = false;
      }
    if (name_colloid != Attribute::None ()
        && !chemistry.know (name_colloid))
      {
        msg.error ("'" + name_colloid + "' not traced");
        ok = false;
      }
    return ok;
  }
  void initialize (const Geometry& geo, const Soil&, 
                   const SoilWater&, const SoilHeat&,
                   const OrganicMatter&, const Surface&, Treelog& msg)
  { 
    TREELOG_MODEL (msg);
    const size_t cell_size = geo.cell_size ();
    S_sorption.insert (S_sorption.begin (), cell_size, 0.0);
    S_sorption_primary.insert (S_sorption_primary.begin (), cell_size, 0.0);
    S_sorption_secondary.insert (S_sorption_secondary.begin (), cell_size, 0.0);
    daisy_assert (S_sorption.size () == cell_size);
    daisy_assert (S_sorption_primary.size () == cell_size);
    daisy_assert (S_sorption_secondary.size () == cell_size);
  }
  explicit ReactionSorption (const BlockModel& al)
    : Reaction (al),
      name_solute (al.name ("solute")),
      name_sorbed (al.name ("sorbed")),
      K_d (al.number ("K_d", -42.42e42)),
      K_clay (al.number ("K_clay", 0.0)),
      K_humus (al.number ("K_OC", K_clay) * c_fraction_in_humus),
      k_sorption (al.number ("k_sorption")),
      k_desorption (al.number ("k_desorption", k_sorption)),
      name_colloid (al.name ("colloid", Attribute::None ())),
      soil_enrichment_factor (al.number ("soil_enrichment_factor")),
      surface_sorption (0.0)
  { }
};

const symbol
ReactionSorption::k_unit ("h^-1");

static struct ReactionSorptionSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionSorption (al); }
  ReactionSorptionSyntax ()
    : DeclareModel (Reaction::component, "sorption", 
                    "Kinetic linear sorption equilibrium.\n\
Faster than the 'equilibrium' reaction model, more flexible than the\n\
'adsorption' reaction model.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;

    const bool has_K_d = al.check ("K_d");
    const bool has_K_clay = al.check ("K_clay");
    const bool has_K_OC = al.check ("K_OC");
      
    if (!has_K_d && !has_K_clay && !has_K_OC)
      {
	msg.error ("You must specify either 'K_d', 'K_clay' or 'K_OC'");
	ok = false;
      }
    if (has_K_d && (has_K_clay || has_K_OC))
      {
	msg.error ("You cannot specify 'K_d' with 'K_clay' or 'K_OC'");
	ok = false;
      }
    return ok;
  }

  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_string ("solute", Attribute::Const,
                          "Name of solute form of chemical.\n\
If this chemical has equilibrium sorption, only the colute part is used.");
    frame.declare_string ("sorbed", Attribute::Const,
                          "Name of sorbed form of chemical.");
    frame.declare ("K_d", "cm^3/g", Check::non_negative (), 
                   Attribute::OptionalConst, "\
Equillibrium parameter: M = C (K_d rho_b + Theta)\n\
Here M is the total amount, C is solute concentration, K_d is this\n\
parameter, rho_b is the dry bulk density, and Theta is the volumetric\n\
water content.  By default, K_d is calculated from K_clay and K_OC.");
    frame.declare ("K_clay", "cm^3/g", Check::non_negative (), 
		Attribute::OptionalConst, 
		"Clay dependent distribution parameter.\n\
It is multiplied with the soil clay fraction to get the clay part of\n\
the 'K_d' factor.  If 'K_OC' is specified, 'K_clay' defaults to 0.");
    frame.declare ("K_OC", "cm^3/g", Check::non_negative (), 
		Attribute::OptionalConst, 
		"Humus dependent distribution parameter.\n\
It is multiplied with the soil organic carbon fraction to get the\n\
carbon part of the 'K_d' factor.  By default, 'K_OC' is equal to 'K_clay'.");
    frame.declare ("k_sorption", "h^-1",
                   Check::non_negative (), Attribute::Const, "\
Sorption rate.");
    frame.declare ("k_desorption", "h^-1", 
                   Check::non_negative (), Attribute::OptionalConst, "\
Desorption rate.\n\
By default, this is identical to 'k_sorption'.");
    frame.declare ("surface_sorption", "g/cm^2/h", Attribute::LogOnly, "\
Sorption on surface this timestep (may be negative).");
    frame.declare ("S_sorption", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells, "\
Sorption in soil this timestep (may be negative).");
    frame.declare ("S_sorption_primary", "g/cm^3/h", 
                   Attribute::LogOnly, Attribute::SoilCells, "\
Sorption in primary domain this timestep (may be negative).");
    frame.declare ("S_sorption_secondary", "g/cm^3/h",
                   Attribute::LogOnly, Attribute::SoilCells, "\
Sorption in secondary domain this timestep (may be negative).");
    frame.declare_string ("colloid", Attribute::OptionalConst, "\
Sorp to this chemical instead of to the soil matrix.");
    frame.declare ("soil_enrichment_factor", 
                   Attribute::None (), Check::positive (), Attribute::Const, "\
Multiply K_d with this number if 'colloid' is set.\n\
This represents how much more accesible colloids is compared to\n\
the soil matrix.");
    frame.set ("soil_enrichment_factor", 1.0);
  }
} ReactionSorption_syntax;

// reaction_sorption.C ends here.
