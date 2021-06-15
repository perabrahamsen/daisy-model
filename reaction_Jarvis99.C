// reaction_Jarvis99.C -- Colloids generation emulating the MACRO model.
// 
// Copyright 2008 Birgitte Gjettermann, Per Abrahamsen and KU
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "reaction_colgen.h"
#include "mathlib.h"
#include "check.h"
#include "block_model.h"
#include "librarian.h"
#include "chemistry.h"
#include "chemical.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "surface.h"
#include "treelog.h"
#include "frame.h"
#include "rainergy.h"
#include "plf.h"
#include "vegetation.h"
#include "bioclimate.h"
#include <sstream>
#include <memory>

struct ReactionJarvis99 : public ReactionColgen
{
  const Units& units;
  
  // Parameters.
  const std::unique_ptr<Rainergy> rainergy; // Energy in rain [J/cm^2/h]
  const bool tillage_replenish_all;       // Ms = Mmax after tillage.
  /* const */ double Mmax;            // Max colloid pool [g C/g S]
  const PLF Mmax_tillage_factor; // Modification factor for tillage age.
  const double kd;              // Depletion rate from pool [g S/J]
  const double kr;              // Replenishment rate to pool [g C/cm^2 S/h] 
  /* const */ double zi;        // Mixing layer thickness [cm S]

  // Initialized.
  /* const */ double rho_b;     // Dry bulk density [g S/cm^3 S]

  // State variable.
  double Ms;                    // Colloid pool in soil [g C/g S]

  // Log variable.
  double As;                    // Colloid pool in surface [g C/cm^2]
  double P;                     // Replenishment [g C/cm^2 S/h]
  double KE;                    // Energy for colloids [J/cm^2/h]
  double E;                     // Energy in rain [J/cm^2 S/mm W]

  // Simulation.
  void colloid_generation (const double tillage_age /* [d] */,
                           const double total_rain /* [mm/h] */, 
                           const double direct_rain /* [mm/h] */,
                           const double canopy_drip /* [mm/h] */,
                           const double canopy_height /* [m] */,
                           const double dt /* [h] */);
  void tick_top (const Vegetation& vegetation,
		 const Bioclimate& bioclimate,
		 const double tillage_age /* [d] */,
                 const double total_rain, 
                 const double h_pond,
                 OrganicMatter&,
		 Chemistry& chemistry, const double dt, Treelog&);
                           
  void output (Log& log) const;

  // Create and Destroy.
  void initialize (const Geometry& geo,
                   const Soil& soil, const SoilWater&, const SoilHeat&, 
                   const OrganicMatter&, const Surface&, Treelog&);
  bool check (const Geometry& geo,
              const Soil&, const SoilWater&, const SoilHeat&,
	      const OrganicMatter&, const Chemistry& chemistry,
	      Treelog& msg) const;
  ReactionJarvis99 (const BlockModel& al);
};

void
ReactionJarvis99::colloid_generation (const double tillage_age /* [d] */,
                                      const double total_rain /* [mm/h] */, 
                                      const double direct_rain /* [mm/h] */,
                                      const double canopy_drip /* [mm/h] */,
                                      const double canopy_height /* [m] */,
                                      const double dt /* [h] */)
{
  // [J/cm^2/h]
  KE = rainergy->value (total_rain , direct_rain, canopy_drip, canopy_height);
  daisy_assert (KE >= 0.0);

  // Kinetic energy of rain. [J cm^-2 mm^-1]
  E = (total_rain > 0) ? KE / total_rain : 0.0;
  
  // Detachment of colloids at the surface. [g cm^-2 h^-1]
  daisy_assert (KH >= 0.0);
  daisy_assert (KH <= 1.0);
  daisy_assert (Ms >= 0.0);
  D = std::min (kd * KE * KH * dt * Ms, As) / dt; 
  daisy_assert (D >= 0.0);

  // Fraction 
  if (D > 0.0)
    surface_release = dt * D / As;
  else
    surface_release = 0.0;

  // Replenishment of colloids in the surface layer.
  const double Mmax_t = Mmax * Mmax_tillage_factor (tillage_age);
  if (Ms >= Mmax_t)
    P = 0.0;                    // [g cm^-2 h^-1]
  else if (tillage_replenish_all && tillage_age < dt * 2)
    P = (Mmax_t - Ms) * (rho_b * zi) / dt; // [g cm^-2 h^-1]
  else
    P = kr * (1 - Ms / Mmax_t); // [g cm^-2 h^-1]
  
  // Pure forward mass balance.
  As += (-D + P) * dt;  //[g cm^-2]
  daisy_assert (As >= 0.0);
  Ms = As / (rho_b * zi); // [g C/g S]
}

void 
ReactionJarvis99::tick_top (const Vegetation& vegetation,
			    const Bioclimate& bioclimate,
			    const double tillage_age /* [d] */,
                            const double total_rain, 
                            const double h_pond,
                            OrganicMatter&, Chemistry& chemistry,
			    const double dt, Treelog&)
{
  const double exposed_soil = 1.0 - bioclimate.litter_cover ();
  const double direct_rain = bioclimate.direct_rain () * exposed_soil; // [mm/h]
  const double canopy_drip = bioclimate.canopy_leak () * exposed_soil; // [mm/h]
  const double h_veg = vegetation.height () * 0.01 ;	 // [m]

  ReactionColgen::tick_colgen (total_rain, h_pond);

  Chemical& colloid = chemistry.find (colloid_name);
  
  // Generate the colloids.
  colloid_generation (tillage_age, 
                      total_rain, direct_rain, canopy_drip, h_veg, dt);
  
  colloid.add_to_surface_transform_source (D);
  colloid.release_surface_colloids (surface_release);
}

void 
ReactionJarvis99::output (Log& log) const 
{
  ReactionColgen::output_colgen (log);
  output_variable (Ms, log); 
  output_variable (As, log); 
  output_variable (P, log); 
  output_variable (KE, log); 
  output_variable (E, log); 
}


void 
ReactionJarvis99::initialize (const Geometry& geo,
                              const Soil& soil, const SoilWater&, 
                              const SoilHeat&, const OrganicMatter&,
			      const Surface& surface, 
                              Treelog& msg)
{  
  TREELOG_MODEL (msg);

  // Mixing layer
  if (zi < 0.0)
    zi = surface.mixing_depth ();
  daisy_assert (zi > 0.0);

  // Find average dry bulk density for top cells.
  rho_b = geo.content_hood (soil, &Soil::dry_bulk_density, 
                            Geometry::cell_above);
  if (Mmax < 0.0)
    {
      const double clay 
        = geo.content_hood (soil, &Soil::clay, Geometry::cell_above);

      // Brubaker et at, 1992
      Mmax = 0.362 * clay - 0.00518;
      std::ostringstream tmp;
      tmp << "Mmax = " << Mmax;
      msg.debug (tmp.str ());
      if (Mmax <= 0.0)
        { 
          msg.warning ("Too little clay to initialize Mmax, using 0.01");
          Mmax = 0.01;
        }
    }

  if (Ms < 0.0)
    // Initialize to 10% of max.
    Ms = Mmax * 0.1;

  // [g C/cm^2 S] = [g C/g S] * [g S/cm^3 S] * [cm S]
  As = Ms * rho_b * zi; 
}

bool 
ReactionJarvis99::check (const Geometry& geo,
                         const Soil& soil, const SoilWater& soil_water,
                         const SoilHeat& soil_heat,
                         const OrganicMatter& organic,
			 const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (geo.bottom () > -zi)
    {
      ok = false;
      msg.error ("'zi' should be wholy within the soil");
    }
  if (!ReactionColgen::check (geo, soil, soil_water, soil_heat, 
                              organic, chemistry, msg))
    {
      ok = false;
    }
  return ok;
}

ReactionJarvis99::ReactionJarvis99 (const BlockModel& al)
  : ReactionColgen (al),
    units (al.units ()),
    rainergy (Librarian::build_item<Rainergy> (al, "rainergy")),
    tillage_replenish_all (al.flag ("tillage_replenish_all")),
    Mmax (al.number ("Mmax", -42.42e42)),
    Mmax_tillage_factor (al.plf ("Mmax_tillage_factor")),
    kd (al.number ("kd")),
    kr (al.number ("kr")),
    zi (al.number ("zi", -42.24e42)),
    rho_b (-42.42e42),
    Ms (al.number ("Ms", -42.42e42)),
    As (-42.42e42),
    P (-42.42e42),
    E (0.0)
{ }

static struct ReactionJarvis99Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionJarvis99 (al); }
  ReactionJarvis99Syntax ()
    : DeclareModel (Reaction::component, "colgen_Jarvis99", "colgen", "\
Colloid generation emulating the MACRO model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("ponddamp", "none");
    frame.set_strings ("cite", "macro-colloid", "mmax");
    frame.declare_object ("rainergy", Rainergy::component,
                      Attribute::Const, Attribute::Singleton,
                      "Model for calculating energy in rain.");
    frame.set ("rainergy", "Brown87");
    frame.declare_boolean ("tillage_replenish_all", Attribute::Const, "\
Set Ms = Mmax after tillage.");
    frame.set ("tillage_replenish_all", false);
    frame.declare ("Mmax", "g/g", Check::non_negative (), 
                   Attribute::OptionalConst, "\
Maximum amount of detachable particles.\n\
By default, method 1 of Brubaker et al, 1992, will be used.");
    // frame.set ("Mmax", 0.165);
    frame.declare ("Mmax_tillage_factor", "d", Attribute::None (), 
                   Check::non_negative (), Attribute::Const, "\
Factor to modify Mmax with as a fuction of days after tillage.");
    frame.set ("Mmax_tillage_factor", PLF::always_1 ());

    frame.declare ("kd", "g/J", Check::non_negative (), Attribute::Const,
                "Detachment rate coefficient.");
    // frame.set ("kd", 15.0);
    frame.declare ("kr", "g/cm^2/h", Check::non_negative (), Attribute::Const,
                "Replenishment rate coefficient.");
    // frame.declare ("kr", 0.1 /* [g/m^2/h] */ / (100.0 /* [cm/m] */ * 100.0));
    frame.declare ("zi", "cm", Check::positive (), Attribute::OptionalConst,
                   "Thickness of surface soil layer.\n\
By default, the value of 'z_mixing' from 'Surface' is used.");
    // frame.set ("zi", 0.1);
    frame.declare ("Ms", "g/g", Check::non_negative (), Attribute::OptionalState,
                "Current concentration of detachable particles in top soil.\n\
By default, 10% of Mmax.");
    frame.declare ("As", "g/cm^2", Attribute::LogOnly, 
                "Current amount of detachable particles in top soil.");
    frame.declare ("P", "g/cm^2/h", Attribute::LogOnly, 
                "Replenishment of detachable particles to top soil.");
    frame.declare ("KE", "J/cm^2/h", Attribute::LogOnly, 
               "Kinertic energy avalable for colloid generation.");
    frame.declare ("E", "J/cm^2/mm", Attribute::LogOnly, 
                "Kinetic energy in rain.");
  }
} ReactionJarvis99syntax;

// reaction_Jarvis99.C ends here.
