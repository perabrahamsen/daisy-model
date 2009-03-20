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
#include "block.h"
#include "librarian.h"
#include "chemistry.h"
#include "chemical.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "treelog.h"
#include "frame.h"
#include "rainergy.h"
#include <sstream>
#include <memory>

struct ReactionJarvis99 : public ReactionColgen
{
  // Parameters.
  const std::auto_ptr<Rainergy> rainergy; // Energy in rain [J/cm^2/h]
  const double Mmax;            // Max colloid pool [g C/g S]
  const double kd;              // Depletion rate from pool [g S/J]
  const double kr;              // Replenishment rate to pool [g C/cm^2 S/h] 
  const double zi;              // Mixing layer thickness [cm S]

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
  void colloid_generation (const double total_rain /* [mm/h] */, 
                           const double direct_rain /* [mm/h] */,
                           const double canopy_drip /* [mm/h] */,
                           const double canopy_height /* [m] */,
                           const double dt /* [h] */);
  void tick_top (const double total_rain, const double direct_rain,
                 const double canopy_drip,
                 const double cover, const double h_veg, 
                 const double h_pond,
                 Chemistry& chemistry, const double dt, Treelog&);
                           
  void output (Log& log) const;

  // Create and Destroy.
  void initialize (const Units&, const Geometry& geo,
                   const Soil& soil, const SoilWater&, const SoilHeat&, 
                   Treelog&);
  bool check (const Units&, const Geometry& geo,
              const Soil&, const SoilWater&, const SoilHeat&,
	      const Chemistry& chemistry, Treelog& msg) const;
  ReactionJarvis99 (Block& al);
};

void
ReactionJarvis99::colloid_generation (const double total_rain /* [mm/h] */, 
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
  D = std::min (kd * KE * KH * dt * Ms, As); 
  daisy_assert (D >= 0.0);

  // Replenishment of colloids in the surface layer.
  daisy_assert (Ms <= Mmax);
  P = kr * (1 - Ms / Mmax);     // [g cm^-2 h^-1]
  
  // Pure forward mass balance.
  As += (-D + P) * dt;  //[g cm^-2]
  daisy_assert (As >= 0.0);
  Ms = As / (rho_b * zi); // [g C/g S]
}

void 
ReactionJarvis99::tick_top (const double total_rain, const double direct_rain,
                            const double canopy_drip,
                            const double cover, const double h_veg, 
                            const double h_pond,
                            Chemistry& chemistry, const double dt, Treelog&)
{
  ReactionColgen::tick_colgen (total_rain, h_pond);

  Chemical& colloid = chemistry.find (colloid_name);
  
  // Generate the colloids.
  colloid_generation (total_rain, direct_rain, canopy_drip, h_veg, dt);

  colloid.add_to_surface_transform_source (D);
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
ReactionJarvis99::initialize (const Units&, const Geometry& geo,
                           const Soil& soil, const SoilWater&, const SoilHeat&, 
                           Treelog&)
{  
  // Find average dry bulk density for top cells.
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  double total_area = 0.0;
  rho_b = 0.0;
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const size_t cell = geo.edge_other (edge, Geometry::cell_above);
      // We weight them according to horizontal area.
      const double area = geo.edge_area (edge) * geo.edge_sin_angle (edge);
      total_area += area;
      rho_b += soil.dry_bulk_density (cell) * area;
    }
  daisy_assert (total_area > 0.0);
  rho_b /= total_area;

  // [g C/cm^2 S] = [g C/g S] * [g S/cm^3 S] * [cm S]
  As = Ms * rho_b * zi; 
}

bool 
ReactionJarvis99::check (const Units& units, const Geometry& geo,
                         const Soil& soil, const SoilWater& soil_water,
                         const SoilHeat& soil_heat,
                         const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (geo.bottom () > -zi)
    {
      ok = false;
      msg.error ("'zi' should be wholy within the soil");
    }
  if (!ReactionColgen::check (units, geo, soil, soil_water, soil_heat, 
                              chemistry, msg))
    {
      ok = false;
    }
  return ok;
}

ReactionJarvis99::ReactionJarvis99 (Block& al)
  : ReactionColgen (al),
    rainergy (Librarian::build_item<Rainergy> (al, "rainergy")),
    Mmax (al.number ("Mmax")),
    kd (al.number ("kd")),
    kr (al.number ("kr")),
    zi (al.number ("zi")),
    rho_b (-42.42e42),
    Ms (al.number ("Ms", Mmax * 0.1)),
    As (-42.42e42),
    P (-42.42e42),
    E (0.0)
{ }

static struct ReactionJarvis99Syntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ReactionJarvis99 (al); }
  ReactionJarvis99Syntax ()
    : DeclareModel (Reaction::component, "colgen_Jarvis99", "colgen", "\
Colloid generation emulating the MACRO model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_strings ("cite", "macro-colloid");
    frame.add_object ("rainergy", Rainergy::component,
                      Value::Const, Value::Singleton,
                      "Model for calculating energy in rain.");
    frame.add ("rainergy", "Brown87");
    frame.add ("Mmax", "g/g", Check::non_negative (), Value::Const,
                "Maximum amount of detachable particles.");
    // frame.add ("Mmax", 0.165);
    frame.add ("kd", "g/J", Check::non_negative (), Value::Const,
                "Detachment rate coefficient.");
    // frame.add ("kd", 15.0);
    frame.add ("kr", "g/cm^2/h", Check::non_negative (), Value::Const,
                "Replenishment rate coefficient.");
    // frame.add ("kr", 0.1 /* [g/m^2/h] */ / (100.0 /* [cm/m] */ * 100.0));
    frame.add ("zi", "cm", Check::positive (), Value::Const,
                "Thickness of surface soil layer.");
    // frame.add ("zi", 0.1);
    frame.add ("Ms", "g/g", Check::non_negative (), Value::OptionalState,
                "Current concentration of detachable particles in top soil.\n\
By default, 10% of Mmax.");
    frame.add ("As", "g/cm^2", Value::LogOnly, 
                "Current amount of detachable particles in top soil.");
    frame.add ("P", "g/cm^2/h", Value::LogOnly, 
                "Replenishment of detachable particles to top soil.");
    frame.add ("KE", "J/cm^2/h", Value::LogOnly, 
               "Kinertic energy avalable for colloid generation.");
    frame.add ("E", "J/cm^2/mm", Value::LogOnly, 
                "Kinetic energy in rain.");
  }
} ReactionJarvis99syntax;

// reaction_Jarvis99.C ends here.
