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

#include "reaction.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block.h"
#include "librarian.h"
#include "chemistry.h"
#include "chemical.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "treelog.h"

struct ReactionJarvis99 : public Reaction
{
  // Parameters.
  const symbol colloid_name;
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
  double D;                     // Depletion [g C/cm^2 S/h]
  double P;                     // Replenishment [g C/cm^2 S/h]
  double E;                     // Energy in rain [J/cm^2 S/mm W]

  // Simulation.
  static const double R_to_E (const double R /* [mm W/h] */); // [J/cm^2/mm]
  void colloid_generation (const double Rain_intensity /* [mm W/h] */,
                           const double dt /* [h] */);
  void tick_top (const double direct_rain, 
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

const double 
ReactionJarvis99::R_to_E (const double R)
{ 
  const double cm2_per_m2 = (100.0 * 100.0);
  return  29.0 * (1 - 0.72 * exp (-0.05 * R)) / cm2_per_m2; 
}

void
ReactionJarvis99::colloid_generation (const double Rain_intensity, // [mm/h] 
                                   const double dt) // [h]
{
  // Kinetic energy of rain. [J cm^-2 mm^-1]
  E = R_to_E (Rain_intensity);
  
  // Detachment of colloids at the surface. [g cm^-2 h^-1]
  D = kd * E * Rain_intensity * Ms; 

  // Replenishment of colloids in the surface layer.
  P = kr * (1 - Ms / Mmax);     // [g cm^-2 h^-1]
  
  // Pure forward mass balance.
  As += (-D + P) * dt;  //[g cm^-2]
  Ms = As / (rho_b * zi); // [g C/g S]
}

void 
ReactionJarvis99::tick_top (const double direct_rain,
                         Chemistry& chemistry, const double dt, Treelog&)
{
  Chemical& colloid = chemistry.find (colloid_name);
  
  // Intensity of rain that hit the soil directly, without hitting the
  // canopy or snow pack on the way.
  const double R = direct_rain; // [mm/h]

  // Generate the colloids.
  colloid_generation (R, dt);

  colloid.add_to_surface_transform_source (D);
}

void 
ReactionJarvis99::output (Log& log) const 
{
  output_variable (Ms, log); 
  output_variable (As, log); 
  output_variable (D, log); 
  output_variable (P, log); 
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
ReactionJarvis99::check (const Units&, const Geometry& geo,
                      const Soil&, const SoilWater&, const SoilHeat&,
                      const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (geo.bottom () > -zi)
    {
      ok = false;
      msg.error ("'zi' should be wholy within the soil");
    }
  if (!chemistry.know (colloid_name))
    {
      msg.error ("'" + colloid_name + "' not traced");
      ok = false;
    }
  return ok;
}

ReactionJarvis99::ReactionJarvis99 (Block& al)
  : Reaction (al),
    colloid_name (al.name ("colloid")),
    Mmax (al.number ("Mmax")),
    kd (al.number ("kd")),
    kr (al.number ("kr")),
    zi (al.number ("zi")),
    rho_b (-42.42e42),
    Ms (al.number ("Ms", Mmax * 0.1)),
    As (-42.42e42),
    D (-42.42e42),
    P (-42.42e42),
    E (R_to_E (0.0))
{ }

static struct ReactionJarvis99Syntax
{
  static Model& make (Block& al)
  { return *new ReactionJarvis99 (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("colloid", Value::String, Value::Const,
		"Colloid to generate.");
    syntax.add ("Mmax", "g/g", Check::non_negative (), Value::Const,
                "Maximum amount of detachable particles.");
    alist.add ("Mmax", 0.165);
    syntax.add ("kd", "g/J", Check::non_negative (), Value::Const,
                "Detachment rate coefficient.");
    alist.add ("kd", 15.0);
    syntax.add ("kr", "g/cm^2/h", Check::non_negative (), Value::Const,
                "Replenishment rate coefficient.");
    alist.add ("kr", 0.1 /* [g/m^2/h] */ / (100.0 /* [cm/m] */ * 100.0));
    syntax.add ("zi", "cm", Check::positive (), Value::Const,
                "Thickness of surface soil layer.");
    alist.add ("zi", 0.1);
    syntax.add ("Ms", "g/g", Check::non_negative (), Value::OptionalState,
                "Current concentration of detachable particles in top soil.\n\
By default, 10% of Mmax.");
    syntax.add ("As", "g/cm^2", Value::LogOnly, 
                "Current amount of detachable particles in top soil.");
    syntax.add ("D", "g/cm^2/h", Value::LogOnly, 
                "Depletion of detachable particles from top soil.");
    syntax.add ("P", "g/cm^2/h", Value::LogOnly, 
                "Replenishment of detachable particles to top soil.");
    syntax.add ("E", "J/cm^2/mm", Value::LogOnly, 
                "Kinetic energy in rain.");
  }
  ReactionJarvis99Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Colloid generation emulating the MACRO model.");
    alist.add_strings ("cite", "macro-colloid");
    load_syntax (syntax, alist);
    Librarian::add_type (Reaction::component, "colgen_Jarvis99",
                         alist, syntax, &make);
  }
} ReactionJarvis99syntax;

// reaction_Jarvis99.C ends here.
