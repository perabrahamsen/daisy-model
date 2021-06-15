// reaction_Morgan98.C -- Colloids generation based on kinetic energy.
// 
// Copyright 2009 Per Abrahamsen and KU
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
#include <sstream>
#include "check.h"
#include "block_model.h"
#include "librarian.h"
#include "chemistry.h"
#include "chemical.h"
#include "log.h"
#include "treelog.h"
#include "frame.h"
#include "rainergy.h"
#include "vegetation.h"
#include "bioclimate.h"
#include <memory>

struct ReactionMorgan98 : public ReactionColgen
{
  // Constants.
  /* const */ double surface_soil; // Soil on surface layer. [g/m^2]

  // Parameters.
  const std::unique_ptr<Rainergy> rainergy; // Energy in rain [J/cm^2/h]
  const double kd;                        // Detachment rate coefficient [g/J]

  // Log variable.
  double KE;                    // Energy for colloid generation [J/cm^2/h]
  double E;                     // Energy in rain [J/cm^2/h]

  // Simulation.
  void colloid_generation (const double total_rain /* [mm/h] */, 
                           const double direct_rain /* [mm/h] */,
                           const double canopy_drip /* [mm/h] */,
                           const double canopy_height /* [m] */,
                           const double h_pond /* [mm] */,
                           const double dt /* [h] */);
  void  tick_top (const Vegetation&, const Bioclimate&,
		  const double tillage_age /* [d] */,
                  const double total_rain,  
                  const double h_pond,
                  OrganicMatter&, Chemistry& chemistry,
		  const double dt, Treelog&);
                           
  void output (Log& log) const;

  // Create and Destroy.
  void initialize (const Geometry& geo,
                   const Soil& soil, const SoilWater&, const SoilHeat&, 
                   const OrganicMatter&, const Surface&, Treelog&);
  ReactionMorgan98 (const BlockModel& al);
};

void
ReactionMorgan98::colloid_generation (const double total_rain /* [mm/h] */, 
                                      const double direct_rain /* [mm/h] */,
                                      const double canopy_drip /* [mm/h] */,
                                      const double canopy_height /* [m] */,
                                      const double h_pond /* [mm] */,
                                      const double dt /* [h] */)
{
  // Energy for colloid generation. [J cm^-2 h^-1]
  KE = rainergy->value (total_rain , direct_rain, canopy_drip, canopy_height);

  // Kinetic energy of rain. [J cm^-2 mm^-1]
  E = (total_rain > 0) ? KE / total_rain : 0.0;
  
  // Detachment of colloids at the surface. [g cm^-2 h^-1]
  D = kd * KH * KE;

  surface_release = D * dt / surface_soil; // []
}

void 
ReactionMorgan98::tick_top (const Vegetation& vegetation,
			    const Bioclimate& bioclimate,
			    const double /* tillage_age */,
                            const double total_rain, 
                            const double h_pond,
                            OrganicMatter&,
			    Chemistry& chemistry, const double dt, Treelog&)
{
  const double exposed_soil = 1.0 - bioclimate.litter_cover ();
  const double direct_rain = bioclimate.direct_rain () * exposed_soil; // [mm/h]
  const double canopy_drip = bioclimate.canopy_leak () * exposed_soil; // [mm/h]
  const double h_veg = vegetation.height () * 0.01 ;	 // [m]
  
  ReactionColgen::tick_colgen (total_rain, h_pond);

  Chemical& colloid = chemistry.find (colloid_name);
  
  colloid_generation (total_rain, direct_rain, canopy_drip, h_veg,
                      h_pond, dt);

  colloid.add_to_surface_transform_source (D);
  colloid.release_surface_colloids (surface_release);
}

void 
ReactionMorgan98::output (Log& log) const 
{
  ReactionColgen::output_colgen (log);
  output_variable (KE, log); 
  output_variable (E, log); 
}


void 
ReactionMorgan98::initialize (const Geometry& geo, 
                              const Soil& soil,
                              const SoilWater&, const SoilHeat&, 
                              const OrganicMatter&,
			      const Surface& surface, Treelog&)
{ surface_soil = find_surface_soil (geo, soil, surface); }

ReactionMorgan98::ReactionMorgan98 (const BlockModel& al)
  : ReactionColgen (al),
    surface_soil (-42.42e42), 
    rainergy (Librarian::build_item<Rainergy> (al, "rainergy")),
    kd (al.number ("kd")),
    E (-42.42e42)
{ }

static struct ReactionMorgan98Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionMorgan98 (al); }
  ReactionMorgan98Syntax ()
    : DeclareModel (Reaction::component, "colgen_Morgan98", "colgen", "\
Colloid generation using kinetic energy, emulating EUROSEM.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "EUROSEM");

    frame.declare ("kd", "g/J", Check::non_negative (), Attribute::Const,
                "Detachment rate coefficient.\n\
The EUROSEM user manual list values between 0.8 and 6.0 [g/J] for various\n\
soils in Table A9.1.");
    frame.declare_object ("rainergy", Rainergy::component,
                      Attribute::Const, Attribute::Singleton,
                      "Model for calculating energy in rain.");
    frame.set ("rainergy", "EUROSEM");

    frame.declare ("KE", "J/cm^2/h", Attribute::LogOnly, 
               "Kinertic energy avalable for colloid generation.");
    frame.declare ("E", "J/cm^2/mm", Attribute::LogOnly, 
               "Kinetic energy in rain.");
  }
} ReactionMorgan98syntax;

// reaction_Morgan98.C ends here.
