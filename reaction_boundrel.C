// reaction_boundrel.C -- Release of bound surface chemicals with colloids.
// 
// Copyright 2009 Copenhagen Univeristy
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
#include "log.h"
#include "geometry.h"
#include "chemical.h"
#include "chemistry.h"
#include "treelog.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"

struct ReactionBoundrel : public Reaction
{
  // Parameters.
  const symbol immobile;
  const symbol bound;
  const symbol colloid;
   
  // Output.
  double release;               // Release rate [g/cm^2/h]
  void output (Log& log) const
  { output_variable (release, log); }

  // Simulation.
  void tick_top (const Vegetation&, const Bioclimate&,
		 const double /* tillage_age */,
                 const double /* total_rain */, 
                 const double /* h_pond */,
                 OrganicMatter&, Chemistry& chemistry,
		 const double dt, Treelog&)
  { 
    // Find it.
    Chemical& imm = chemistry.find (immobile);  
    Chemical& col = chemistry.find (colloid);
    const double colloid_release_fraction 
      = col.surface_release_fraction (); // []
    const double immobile_amount 
      = imm.surface_immobile_amount (); // [g/cm^2]
    const double release_amount 
      = immobile_amount * colloid_release_fraction; // [g/cm^2]

    // Log it.
    release = release_amount / dt; // [g/cm^2/h]

    // Make it so.
    imm.add_to_surface_transform_source (-release);
    if (chemistry.know (bound))
      chemistry.find (bound).add_to_surface_transform_source (release);
  }


  // Create.
  bool check (const Geometry&, 
              const Soil&, const SoilWater&, const SoilHeat&,
              const OrganicMatter&,
	      const Chemistry& chemistry, Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (immobile))
      {
        msg.error ("'" + immobile + "' not traced");
        ok = false;
      }
    if (!chemistry.know (bound) && bound != Attribute::None ())
      {
        msg.error ("'" + bound + "' not traced");
        ok = false;
      }
    if (!chemistry.know (colloid))
      {
        msg.error ("'" + colloid + "' not traced");
        ok = false;
      }
    return ok;
  }
  void initialize (const Geometry&, const Soil&, 
                   const SoilWater&, const SoilHeat&, const OrganicMatter&,
		   const Surface&, Treelog&)
  { }
  explicit ReactionBoundrel (const BlockModel& al)
    : Reaction (al),
      immobile (al.name ("immobile")),
      bound (al.name ("bound", Attribute::None ())),
      colloid (al.name ("colloid")),
      release (0.0)
  { }
};

static struct ReactionBoundrelSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionBoundrel (al); }
  ReactionBoundrelSyntax ()
    : DeclareModel (Reaction::component, "bound_release", "\
Release of chemicals bound to colloids from surface soil.\n\
\n\
This follows the generation of colloids on the surface.  The colloid\n\
generation model should already have calculated the amount of released\n\
colloids as either a fraction of the readily available colloids\n\
(Jarvis99), or as a fraction of the total amount of soil in the mixing\n\
layer (Styczen88, Morgan98).  The same fraction of the immobile\n\
chemical on the surface is released in the colloid bound form.\n\
\n\
This reaction must be listed after the colloid generation reaction in\n\
the setup file.")
  { }
  void load_frame (Frame& frame) const
  {


    frame.declare_string ("immobile", Attribute::Const,
                   "Immobile (or mixed form) chemical in the soil surface.");
    frame.declare_string ("bound", Attribute::OptionalConst,
                   "Chemical bound to colloids.\n\
If unspecified, the colloid bound form will not be traced.");
    frame.declare_string ("colloid", Attribute::OptionalConst,
                   "Name of colloid whose release we mimic.");
    frame.set ("colloid", "colloid");
    frame.declare ("release", "g/cm^2/h", Attribute::LogOnly,
                   "Release rate of immobile chemical as colloids.");
  }
  
} ReactionBoundrel_syntax;

// reaction_boundrel.C ends here.


