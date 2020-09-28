// reaction_colgen.C --- Base model for coloid generation.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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

#include "reaction_colgen.h"
#include "librarian.h"
#include "frame.h"
#include "log.h"
#include "chemistry.h"
#include "treelog.h"
#include "block_model.h"
#include "geometry.h"
#include "soil.h"
#include "surface.h"

double
ReactionColgen::find_surface_soil (const Geometry& geo, const Soil& soil, 
                                   const Surface& surface)
{
  const double z_mixing = surface.mixing_depth (); // [cm]
  const double rho_b 
    = geo.content_hood (soil, &Soil::dry_bulk_density, 
                        Geometry::cell_above); // [g/cm^3]
  return rho_b * z_mixing;                     // [g/cm^2]
}

void 
ReactionColgen::tick_colgen (const double total_rain, const double h_pond)
{
  dds = Ponddamp::dds (total_rain);
  KH = ponddamp->value (h_pond, total_rain);
}

void 
ReactionColgen::output_colgen (Log& log) const
{
  if (dds > -1.0)
    output_variable (dds, log); 
  if (KH > -1.0)
    output_variable (KH, log); 
  output_variable (D, log); 
  output_variable (surface_release, log);
}

bool 
ReactionColgen::check (const Geometry& geo,
                       const Soil&, const SoilWater&, const SoilHeat&,
                       const OrganicMatter&,
		       const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.know (colloid_name))
    {
      msg.error ("'" + colloid_name + "' not traced");
      ok = false;
    }
  return ok;
}

ReactionColgen::ReactionColgen (const BlockModel& al)
  : Reaction (al),
    colloid_name (al.name ("colloid")),
    ponddamp (Librarian::build_item<Ponddamp> (al, "ponddamp")),
    dds (-42.42e42),
    KH (-42.42e42),
    D (-42.42e42),
    surface_release (0.0)
{ }
 
ReactionColgen::~ReactionColgen ()
{ }

static struct ReactionColgenSyntax : public DeclareBase
{
  ReactionColgenSyntax ()
    : DeclareBase (Reaction::component, "colgen", "\
Shared parameter and log variable for colloid generation models.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("colloid", Attribute::Const, "Colloid to generate.");
    frame.declare_object ("ponddamp", Ponddamp::component,
                          Attribute::Const, Attribute::Singleton,
                          "Model for calculating 'KH'.");
    frame.declare ("dds", "mm", Attribute::LogOnly, "Median raindrop size.");
    frame.declare ("KH", Attribute::Fraction (), Attribute::LogOnly, 
                   "Ponding factor.");
    frame.declare ("D", "g/cm^2/h", Attribute::LogOnly, 
                   "Depletion of detachable particles from top soil.");
    frame.declare ("surface_release", Attribute::Fraction (), Attribute::LogOnly, "\
Fraction of available soil particles released as colloids this timestep.");
  }
} ReactionColgen_syntax;

// reaction_colgen.C ends here.
