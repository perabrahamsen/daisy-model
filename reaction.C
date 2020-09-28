// reaction.C --- Transformation between two soil chemicals.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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
#include "librarian.h"

const char *const Reaction::component = "reaction";

symbol 
Reaction::library_id () const
{
  static const symbol id (component);
  return id;
}

void 
Reaction::tick_top (const Vegetation&, const Bioclimate&,
		    const double tillage_age /* [d] */,
                    const double total_rain, 
                    const double h_pond,
                    OrganicMatter&, Chemistry& chemistry,
		    const double dt, Treelog&)
{ }

void 
Reaction::tick_surface (const Geometry&, 
                        const Soil&, const SoilWater&, const SoilHeat&,
                        const Surface&,
			OrganicMatter&, Chemistry&, const double dt, Treelog&)
{ }

void 
Reaction::tick_soil (const Geometry&, const Soil&, const SoilWater&,
                     const SoilHeat&, OrganicMatter&, Chemistry&,
                     const double, Treelog&)
{ }

Reaction::Reaction (const BlockModel& al)
  : ModelFramed (al)
{ }

Reaction::~Reaction ()
{ }

static struct ReactionInit : public DeclareComponent 
{
  ReactionInit ()
    : DeclareComponent (Reaction::component, "\
Generic transformations between soil chemicals.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Reaction_init;

// reaction.C ends here.
