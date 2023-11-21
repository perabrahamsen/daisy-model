// rootdens.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "rootdens.h"
#include "block_model.h"
#include "frame.h"
#include "check.h"
#include "librarian.h"

const char *const Rootdens::component = "rootdens";

symbol 
Rootdens::library_id () const
{
  static const symbol id (component);
  return id;
}

void
Rootdens::tick (const Geometry& geo,
		const SoilHeat& soil_heat, const SoilWater& soil_water,
		std::vector<double>& L, const double dt, Treelog& msg)
{ }

Rootdens::Rootdens (const BlockModel& al)
  : ModelFramed (al),
    SpRtLength (al.number ("SpRtLength"))
{ }

Rootdens::~Rootdens ()
{ }

static struct RootdensInit : public DeclareComponent 
{
  RootdensInit ()
    : DeclareComponent (Rootdens::component, "\
Root density calculations.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare ("SpRtLength", "m/g", Check::positive (), Attribute::Const,
               "Specific root length");
    frame.set ("SpRtLength", 100.0);
  }
} Rootdens_init;

// rootdens.C ends here.

