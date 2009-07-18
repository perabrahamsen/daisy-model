// photo.C - Leaf photosynthesis component.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#include "photo.h"
#include "block_model.h"
#include "librarian.h"
#include "check.h"
#include "frame.h"

const char *const Photo::component = "photosynthesis";

symbol
Photo::library_id () const
{
  static const symbol id (component);
  return id;
}

double
Photo::min_PAR () const // Minimum PAR at top of canopy. [W/m^2]
{ return min_PAR_; }

void
Photo::clear ()
{ }

Photo::Photo (const BlockModel& al)
  : ModelDerived (al.type_name ()),
    min_PAR_ (al.number ("min_PAR"))
{ }

Photo::~Photo ()
{ }

static struct PhotoInit : public DeclareComponent 
{
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame);
    frame.declare ("min_PAR", "W/m^2", Check::non_negative (), Attribute::Const,
               "Minimum PAR at top of canopy for photosynthesis.\n\
If radiation is below this amount, photosynthesis will be disabled.");
    frame.set ("min_PAR", 0.1);
  }
  PhotoInit ()
    : DeclareComponent (Photo::component, "\
Leaf photosynthesis.")
  { }
} Photo_init;

// photo.C ends here.
