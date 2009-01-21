// litter.C -- Litter lay below permanent vegetation.
// 
// Copyright 2003 Per Abrahamsen and KVL.
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

#include "litter.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "check.h"

void
Litter::load_syntax (FrameSubmodel& frame)
{
  frame.add_fraction ("vapor_flux_factor", Value::Const, "\
Reduction factor for potential evaporation below litter.");
  frame.add ("vapor_flux_factor", 1.0);
  frame.add ("interception_capacity", "mm", Value::Const,
	      "Storage capacity of litter.");
  frame.add ("interception_capacity", 0.0);
  frame.add ("albedo", Value::None (), Check::positive (),
              Value::OptionalConst, "Reflection factor.\n\
By default, the surface albedo will be used.");
}

Litter::Litter (const AttributeList& al)
  : vapor_flux_factor (al.number ("vapor_flux_factor")),
    interception_capacity (al.number ("interception_capacity")),
    albedo (al.number ("albedo", -1.0))
{ }

Litter::~Litter ()
{ }

static DeclareSubmodel 
lutter_submodel (Litter::load_syntax, "Litter", "\
Properties of litter below permanent vegetation.");
