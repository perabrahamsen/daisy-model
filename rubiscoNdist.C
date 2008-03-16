// rubiscoNdist.C  -- Rubisco N distribution in canopy
// 
// Copyright 2006 Birgitte Gjettermann, Per Abrahamsen and KVL.
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

#include "rubiscoNdist.h"
#include "block.h"
#include "mathlib.h"
#include "librarian.h"

const char *const RubiscoNdist::component = "rubiscoNdist";

symbol
RubiscoNdist::library_id () const
{
  static const symbol id (component);
  return id;
}

RubiscoNdist::RubiscoNdist (Block& al)
  : ModelLogable (al.identifier ("type"))
{ }

RubiscoNdist::~RubiscoNdist ()
{ }

static Librarian RubiscoNdist_init (RubiscoNdist::component, "\
The 'rubiscoNdist' component calculates the rubisco N distribution for photosynthesis in the canopy.");

