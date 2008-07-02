// stomatacon.C  -- Calculating stomata conductance
// 
// Copyright 2008 Birgitte Gjettermann, Per Abrahamsen and KU.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.5
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


#include "stomatacon.h"
#include "mathlib.h"
#include "block.h"
#include "librarian.h"

const char *const StomataCon::component = "Stomatacon";

symbol 
StomataCon::library_id () const
{
  static const symbol id (component);
  return id;
}

StomataCon::StomataCon (Block& al)
  : ModelLogable (al.identifier ("type"))
{ }

StomataCon::~StomataCon ()
{ }

static Librarian StomataCon_init (StomataCon::component, "\
The 'Stomatacon' component calculates the stomata conductance of water vapour.");

// StomataCon.C ends here.
