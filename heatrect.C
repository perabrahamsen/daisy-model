// heatrect.C -- Heat transport in rectangular grid.
// 
// Copyright 2008 Per Abrahamsen andKVL.
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

#include "heatrect.h"
#include "block.h"
#include "librarian.h"

const char *const Heatrect::component = "heatrect";

Heatrect::Heatrect (Block& al)
  : name (al.identifier ("type"))
{ }

Heatrect::~Heatrect ()
{ }

static Librarian Heatrect_init (Heatrect::component, "\
Heat transport in rectangular grid.");

// heatrect.C ends here
