// unit.C -- Specify unit for scalar.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "unit.h"
#include "librarian.h"
#include "block.h"

const char *const Unit::component = "unit";

Unit::Unit (Block& al)
  : name (al.identifier ("type"))
{ }

Unit::~Unit ()
{ }

static Librarian Unit_init (Unit::component, "\
The 'unit' allows you to define convertion to and from SI base units .");

// unit.C ends here.
