// movement.C
// 
// Copyright 2006 Per Abrahamsen andKVL.
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

#include "movement.h"
#include "block.h"
#include "librarian.h"

const char *const Movement::component = "movement";

symbol
Movement::library_id () const
{
  static const symbol id (component);
  return id;
}

Movement::Movement (Block& al)
  : ModelLogable (al.identifier ("type"))
{ }

Movement::~Movement ()
{ }

static Librarian Movement_init (Movement::component, "\
This component handles the movement in the soil.");

