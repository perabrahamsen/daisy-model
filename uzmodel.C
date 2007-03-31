// uzmodel.C
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


#include "uzmodel.h"
#include "block.h"
#include "librarian.h"

UZmodel::UZmodel (Block& al)
  : name (al.identifier ("type"))
{ }

UZmodel::~UZmodel ()
{ }

const char *const UZmodel::component = "uzmodel";

static Librarian UZmodel_init (UZmodel::component, "\
The 'uzmodel' component handles the vertical water movement in the\n\
unsaturated zone soil matrix.");

