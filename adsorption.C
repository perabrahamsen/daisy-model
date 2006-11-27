// adsorption.C
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


#include "adsorption.h"
#include "block.h"

template<>
Librarian<Adsorption>::Content* Librarian<Adsorption>::content = NULL;

const char *const Adsorption::description = "\
This component describes the adsorption of a chemical to the soil,\n\
which among other things affects how large a fraction can be\n\
transported with the water.";

bool
Adsorption::full () const
{ return false; }

void
Adsorption::output (Log&) const
{ }

Adsorption::Adsorption (Block& al)
  : name (al.identifier ("type"))
{ }

Adsorption::~Adsorption ()
{ }
