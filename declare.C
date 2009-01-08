// librarian.C --- Manage model libraries.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "declare.h"
#include "librarian.h"

Declare::Declare (const symbol c, const symbol name, const symbol s,
                  const symbol d)
  : component (c),
    super (s),
    description (d)
{ Librarian::declare (component, name, *this); }

Declare::Declare (const symbol c, const symbol name, const symbol d)
  : component (c),
    super ("component"),
    description (d)
{ Librarian::declare (component, name, *this); }

Declare::~Declare ()
{ }

DeclareModel::DeclareModel (const symbol component,
                            const symbol name, const symbol super, 
                            const symbol description)
  : Declare (component, name, super, description)
{ }

DeclareModel::DeclareModel (const symbol component, 
                            const symbol name, 
                            const symbol description)
  : Declare (component, name, description)
{ }

DeclareModel::~DeclareModel ()
{ }


// librarian.C ends here
