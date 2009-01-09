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
#include "frame.h"
#include "intrinsics.h"
#include "library.h"

void 
Declare::load (Frame& frame) const
{
  frame.add ("description", description);
  load_frame (frame);
}

symbol 
Declare::root_name ()
{
  static const symbol name ("component");
  return name;
}

const FrameModel* 
Declare::parent_model () const
{ return NULL; }

Declare::Declare (const symbol c, const symbol name,
                  const symbol d)
  : component (c),
    description (d)
{ Librarian::declare (component, name, *this); }

Declare::~Declare ()
{ }

void 
DeclareComponent::load_frame (Frame&) const
{ }

DeclareComponent::DeclareComponent (const symbol component,
                                    const symbol description)
  : Declare (component, root_name (), description),
    librarian (component, description)
{ }

const FrameModel* 
DeclareModel::parent_model () const
{ 
  Librarian::intrinsics ().instantiate (component, super);
  return &Librarian::intrinsics ().library (component).model (super); 
}

DeclareModel::DeclareModel (const symbol component,
                            const symbol name, const symbol s, 
                            const symbol description)
  : Declare (component, name, description),
    super (s)
{ }

DeclareModel::DeclareModel (const symbol component, 
                            const symbol name, 
                            const symbol description)
  : Declare (component, name, description),
    super (root_name ())
{ }

DeclareModel::~DeclareModel ()
{ }


// librarian.C ends here
