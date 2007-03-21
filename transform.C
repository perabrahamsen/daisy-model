// transform.C --- Transformation between two soil components.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#include "transform.h"
#include "block.h"

template<>
BuildBase* Librarian<Transform>::content = NULL;

const char *const Transform::description = "\
Generic transformations between soil components.";

const char *const Transform::component = "transform";

bool
Transform::check (const Soil&, Treelog&) const
{ return true; }

void
Transform::initialize (const Soil&, Treelog&)
{ }

void
Transform::load_syntax (Syntax&, AttributeList&)
{ }

Transform::Transform (Block& al)
  : name (al.identifier ("type")),
    alist (al.alist ())
{ }

Transform::~Transform ()
{ }
