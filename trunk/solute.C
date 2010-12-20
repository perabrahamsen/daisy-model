// solute.C --- Dirty water.
// 
// Copyright 2010 KU.
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

#include "solute.h"
#include "block_model.h"
#include "librarian.h"

// solute component.

const char *const Solute::component = "solute";

symbol
Solute::library_id () const
{
  static const symbol id (component);
  return id;
}

Solute::Solute (const BlockModel& al)
  : ModelLogable (al.type_name ())
{ }

Solute::~Solute ()
{ }

static struct SoluteInit : public DeclareComponent 
{
  SoluteInit ()
    : DeclareComponent (Solute::component, "\
Water composition.")
  { }
} Solute_init;

// solute.C ends here.
