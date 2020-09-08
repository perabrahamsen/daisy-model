// chemical.C
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

#define BUILD_DLL


#include "chemical.h"
#include "block_model.h"
#include "librarian.h"
#include "vcheck.h"
#include "assertion.h"
#include <map>

const char *const Chemical::component = "chemical";

symbol 
Chemical::library_id () const
{
  static const symbol id (component);
  return id;
}

const symbol
Chemical::NO3 ()
{
  static const symbol unit ("NO3");
  return unit;
}

const symbol
Chemical::NH4 ()
{
  static const symbol unit ("NH4");
  return unit;
}

const symbol
Chemical::DON ()
{
  static const symbol unit ("DON");
  return unit;
}

const symbol
Chemical::DOC ()
{
  static const symbol unit ("DOC");
  return unit;
}

const symbol
Chemical::surface_storage_unit ()
{
  static const symbol unit ("g/m^2");
  return unit;
}

const symbol
Chemical::surface_flux_unit ()
{
  static const symbol unit ("g/m^2/h");
  return unit;
}

const VCheck& 
Chemical::check_buildable ()
{
  static const VCheck::Buildable buildable (component);
  return buildable;
}

Chemical::Chemical (const BlockModel& al)
  : ModelFramed (al)
{ }

Chemical::~Chemical ()
{ }

static struct ChemicalInit : public DeclareComponent 
{
  ChemicalInit ()
    : DeclareComponent (Chemical::component, "\
This component should, for a specific chemical (typically a pesticide),\n\
provide a description of the properties of interest to Daisy.")
  { }
} Chemical_init;

// chemical.C ends here.
