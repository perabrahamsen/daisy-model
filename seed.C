// seed.C - Initial growth.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "seed.h"
#include "block_model.h"
#include "librarian.h"

const char *const Seed::component = "seed";

symbol
Seed::library_id () const
{
  static const symbol id (component);
  return id;
}

Seed::Seed (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Seed::~Seed ()
{ }

static struct SeedInit : public DeclareComponent 
{
  SeedInit ()
    : DeclareComponent (Seed::component, "\
Initial growth after emergence.\n\
\n\
The initial growth process governs the growth of the crop until the\n\
point where there is enough leaf area for photosynthesis to take over.")
  { }
} Seed_init;

// seed.C ends here.
