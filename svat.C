// svat.C  -- Soil, Vegetation and ATmostphere.
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


#include "svat.h"
#include "log.h"
#include "block.h"

const char *const SVAT::description = "\
The task of the 'svat' component is to calculate the production\n\
stress, given the potential evapotranspiration, the actual\n\
evaporation from the surface, meteorological data, and the vegetation\n\
and soil state.";

const char *const SVAT::component = "svat";

void
SVAT::output (Log&) const
{ }

void 
SVAT::load_syntax (Syntax&, AttributeList&)
{ }

SVAT::SVAT (Block& al)
  : name (al.identifier ("type"))
{ }

SVAT::~SVAT ()
{ }

// svat.C ends here.
