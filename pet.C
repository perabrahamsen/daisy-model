// pet.C  -- Potential evopotranspiration
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


#include "pet.h"
#include "syntax.h"
#include "block.h"
#include "log.h"
#include "vegetation.h"
#include "surface.h"

using namespace std;

template<>
BuildBase* Librarian<Pet>::content = NULL;

const char *const Pet::description = "\
The 'pet' component should calculate the potential evapotranspiration\n\
from meteorological data, as well as the crop and soil state.";

const char *const Pet::component = "pet";

double
Pet::reference_to_potential (const Vegetation& crops, 
			     const Surface& surface,
			     double ref)
{
  const double cover = crops.cover ();
  const double EpFactor = cover * crops.EpFactor ()
    + (1.0 - cover) * surface.EpFactor ();
  return EpFactor * max (0.0, ref);
}

double
Pet::dry () const 
{ return wet (); }

void
Pet::output (Log& log) const
{
  output_value (wet (), "wet", log);
  output_value (dry (), "dry", log);
}

void 
Pet::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("wet", "mm/h", Syntax::LogOnly, 
	      "Potential evapotranspiration for a wet system.");
  syntax.add ("dry", "mm/h", Syntax::LogOnly, 
	      "Potential evapotranspiration for a dry system.");
  syntax.add ("reference_evapotranspiration", "mm/h", Syntax::LogOnly, 
	      "Reference evapotranspiration for a dry system.");
}

Pet::Pet (Block& al)
  : name (al.identifier ("type")),
    alist (al.alist ())
{ }

Pet::~Pet ()
{ }

