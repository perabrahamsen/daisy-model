// soil_heat.C
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


#include "soil_heat.h"
#include "alist.h"
#include "soil.h"
#include "geometry.h"
#include "syntax.h"
#include "log.h"
#include <sstream>

void
SoilHeat::output_base (Log& log) const
{ output_value (T_, "T", log); }

bool
SoilHeat::check (const size_t n, Treelog& err) const
{
  bool ok = true;
  if (T_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n << " intervals but " 
          << T_.size () << " T values";
      err.entry (tmp.str ());
      ok = false;
    }
  return ok;
}

void
SoilHeat::load_base (Syntax& syntax, AttributeList&)
{ 
  Geometry::add_layer (syntax, Syntax::OptionalState, "T", "dg C",
                       "Soil temperature.");
}

SoilHeat::SoilHeat (const AttributeList&)
{ }

void
SoilHeat::initialize_base (const AttributeList& al, 
                           const Geometry& geo,
                           Treelog& out)
{
  // Fetch initial T.
  geo.initialize_layer (T_, al, "T", out);
}

SoilHeat::~SoilHeat ()
{ }
