// phenology.C -- Crop development process.
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003 KVL.
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

#include "phenology.h"
#include "block.h"
#include "log.h"
#include "syntax.h"
#include "alist.h"
#include "librarian.h"

const char *const Phenology::component = "phenology";

void
Phenology::light_time (const double dt)
{ partial_day_length += dt; }

void 
Phenology::output (Log& log) const
{
  output_variable (DAP, log);
  output_variable (DS, log);
  output_variable (partial_day_length, log);
  output_variable (day_length, log);
}

bool
Phenology::mature () const
{ return DS >= 2.0; }

void 
Phenology::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Variables.
  syntax.add ("DAP", "d", Syntax::State, "Days after planting.");
  alist.add ("DAP", 0.0);
  syntax.add ("DS", Syntax::None (), Syntax::State,
	      "Development Stage.");
  alist.add ("DS", -1.0);
  syntax.add ("partial_day_length", "h", Syntax::State,
	      "Number of light hours this day, so far.");
  alist.add ("partial_day_length", 0.0);
  syntax.add ("day_length", "h", Syntax::State,
	      "Number of light hours yesterday.");
  alist.add ("day_length", 0.0);
}

Phenology::Phenology (Block& al)
  : name (al.identifier ("type")),
    // State.
    DAP (al.number ("DAP")),
    DS (al.number ("DS")),
    partial_day_length (al.number ("partial_day_length")),
    day_length (al.number ("day_length"))
{ }

Phenology::~Phenology ()
{ }

static Librarian Phenology_init (Phenology::component, "\
The development process.");

