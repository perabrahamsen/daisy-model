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
#include "log.h"

EMPTY_TEMPLATE
Librarian<Phenology>::Content* Librarian<Phenology>::content = NULL;

const char *const Phenology::description = "\
The development process.";

void
Phenology::light_hour ()
{ partial_day_length += 1.0; }

void 
Phenology::output (Log& log) const
{
  log.output ("DS", DS);
  log.output ("partial_day_length", partial_day_length);
  log.output ("day_length", day_length);
  log.output ("partial_soil_temperature", partial_soil_temperature);
  log.output ("soil_temperature", soil_temperature);
  log.output ("soil_h", soil_h);
}

void 
Phenology::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Variables.
  syntax.add ("DS", Syntax::None (), Syntax::State,
	      "Development Stage.");
  alist.add ("DS", -1.0);
  syntax.add ("partial_day_length", "h", Syntax::State,
	      "Number of light hours this day, so far.");
  alist.add ("partial_day_length", 0.0);
  syntax.add ("day_length", "h", Syntax::State,
	      "Number of light hours yesterday.");
  alist.add ("day_length", 0.0);
  syntax.add ("partial_soil_temperature", "dg C h", Syntax::State,
	      "Soil temperature hours this day, so far.");
  alist.add ("partial_soil_temperature", 0.0);
  syntax.add ("soil_temperature", "dg C", Syntax::State,
	      "Average soil temperature yesterday.");
  alist.add ("soil_temperature", 0.0);
  syntax.add ("soil_h", "cm", Syntax::State,
	      "Soil pressure potential.");
  alist.add ("soil_h", -100.0);
}

Phenology::Phenology (const AttributeList& al)
  : name (al.name ("type")),
    // State.
    DS (al.number ("DS")),
    partial_day_length (al.number ("partial_day_length")),
    day_length (al.number ("day_length")),
    partial_soil_temperature (al.number ("partial_soil_temperature")),
    soil_temperature (al.number ("soil_temperature")),
    soil_h (al.number ("soil_h"))
{ }

Phenology::~Phenology ()
{ }
