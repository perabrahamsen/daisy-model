// nitrification.C
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


#include "nitrification.h"
#include "log.h"

EMPTY_TEMPLATE
Librarian<Nitrification>::Content* Librarian<Nitrification>::content = NULL;

const char *const Nitrification::description = "\
The nitrification process, transforming ammonium into nitrate and\n\
nitrous oxide.";

void
Nitrification::output (Log& log) const
{
  output_variable (NH4, log);
  output_variable (NO3, log);
  output_variable (N2O, log);
}

void
Nitrification::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_fraction ("N2O_fraction", Syntax::Const, 
                       "Fraction of ammonium lost as N2O.");
  alist.add ("N2O_fraction", 0.02);
  syntax.add ("NH4", 
              "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
              "Amount of ammonium consumed this hour.");
  syntax.add ("NO3", 
              "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
              "Amount of nitrate generated this hour.");
  syntax.add ("N2O", 
              "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
              "Amount of nitrous oxide generated this hour.");
}

Nitrification::Nitrification (const AttributeList& al)
  : name (al.identifier ("type")),
    N2O_fraction (al.number ("N2O_fraction"))
{ }

Nitrification::~Nitrification ()
{ }

