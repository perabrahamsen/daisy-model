// rootdens.C
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


#include "rootdens.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "check.h"
#include "librarian.h"

const char *const Rootdens::description = "\
Root density calculations.";

const char *const Rootdens::component = "rootdens";

void
Rootdens::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("SpRtLength", "m/g", Check::positive (), Syntax::Const,
	      "Specific root length");
  alist.add ("SpRtLength", 100.0);
}

Rootdens::Rootdens (Block& al)
  : name (al.identifier ("type")),
    SpRtLength (al.number ("SpRtLength"))
{ }

Rootdens::~Rootdens ()
{ }

static Librarian Rootdens_init (Rootdens::component, Rootdens::description);

