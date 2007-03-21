// column.C
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


#include "column.h"
#include "block.h"
#include "syntax.h"
#include "log.h"
#include <map>

const char *const Column::description = "\
A 'column' is an one-dimensional vertical description of the\n\
soil/crop/atmosphere system.  The column component contains most of\n\
the other processes in Daisy as submodels.";

const char *const Column::component = "column";

void
Column::output (Log& log) const
{
  output_variable (size, log);
}

void
Column::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("size", Syntax::Unknown (), Syntax::State,
	      "Area covered by this column, for use by the 'merge' action.\n\
The dimension is up to you, as long as all columns use the same unit.");
  alist.add ("size", 1.0);
}

Column::Column (Block& al)
  : alist (al.alist ()),
    name (al.identifier ("type")),
    size (alist.number ("size"))
{ }

Column::~Column ()
{ }

