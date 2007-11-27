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

#define BUILD_DLL

#include "column.h"
#include "block.h"
#include "syntax.h"
#include "log.h"
#include "librarian.h"
#include "submodeler.h"
#include <map>

const char *const Column::component = "column";

void
Column::Point::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("x", Syntax::Unknown (), Syntax::Const, "X-Coordinate.");
  syntax.add ("y", Syntax::Unknown (), Syntax::Const, "Y-Coordinate.");
  syntax.order ("x", "y");
}

Column::Point::Point (const Block& al)
  : x (al.number ("x")),
    y (al.number ("y"))
{ }

Column::Point::~Point ()
{ }

const std::vector<const Column::Point*>&
Column::location () const
{ return location_; }

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

  syntax.add_submodule_sequence ("location", Syntax::Const, "\
Location of this column.\n\
\n\
The meaning depends on the number of point in the sequence.\n\
0 points: The column has no specific location.\n\
1 point: The column has a location, but no specific area.\n\
3 or more points: The column represents the area specified by a\n\
polygon with the specified corner points.", Point::load_syntax);
  alist.add ("location", std::vector<const AttributeList*> ());
}

Column::Column (Block& al)
  : alist (al.alist ()),
    name (al.identifier ("type")),
    size (al.number ("size")),
    location_ (map_submodel_const<Point> (al, "location"))
{ }

Column::~Column ()
{ }

static Librarian Column_init (Column::component, "\
A 'column' is an one-dimensional vertical description of the\n\
soil/crop/atmosphere system.  The column component contains most of\n\
the other processes in Daisy as submodels.");

