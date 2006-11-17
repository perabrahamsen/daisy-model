// geometry_vert.C --- A discretization based on vertical columns.
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


#include "geometry_vert.h"
#include "block.h"
#include <sstream>

bool 
GeometryVert::check_z_border (const double value, Treelog& err) const
{
  bool ok = false;

  for (size_t i = 0; i < cell_size (); i++)
    if (approximate (value, zplus (i)))
      ok = true;

  if (!ok)
    {
      std::ostringstream tmp;
      tmp << "No geometric border near " << value 
          << " [cm]on z-axis, log results will be inexact";
      err.warning (tmp.str ());
    }
  return ok;
}

GeometryVert::GeometryVert (Block& al)
  : Geometry (al)
{ }

GeometryVert::~GeometryVert ()
{ }
