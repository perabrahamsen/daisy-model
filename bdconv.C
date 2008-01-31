// bdconv.C -- Bulk density convertion for select models.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "bdconv.h"
#include "geometry.h"
#include "soil.h"
#include "volume.h"

double
BD_convert::operator()(double value) const
{ 
  daisy_assert (bulk > 0.0);
  return out (in (value) / bulk); 
}

bool
BD_convert::valid (double value) const
{
  daisy_assert (bulk > 0.0);
  return in.valid (value) && out.valid (in (value) / bulk);
}

void
BD_convert::set_bulk (const Geometry& geo,
                      const Soil& soil, const Volume& volume,
                      const bool density_z, const bool density_x,
                      const bool density_y)
{
  bulk = 0.0;

  const size_t cell_size = geo.cell_size ();
  for (size_t i = 0; i < cell_size; i++)
    {
      const double f = geo.fraction_in_volume (i, volume);
      if (f > 1e-10)
        bulk += soil.dry_bulk_density (i) * geo.cell_volume (i) * f;
    }
  if (density_z)
    bulk /= volume.height (geo.bottom (), geo.top ()); 
  if (density_x)
    bulk /= volume.width (geo.left (), geo.right ()); 
  if (density_y)
    bulk /= volume.depth (geo.front (), geo.back ()); 
}

BD_convert::BD_convert (const symbol has, const symbol want,
                        const symbol bulk_unit)
  : in (Units::get_convertion (has, bulk_unit)),
    out (Units::get_convertion (Syntax::fraction (), want)),
    bulk (-42.42e42)
{ }

// bdconv.C ends here.
