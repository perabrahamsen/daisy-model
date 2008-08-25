// bdconv.h -- Bulk density convertion for select models.
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


#ifndef BDCONV_H
#define BDCONV_H

#include "units.h"
#include "symbol.h"
#include <memory>

class Geometry;
class Soil;
class Volume;

struct BD_convert : public Units::Convert
{
  const Units::Convert& in;
  const Units::Convert& out;
  double bulk;

  // Use.
  double operator()(double value) const;
  bool valid (double value) const;
  void set_bulk (const Geometry& geo,
                 const Soil& soil, const Volume& volume,
                 const bool density_z, const bool density_x,
                 const bool density_y);
  void set_bulk (const double new_bulk)
  { bulk = new_bulk; }

  // Create and destroy.
  BD_convert (const symbol has, const symbol want, const symbol bulk_unit);
};

#endif // BDCONV_H
