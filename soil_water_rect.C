// soil_water_rect.C -- Soil water movement in a a rectangular grid.
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


#include "soil_water_rect.h"
#include "geometry_rect.h"
#include "soil.h"
#include "submodel.h"

void
SoilWaterRect::clear ()
{ clear_base (); }

double 
SoilWaterRect::top_flux () const
{ return 0.0; }
 
bool 
SoilWaterRect::check (size_t n, Treelog& msg) const
{
  bool ok = check_base (n, msg);
  return ok;
}
void 
SoilWaterRect::output (Log& log) const
{ output_base (log); }

  // Communication with surface.
double
SoilWaterRect::MaxExfiltration (const Geometry&,
                                const Soil&, double) const
{ return 0.0; }

void
SoilWaterRect::initialize (const AttributeList& al,
                           const GeometryRect& geo,
                           const Soil& soil,
                           Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWaterRect");
  initialize_base (al, geo, soil, msg);

  const size_t size = geo.cell_size ();

  if (h_.size () == 0)
    {
      const double h_pF2 = -100.0; // pF 2.0;
      for (size_t i = 0; i < size; i++)
        {
          h_.push_back (h_pF2);
          Theta_.push_back (soil.Theta (i, h_pF2, h_ice (i)));
        }
    }
  daisy_assert (h_.size () == size);
  daisy_assert (h_.size () == Theta_.size ());

  // We just assume no changes.
  Theta_old_ = Theta_;
  h_old_ = h_;
}

void
SoilWaterRect::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  load_base (syntax, alist);
  alist.add ("submodel", "SoilWaterrect");
  alist.add ("description", "Water content of soil in a rectangular grid.");
}

SoilWaterRect::SoilWaterRect (Block& al)
  : SoilWater (al)
{ }
  
SoilWaterRect::~SoilWaterRect ()
{ }

static Submodel::Register 
soil_water_rect_submodel ("SoilWaterRect", SoilWaterRect::load_syntax);
