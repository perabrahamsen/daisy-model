// soil_heat_rect.C -- Heat in a rectangular soil grid.
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

#define BUILD_DLL

#include "soil_heat_rect.h"
#include "geometry_rect.h"
#include "submodel.h"
#include "alist.h"

double 
SoilHeatRect::top_flux (const Geometry&,
                        const Soil&, const SoilWater&) const
{ return 0.0; }
  
double 
SoilHeatRect::T_surface_snow (const Geometry&,
                              const Soil&,
                              const SoilWater&,
                              double T_snow,
                              double /* K_snow */,
                              double /* dZs */) const
{ return T_snow; }

void
SoilHeatRect::output (Log& log) const
{ output_base (log); }

void 
SoilHeatRect::load_syntax (Syntax& syntax, AttributeList& alist)
{
  load_base (syntax, alist); 
  alist.add ("submodel", "SoilHeatRect");
  alist.add ("description", "Heat of soil in a rectangular grid.");
}

SoilHeatRect::SoilHeatRect (const Block& al)
  : SoilHeat (al)
{ }

void 
SoilHeatRect::initialize (const AttributeList& al, 
                          const GeometryRect& geo,
                          Treelog& msg)
{ 
  initialize_base (al, geo, msg);

  while (T_.size () < geo.cell_size ())
    T_.push_back (10.0);
}

SoilHeatRect::~SoilHeatRect ()
{ }

static Submodel::Register 
soil_heat_rect_submodel ("SoilHeatRect", SoilHeatRect::load_syntax);
