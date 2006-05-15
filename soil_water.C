// soil_water.C
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


#include "soil_water.h"
#include "geometry.h"
#include "log.h"

double 
SoilWater::content (const Geometry& geo, double from, double to) const
{ return geo.total (Theta_, from, to); }

void 
SoilWater::output_base (Log& log) const
{
  output_variable (h_, log);
  output_variable (Theta_, log);
  output_variable (S_sum_, log);
  output_variable (S_root_, log);
  output_variable (S_drain_, log);
  output_variable (X_ice_, log);
}

void
SoilWater::load_base (Syntax& syntax, AttributeList&)
{
  Geometry::add_layer (syntax, Syntax::OptionalState, 
                       "h", "cm", "Soil water pressure.");
  Geometry::add_layer (syntax, Syntax::OptionalState,
                       "Theta", Syntax::Fraction (),
                       "Soil water content.");
  syntax.add ("S_sum", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Total water sink (due to root uptake and macropores).");
  syntax.add ("S_root", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to root uptake.");
  syntax.add ("S_drain", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to soil drainage.");
  syntax.add_fraction ("X_ice", Syntax::OptionalState, Syntax::Sequence,
		       "Ice volume fraction in soil.");
}

SoilWater::SoilWater (Block&)
{ }

SoilWater::~SoilWater ()
{ }

