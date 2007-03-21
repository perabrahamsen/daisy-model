// vegetation.C
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


#include "vegetation.h"
#include "log.h"
#include "syntax.h"
#include "block.h"

const char *const Vegetation::description = "\
That green stuff.";

const char *const Vegetation::component = "vegetation";

double 
Vegetation::shared_light_fraction () const
{ return 1.0; }

double
Vegetation::EpInterchange () const
{ return EpInterchange_; }

void
Vegetation::output (Log& log) const
{
  output_value (LAI (), "LAI", log);
  output_value (height (), "height", log);
  output_value (cover (), "cover", log);
  output_value (LAIvsH (), "LAIvsH", log);
  output_value (HvsLAI (), "HvsLAI", log);
  output_value (ACExt (), "ACExt", log);
  output_value (ACRef (), "ACRef", log);
  output_value (ARExt (), "ARExt", log);
  output_value (ARExt (), "ARExt", log);
  output_value (EpFactor (), "EpFactor", log);
  output_value (albedo (), "albedo", log);
  output_value (interception_capacity (), "interception_capacity", log);
}

void
Vegetation::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this vegetation.");
  syntax.add ("LAI", "m^2/m^2", Syntax::LogOnly,
	      "Total LAI of all crops on this column");
  syntax.add ("height", "cm", Syntax::LogOnly,
	      "Max crop height in canopy");
  syntax.add ("cover", "m^2/m^2", Syntax::LogOnly,
	      "Fraction of soil covered by crops");
  syntax.add ("LAIvsH", "m^2/m^2", "cm", Syntax::LogOnly,
	      "Total canopy LAI below given height");
  syntax.add ("HvsLAI", "cm", "m^2/m^2", Syntax::LogOnly, "\
Height in which there is a given LAI below in total canopy");
  syntax.add ("ACExt", Syntax::None (), Syntax::LogOnly,
	      "Canopy extinction coefficient\
\n(how fast the light dim as a function of LAI passed)");
   syntax.add ("ACRef", Syntax::None (), Syntax::LogOnly,
	      "Canopy reflection coefficient");
  syntax.add ("ARExt", Syntax::None (), Syntax::LogOnly,
	      "Radiation Extinction coefficient\
\n(like ACExt, but for all radiation, not just light)");
  syntax.add ("EpFactor", Syntax::None (), Syntax::LogOnly,
	      "Reference to potential evapotranspiration");
  syntax.add_fraction ("EpInterchange", Syntax::Const, "\
Canopy adsorbtion fraction of unreached potential soil evaporation.");
  alist.add ("EpInterchange", 0.6);
  syntax.add ("albedo", Syntax::None (), Syntax::LogOnly,
	      "Another reflection factor");
  syntax.add ("interception_capacity", "mm", Syntax::LogOnly,
	      "Canopy water storage capacity");
}

Vegetation::Vegetation (Block& al)
  : name (al.identifier ("type")),
    EpInterchange_ (al.number ("EpInterchange"))
{ }

Vegetation::~Vegetation ()
{ }
