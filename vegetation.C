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

#define BUILD_DLL

#include "vegetation.h"
#include "log.h"
#include "syntax.h"
#include "block.h"
#include "librarian.h"

const char *const Vegetation::component = "vegetation";

symbol
Vegetation::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol 
Vegetation::all_crops ()
{
  static const symbol all_symbol ("all");
  return all_symbol;
}

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
  output_value (ACExt_PAR (), "ACExt_PAR", log);
  output_value (ACRef_PAR (), "ACRef_PAR", log);
  output_value (ACExt_NIR (), "ACExt_NIR", log);
  output_value (ACRef_NIR (), "ACRef_NIR", log);
  output_value (ARExt (), "ARExt", log);
  output_value (ARExt (), "ARExt", log);
  output_value (EpFactor (), "EpFactor", log);
  output_value (albedo (), "albedo", log);
  output_value (interception_capacity (), "interception_capacity", log);
  output_value (stomata_conductance (), "stomata_conductance", log);
}

void
Vegetation::load_base (Syntax& syntax, AttributeList& alist)
{
  alist.add ("base_model", "common");
  syntax.add ("description", Value::String, Value::OptionalConst,
	      "Description of this vegetation.");
  syntax.add ("LAI", "m^2/m^2", Value::LogOnly,
	      "Total LAI of all crops on this column");
  syntax.add ("height", "cm", Value::LogOnly,
	      "Max crop height in canopy");
  syntax.add ("cover", "m^2/m^2", Value::LogOnly,
	      "Fraction of soil covered by crops");
  syntax.add ("LAIvsH", "m^2/m^2", "cm", Value::LogOnly,
	      "Total canopy LAI below given height");
  syntax.add ("HvsLAI", "cm", "m^2/m^2", Value::LogOnly, "\
Height in which there is a given LAI below in total canopy");
  syntax.add ("ACExt_PAR", Value::None (), Value::LogOnly,
	      "Canopy extinction coefficient of PAR\
\n(how fast the light dim as a function of LAI passed)");
   syntax.add ("ACRef_PAR", Value::None (), Value::LogOnly,
	      "Canopy reflection coefficient of PAR");
  syntax.add ("ACExt_NIR", Value::None (), Value::LogOnly,
	      "Canopy extinction coefficient of NIR\
\n(how fast the light dim as a function of LAI passed)");
   syntax.add ("ACRef_NIR", Value::None (), Value::LogOnly,
	      "Canopy reflection coefficient of NIR");
  syntax.add ("ARExt", Value::None (), Value::LogOnly,
	      "Radiation Extinction coefficient\
\n(like ACExt, but for all radiation, not just light)");
  syntax.add ("EpFactor", Value::None (), Value::LogOnly,
	      "Reference to potential evapotranspiration");
  syntax.add_fraction ("EpInterchange", Value::Const, "\
Canopy adsorbtion fraction of unreached potential soil evaporation.");
  alist.add ("EpInterchange", 0.6);
  syntax.add ("albedo", Value::None (), Value::LogOnly,
	      "Another reflection factor");
  syntax.add ("interception_capacity", "mm", Value::LogOnly,
	      "Canopy water storage capacity");
  syntax.add ("stomata_conductance", "m/s", Value::LogOnly,
              "Stomata´conductance");
}

Vegetation::Vegetation (Block& al)
  : ModelLogable (al.name ("type")),
    EpInterchange_ (al.number ("EpInterchange"))
{ }

Vegetation::~Vegetation ()
{ }

static Librarian Vegetation_init (Vegetation::component, "\
That green stuff.");

static struct VegetationSyntax
{
  VegetationSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Vegetation::load_base (syntax, alist);

    Librarian::add_base (Vegetation::component, alist, syntax);
  }
} Vegetation_syntax;

// vegetation.C ends here.
