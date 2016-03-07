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
#include "frame.h"
#include "block_model.h"
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
  output_value (N (), "N", log);
  output_value (N_fixated (), "N_fixated", log);
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
  output_value (EpFactorDry (), "EpFactorDry", log);
  output_value (EpFactorWet (), "EpFactorWet", log);
  output_value (albedo (), "albedo", log);
  output_value (interception_capacity (), "interception_capacity", log);
  output_value (shadow_stomata_conductance (), 
                "shadow_stomata_conductance", log);
  output_value (sunlit_stomata_conductance (),
                "sunlit_stomata_conductance", log);
}

Vegetation::Vegetation (const BlockModel& al)
  : ModelDerived (al.type_name ()),
    EpInterchange_ (al.number ("EpInterchange"))
{ }

Vegetation::~Vegetation ()
{ }

static struct VegetationInit : public DeclareComponent 
{
  VegetationInit ()
    : DeclareComponent (Vegetation::component, "\
That green stuff.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare ("N", "kg N/ha", Attribute::LogOnly,
                   "Current nitrogen content of vegetation.");
    frame.declare ("N_fixated", "kg N/ha/h", Attribute::LogOnly,
                   "Nitrigen fixation rate.");
    frame.declare ("LAI", "m^2/m^2", Attribute::LogOnly,
                "Total LAI of all crops on this column");
    frame.declare ("height", "cm", Attribute::LogOnly,
                "Max crop height in canopy");
    frame.declare ("cover", "m^2/m^2", Attribute::LogOnly,
                "Fraction of soil covered by crops");
    frame.declare ("LAIvsH", "m^2/m^2", "cm", Attribute::LogOnly,
                "Total canopy LAI below given height");
    frame.declare ("HvsLAI", "cm", "m^2/m^2", Attribute::LogOnly, "\
Height in which there is a given LAI below in total canopy");
    frame.declare ("ACExt_PAR", Attribute::None (), Attribute::LogOnly,
                "Canopy extinction coefficient of PAR\
\n(how fast the light dim as a function of LAI passed)");
    frame.declare ("ACRef_PAR", Attribute::None (), Attribute::LogOnly,
                "Canopy reflection coefficient of PAR");
    frame.declare ("ACExt_NIR", Attribute::None (), Attribute::LogOnly,
                "Canopy extinction coefficient of NIR\
\n(how fast the light dim as a function of LAI passed)");
     frame.declare ("ACRef_NIR", Attribute::None (), Attribute::LogOnly,
                "Canopy reflection coefficient of NIR");
    frame.declare ("ARExt", Attribute::None (), Attribute::LogOnly,
                "Radiation Extinction coefficient\
\n(like ACExt, but for all radiation, not just light)");
    frame.declare ("EpFactorDry", Attribute::None (), Attribute::LogOnly,
                "Reference to potential evapotranspiration");
    frame.declare ("EpFactorWet", Attribute::None (), Attribute::LogOnly,
                "Reference to potential evapotranspiration");
    frame.declare_fraction ("EpInterchange", Attribute::Const, "\
Canopy adsorbtion fraction of unreached potential soil evaporation.");
    frame.set ("EpInterchange", 0.6);
    frame.declare ("albedo", Attribute::None (), Attribute::LogOnly,
                "Another reflection factor");
    frame.declare ("interception_capacity", "mm", Attribute::LogOnly,
                "Canopy water storage capacity");
    frame.declare ("shadow_stomata_conductance", "m/s", Attribute::LogOnly,
                   "Field based stomata conductance of shadow leaves.");
    frame.declare ("sunlit_stomata_conductance", "m/s", Attribute::LogOnly,
                   "Field based stomata conductance of sunlit leaves.");
  }
} Vegetation_init;

// vegetation.C ends here.
