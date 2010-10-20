// weather.C
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

#include "weather.h"
#include "block_model.h"
#include "librarian.h"
#include "log.h"
#include "im.h"

const char *const Weather::component = "weather";

symbol
Weather::library_id () const
{
  static const symbol id (component);
  return id;
}

const symbol 
Weather::dry_deposit_unit ()
{
  static const symbol unit ("kg/ha/y");
  return unit;
}

void 
Weather::output_common (Log& log) const
{
  output_value (air_temperature (), "air_temperature", log);
  output_value (daily_air_temperature (), "daily_air_temperature", log);
  output_value (daily_min_air_temperature (),
                "daily_min_air_temperature", log);
  output_value (daily_max_air_temperature (), 
                "daily_max_air_temperature", log);
  output_value (global_radiation (), "global_radiation", log);
  output_value (daily_global_radiation (), "daily_global_radiation", log);
  if (has_reference_evapotranspiration ())
    output_value (reference_evapotranspiration (), 
                  "reference_evapotranspiration", log);
  output_value (rain (), "rain", log);
  output_value (snow (), "snow", log);
  output_value (rain () + snow (), "precipitation", log);
  output_value (cloudiness (), "cloudiness", log);
  output_value (daily_cloudiness (), "daily_cloudiness", log);
  output_value (vapor_pressure (), "vapor_pressure", log);
  output_value (air_pressure (), "air_pressure", log);
  output_value (diffuse_radiation (), "diffuse_radiation", log);
  output_value (relative_humidity (), "relative_humidity", log);
  output_value (wind (), "wind", log);
  output_value (day_length (), "day_length", log);
  output_value (day_cycle (), "day_cycle", log);
  output_submodule (deposit (), "deposit", log);
}

static void load_flux (Frame& frame)
{ IM::add_syntax (frame, Attribute::LogOnly, IM::flux_unit ()); }

void 
Weather::load_common (Frame& frame)
{
  // Overwritten in weather_none.C
  frame.declare ("air_temperature", "dg C", Attribute::LogOnly,
                 "Temperature this hour.");
  frame.declare ("global_radiation", "W/m^2", Attribute::LogOnly,
                 "Global radiation this hour.");

  // Logs.
  frame.declare ("daily_air_temperature", "dg C", Attribute::LogOnly,
                 "Average temperature this day.");
  frame.declare ("daily_min_air_temperature", "dg C", Attribute::LogOnly,
                 "Minumum temperature this day.");
  frame.declare ("daily_max_air_temperature", "dg C", Attribute::LogOnly,
                 "Maximum temperature this day.");
  frame.declare ("daily_global_radiation", "W/m^2", Attribute::LogOnly,
                 "Average radiation this day.");
  frame.declare ("diffuse_radiation", "W/m^2", Attribute::LogOnly,
                 "Diffuse radiation this hour.");
  frame.declare ("reference_evapotranspiration", "mm/h", Attribute::LogOnly,
                 "Reference evapotranspiration this hour");
  frame.declare ("daily_extraterrastial_radiation", "W/m^2", Attribute::LogOnly,
                 "Extraterrestrial radiation this day.");
  frame.declare ("rain", "mm/h", Attribute::LogOnly, "Rain this hour.");
  frame.declare ("snow", "mm/h", Attribute::LogOnly, "Snow this hour.");
  frame.declare ("precipitation", "mm/h", Attribute::LogOnly, 
                 "Precipitation this hour.");
  frame.declare_fraction ("cloudiness", Attribute::LogOnly,
                          "Fraction of sky covered by clouds [0-1].");
  frame.declare_fraction ("daily_cloudiness", Attribute::LogOnly,
                          "Fraction of sky covered by clouds [0-1].");
  frame.declare ("vapor_pressure", "Pa", Attribute::LogOnly, "Humidity.");
  frame.declare ("air_pressure", "Pa", Attribute::LogOnly, "Air pressure.");
  frame.declare ("relative_humidity", Attribute::Fraction (), Attribute::LogOnly,
                 "Relative humidity.");
  frame.declare ("wind", "m/s", Attribute::LogOnly, "Wind speed.");
  frame.declare ("day_length", "h", Attribute::LogOnly,
                 "Number of light hours this day.");
  frame.declare ("day_cycle", Attribute::None (), Attribute::LogOnly,
                 "Fraction of daily radiation received this hour.");
  frame.declare_submodule_sequence ("deposit", Attribute::LogOnly, "\
Total atmospheric deposition of nitrogen.", load_flux);
}

Weather::Weather (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Weather::~Weather ()
{ }

static struct WeatherInit : public DeclareComponent 
{
  WeatherInit ()
    : DeclareComponent (Weather::component, "\
Meteorological data, as well as the global positioning, are the\n\
responsibility of the 'weather' component, typically be reading the\n\
data from a file.  The meteorological data are common to all columns.")
  { }
} Weather_init;


// weather.C ends here.
