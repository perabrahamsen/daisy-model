// weather_extra.C --- Overwrite some weather values.
// 
// Copyright 2010 KU.
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
#include "librarian.h"
#include "log.h"
#include "frame.h"
#include "block_model.h"
#include "lexer_table.h"
#include "units.h"
#include <boost/scoped_ptr.hpp>
#include <sstream>

struct WeatherExtra : public Weather
{
  // Content.
  const Units& units;
  const boost::scoped_ptr<Weather> weather;
  LexerTable lex;
  Time last_read;
  bool no_more_data;
  const double max_lookahead;
  bool using_default_weather;

  int rain_c;
  int snow_c;
  double rain_value;
  double snow_value;

  double latitude () const
  { return weather->latitude (); }
  double longitude () const 
  { return weather->longitude (); }
  double elevation () const
  { return weather->elevation (); }
  double timezone () const
  { return weather->timezone (); }
  double screen_height () const
  { return weather->screen_height (); }
  Weatherdata::surface_t surface () const
  { return weather->surface (); }
  
  // Simulation.
  void tick (const Time& time, Treelog& msg);
  void output (Log& log) const;

  // Communication with Bioclimate.
  double air_temperature () const
  { return weather->air_temperature (); }
  double daily_air_temperature () const
  { return weather->daily_air_temperature (); }
  double daily_max_air_temperature () const
  { return weather->daily_max_air_temperature (); }
  double daily_min_air_temperature () const
  { return weather->daily_min_air_temperature (); }
  double global_radiation () const
  { return weather->global_radiation (); }
  double daily_global_radiation () const
  { return weather->daily_global_radiation (); }
  double diffuse_radiation () const
  { return weather->diffuse_radiation (); }
  double reference_evapotranspiration () const
  { return weather->reference_evapotranspiration (); }
  double daily_precipitation () const
  { return weather->daily_precipitation (); }
  double rain () const
  { 
    if (rain_value < 0)
      return weather->rain (); 

    return rain_value;
  }
  double snow () const
  { 
    if (snow_value < 0.0)
      return weather->snow (); 

    return snow_value;
  }
  const IM& deposit () const
  { return weather->deposit (); }
  double cloudiness () const
  { return weather->cloudiness (); }
  double vapor_pressure () const
  { return weather->vapor_pressure (); }
  double relative_humidity () const
  { return weather->relative_humidity (); }
  double wind () const
  { return weather->wind (); }
  double CO2 () const
  { return weather->CO2 (); }
  double O2 () const
  { return weather->O2 (); }
  double air_pressure () const
  { return weather->air_pressure (); }
  
  // Initializing bioclimate.
  bool has_reference_evapotranspiration () const
  { return weather->has_reference_evapotranspiration (); }
  bool has_vapor_pressure () const
  { return weather->has_vapor_pressure (); }
  bool has_wind () const
  { return weather->has_wind (); }
  bool has_min_max_temperature () const
  { return weather->has_min_max_temperature (); }
  bool has_diffuse_radiation () const
  { return weather->has_diffuse_radiation (); }
  bool has_relative_humidity () const
  { return weather->has_relative_humidity (); }
  double timestep () const
  { return weather->timestep (); }

  // Light distribution.
  double day_length () const
  { return weather->day_length (); }

  // Communication with SoilHeat.
  double T_normal (const Time& time, double delay = 0.0) const
  { return weather->T_normal (time, delay); }

  // OrganicMatter initialization.
  double average_temperature () const
  { return weather->average_temperature (); }

  // Create and Destroy.
  bool initialize (const Time& time, Treelog& msg);
  bool check (const Time& from, const Time& to, Treelog& msg) const;
  WeatherExtra (const BlockModel& al);
};

void 
WeatherExtra::tick (const Time& time, Treelog& msg)
{ 
  weather->tick (time, msg); 

  if (no_more_data)
    return;

  if (time <= last_read)
    return;
  
  // Read entries.
  std::vector<std::string> entries;
  if (!lex.get_entries (entries)
      || !lex.get_time (entries, last_read, 11))
    {
      lex.warning ("Switching back to default weather.");
      no_more_data = true;
      using_default_weather = true;
      snow_value = rain_value = -42.42e42;
      return;
    };
  
  const double days_ahead= Time::days_between (time, last_read);
  if (days_ahead > max_lookahead)
    {
      if (!using_default_weather)
        {
          using_default_weather = true;
          snow_value = rain_value = -42.42e42;
          std::ostringstream tmp;
          tmp << "Next extra weather data are " << days_ahead << " days ahead."
              << "  Using default weather";
          msg.message (tmp.str ());
        }
      return;
    }

  if (using_default_weather)
    {
      using_default_weather = false;
      msg.message ("Extra weather data available");
    }
  
  if (rain_c < 0)
    rain_value = 0.0;
  else
    rain_value = units.convert (lex.dimension (rain_c),
                                Units::mm_per_h (),
                                lex.convert_to_double (entries[rain_c]));
  if (snow_c < 0)
    snow_value = 0.0;
  else
    snow_value = units.convert (lex.dimension (snow_c),
                                Units::mm_per_h (),
                                lex.convert_to_double (entries[snow_c]));
}


void 
WeatherExtra::output (Log& log) const
{
  output_common (log);
  output_derived (weather, "weather", log);
}

bool 
WeatherExtra::initialize (const Time& time, Treelog& msg) 
{ 
  TREELOG_MODEL (msg);
  
  bool ok = true;

  if (!weather->initialize (time, msg))
    ok = false;

  if (!lex.read_header (msg))
    ok = false;

  rain_c = lex.find_tag ("Rain");
  snow_c = lex.find_tag ("Snow");

  return ok;
}

bool 
WeatherExtra::check (const Time& from, const Time& to, Treelog& msg) const
{ 
  TREELOG_MODEL (msg);

  bool ok =  weather->check (from, to, msg); 
  
  if (rain_c < 0)
    {
      if (snow_c < 0)
        msg.warning ("No weather data found");
      else
        msg.warning ("No rain, only snow");
    }

  return ok;
}

WeatherExtra::WeatherExtra (const BlockModel& al)
  : Weather (al),
    units (al.units ()),
    weather (Librarian::build_item<Weather> (al, "weather")),
    lex (al),
    last_read (1, 1, 1, 0),
    no_more_data (false),
    max_lookahead (al.number ("max_lookahead")),
    using_default_weather (false),
    rain_c (-42),
    snow_c (-42),
    rain_value (-42.42e42),
    snow_value (-42.42e42)
{ }

static struct WeatherExtraSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WeatherExtra (al); }
  WeatherExtraSyntax ()
    : DeclareModel (Weather::component, "extra",
                    "Add extra weather data from a file.\n\
This will overwrite the current values specified in a normal weather file.\n\
Note that daily averages will still be taken from normal weather file, so\n\
subsystems depending of those will act strangely.\n\
\n\
The following columns are recognized:\n\
\n\
Rain, Snow:  If exactly one of these are specified, the other is assumed\n\
to be zero.  Using either may conflict with the markvand action model.")
  { }
  void load_frame (Frame& frame) const
  { 
    LexerTable::load_syntax (frame);
    Weather::load_common (frame);

    frame.declare_object ("weather", Weather::component,
                          Attribute::State, Attribute::Singleton, "\
Get weather data from this file when not specified otherwise.");
    frame.declare ("max_lookahead", "d", Attribute::Const, "\
Use extra weather data if the next entry is less than this period ahead.");
    frame.set ("max_lookahead", 1.1);
  }
} WeatherExtra_syntax;

// weather_extra.C ends here.
