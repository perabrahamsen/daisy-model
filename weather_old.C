// weather_old.C --- Common code for old weather models.
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

#include "weather_old.h"
#include "block_model.h"
#include "frame.h"
#include "librarian.h"
#include "fao.h"
#include "time.h"
#include "units.h"
#include "treelog.h"
#include <sstream>

struct WeatherOld::Implementation
{
  // Snow Model.
  const double T_rain;
  const double T_snow;

  // State.
  double daily_global_radiation; // [W/m²]
  double Prain;
  double Psnow;
  Time time;

  // Create and Destroy.
  Implementation (const BlockModel& al)
    : T_rain (al.number ("T_rain")),
      T_snow (al.number ("T_snow")),
      daily_global_radiation (-42.42e42),
      Prain (0.0),
      Psnow (0.0),
      time (1, 1, 1, 1)
    { }
};

void 
WeatherOld::tick (const Time& time, Treelog& out)
{
  WeatherBase::tick (time, out);
  impl.time = time;
}

void
WeatherOld::output (Log& log) const
{ WeatherBase::output (log); }

double
WeatherOld::global_radiation () const
{ return relative_extraterestial_radiation (impl.time)
    * daily_global_radiation (); }

double
WeatherOld::daily_global_radiation () const
{ return impl.daily_global_radiation; }

double
WeatherOld::diffuse_radiation () const
{ return 0.0; }

double
WeatherOld::rain () const
{
  return impl.Prain;
}

double
WeatherOld::snow () const
{
  return impl.Psnow;
}

void 
WeatherOld::distribute (double precipitation)
{
  const double T = air_temperature ();
  if (T < impl.T_snow)
    impl.Psnow = precipitation;
  else if (impl.T_rain < T)
    impl.Psnow = 0.0;
  else
    impl.Psnow
      = precipitation * (impl.T_rain - T) / (impl.T_rain - impl.T_snow);

  impl.Prain = precipitation - snow ();
}

double
WeatherOld::air_temperature () const
{
  // BUG: Should add some kind of day cycle.  
  return daily_air_temperature (); 
}

double
WeatherOld::reference_evapotranspiration () const
{
  return FAO::Makkink (air_temperature (), global_radiation ());
}

double 
WeatherOld::vapor_pressure () const
{ 
  const double T_min = daily_air_temperature () - 5.0;
  return FAO::SaturationVapourPressure (T_min);
}

double 
WeatherOld::relative_humidity () const
{ return 0.5; }

double 
WeatherOld::wind () const
{ return 3.0; }

WeatherOld::WeatherOld (const BlockModel& al)
  : WeatherBase (al),
    impl (*new Implementation (al))
{ 
  latitude_ = al.number ("Latitude");
  longitude_ = al.number ("Longitude");
  elevation_ = al.number ("Elevation");
  timezone_ = al.number ("TimeZone");
  screen_height_ = al.number ("ScreenHeight");
  T_average = al.number ("average");
  T_amplitude = al.number ("amplitude");
  max_Ta_yday = al.number ("max_Ta_yday");
  DryDeposit = IM (al, "DryDeposit");
  WetDeposit = IM (al, "WetDeposit");
}

WeatherOld::~WeatherOld ()
{ 
  delete &impl;
}

static struct WeatherOldSyntax : public DeclareBase 
{
  WeatherOldSyntax ()
    : DeclareBase (Weather::component, "old", "base", "\
Shared parameters for old models.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  {
    bool ok = true;
    const double latitude = al.number ("Latitude");

    if (latitude > 66 || latitude < -66)
      {
        std::ostringstream tmp;
        tmp << "Warning, Daisy is untested under arctic conditions "
               << "(Latitude = " << latitude << ")";
        err.entry (tmp.str ());
      }

    return ok;
  }
  static void load_dry (Frame& frame)
  { IM::add_syntax (frame, Attribute::Const, Weather::dry_deposit_unit ()); }

  static void load_ppm (Frame& frame)
  { IM::add_syntax (frame, Attribute::Const, Units::ppm ()); }

  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    // Where in the world are we?
    frame.declare ("Latitude", "dg North", Attribute::Const,
                "The position of the weather station on the globe.");
    frame.set ("Latitude", 56.0);
    frame.declare ("Longitude", "dg East", Attribute::Const,
                "The position of the weather station on the globe.");
    frame.set ("Longitude", 10.0);
    frame.declare ("Elevation", "m", Attribute::Const,
                "Height above sea level.");
    frame.set ("Elevation", 0.0);
    frame.declare ("TimeZone", "dg East", Attribute::Const,
                "Time zone in effect (no DST).");
    frame.set ("TimeZone", 15.0);
    frame.declare ("ScreenHeight", "m", Attribute::Const,
                "Measurement height above ground.");
    frame.set ("ScreenHeight", 2.0);
    frame.declare ("UTM_x", Attribute::Unknown (), Attribute::OptionalConst,
                "X position of weather station."); // Unused.
    frame.declare ("UTM_y", Attribute::Unknown (), Attribute::OptionalConst,
                "Y position of weather station."); // Unused.
    frame.declare_submodule_sequence ("DryDeposit", Attribute::Const, "\
Atmospheric deposition.", load_dry);
    frame.set_empty ("DryDeposit");
    frame.declare_submodule_sequence ("WetDeposit", Attribute::Const, "\
Deposition of solutes with precipitation.", load_ppm);
    frame.set_empty ("WetDeposit");

    // Division between Rain and Snow.
    frame.declare ("T_rain", "dg C", Attribute::Const, 
                "Above this air temperature all precipitation is rain.");
    frame.set ("T_rain", 2.0);
    frame.declare ("T_snow", "dg C", Attribute::Const,
                "Below this air temperature all precipitation is snow.");
    frame.set ("T_snow", -2.0);

    // Yearly average temperatures.
    frame.declare ("average", "dg C", Attribute::Const,
                "Average temperature at this location.");
    frame.set ("average", 7.8);
    frame.declare ("amplitude", "dg C", Attribute::Const,
                "How much the temperature change during the year.");
    frame.set ("amplitude", 8.5);
    frame.declare ("max_Ta_yday", "d", Attribute::Const,
                "Julian day where the highest temperature is expected.");
    frame.set ("max_Ta_yday", 209.0);
  }
} WeatherOld_syntax;

// weather_old.C ends here.
