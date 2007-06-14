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
#include "block.h"
#include "alist.h"
#include "fao.h"
#include "time.h"
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
  Implementation (Block& al)
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
  Weather::tick (time, out);
  impl.time = time;
}

void
WeatherOld::output (Log& log) const
{ Weather::output (log); }

double
WeatherOld::hourly_global_radiation () const
{ return (day_cycle () * 24.0) * daily_global_radiation (); }

double
WeatherOld::daily_global_radiation () const
{ return impl.daily_global_radiation; }

double
WeatherOld::hourly_diffuse_radiation () const
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
  const double T = hourly_air_temperature ();
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
WeatherOld::hourly_air_temperature () const
{
  // BUG: Should add some kind of day cycle.  
  return daily_air_temperature (); 
}

double
WeatherOld::reference_evapotranspiration () const
{
  return FAO::Makkink (hourly_air_temperature (), hourly_global_radiation ());
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

WeatherOld::WeatherOld (Block& al)
  : Weather (al),
    impl (*new Implementation (al))
{ 
  latitude = al.number ("Latitude");
  longitude = al.number ("Longitude");
  elevation_ = al.number ("Elevation");
  timezone = al.number ("TimeZone");
  screen_height_ = al.number ("ScreenHeight");
  T_average = al.number ("average");
  T_amplitude = al.number ("amplitude");
  max_Ta_yday = al.number ("max_Ta_yday");
  DryDeposit = IM (al.alist ("DryDeposit"));
  WetDeposit = IM (al.alist ("WetDeposit"));
}

WeatherOld::~WeatherOld ()
{ 
  delete &impl;
}

static bool 
check_alist (const AttributeList& al, Treelog& err)
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

void
WeatherOld::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Weather::load_syntax (syntax, alist);
  syntax.add_check (check_alist);
  // Where in the world are we?
  syntax.add ("Latitude", "dg North", Syntax::Const,
	      "The position of the weather station on the globe.");
  alist.add ("Latitude", 56.0);
  syntax.add ("Longitude", "dg East", Syntax::Const,
	      "The position of the weather station on the globe.");
  alist.add ("Longitude", 10.0);
  syntax.add ("Elevation", "m", Syntax::Const,
	      "Height above sea level.");
  alist.add ("Elevation", 0.0);
  syntax.add ("TimeZone", "dg East", Syntax::Const,
	      "Time zone in effect (no DST).");
  alist.add ("TimeZone", 15.0);
  syntax.add ("ScreenHeight", "m", Syntax::Const,
	      "Measurement height above ground.");
  alist.add ("ScreenHeight", 2.0);
  syntax.add ("UTM_x", Syntax::Unknown (), Syntax::OptionalConst,
	      "X position of weather station."); // Unused.
  syntax.add ("UTM_y", Syntax::Unknown (), Syntax::OptionalConst,
	      "Y position of weather station."); // Unused.

  syntax.add_submodule ("DryDeposit", alist, Syntax::Const, 
			"\
Dry atmospheric deposition of nitrogen [kg N/ha/y].", &IM::load_field_flux);
  syntax.add_submodule ("WetDeposit", alist, Syntax::Const, 
			"\
Deposition of nitrogen solutes with precipitation [ppm].", &IM::load_ppm);

  // Division between Rain and Snow.
  syntax.add ("T_rain", "dg C", Syntax::Const, 
	      "Above this air temperature all precipitation is rain.");
  alist.add ("T_rain", 2.0);
  syntax.add ("T_snow", "dg C", Syntax::Const,
	      "Below this air temperature all precipitation is snow.");
  alist.add ("T_snow", -2.0);

  // Yearly average temperatures.
  syntax.add ("average", "dg C", Syntax::Const,
	      "Average temperature at this location.");
  alist.add ("average", 7.8);
  syntax.add ("amplitude", "dg C", Syntax::Const,
	      "How much the temperature change during the year.");
  alist.add ("amplitude", 8.5);
  syntax.add ("max_Ta_yday", "d", Syntax::Const,
	      "Julian day where the highest temperature is expected.");
  alist.add ("max_Ta_yday", 209.0);
}

