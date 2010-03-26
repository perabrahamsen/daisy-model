// weather_base.C
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "weather_base.h"
#include "fao.h"
#include "assertion.h"
#include "time.h"
#include "log.h"
#include "mathlib.h"
#include "librarian.h"
#include "block_model.h"
#include "units.h"
#include "frame.h"

// WeatherBase

double
WeatherBase::latitude () const
{ return latitude_; }

double
WeatherBase::longitude () const
{ return longitude_; }

double
WeatherBase::elevation () const	// [m]
{ return elevation_; }

double
WeatherBase::timezone () const
{ return timezone_; }

double
WeatherBase::screen_height () const
{ return screen_height_; }

Weather::surface_t 
WeatherBase::surface () const
{ return surface_; }

void
WeatherBase::tick (const Time& time, Treelog&)
{
  // Day length.
  day_length_ = day_length (time);
  day_cycle_ = day_cycle (time);
}

void 
WeatherBase::tick_after (const Time& time, Treelog&)
{
  // Hourly claudiness.
  const double Si = global_radiation (); 
  const double rad = HourlyExtraterrestrialRadiation (time);
  if (Si > 25.0 && rad > 25.0)
    {
      cloudiness_ = FAO::CloudinessFactor_Humid (Si, rad);
      daisy_assert (cloudiness_ >= 0.0);
      daisy_assert (cloudiness_ <= 1.0);
    }

  // Daily claudiness.
  if (time.hour () == 0)
    {
      const double Si = daily_global_radiation () ;
      const double rad = ExtraterrestrialRadiation (time);
      if (Si > 25.0 && rad > 25.0)
	{
	  daily_cloudiness_ = FAO::CloudinessFactor_Humid (Si, rad);
	  daisy_assert (daily_cloudiness_ >= 0.0);
	  daisy_assert (daily_cloudiness_ <= 1.0);
	}
    }

  // Deposition.
  const double Precipitation = rain () + snow (); // [mm]
  daisy_assert (Precipitation >= 0.0);
  
  const Unit& u_flux = units.get_unit (IM::flux_unit ());
  const Unit& u_solute = units.get_unit (IM::solute_unit ());
  const Unit& u_precip = units.get_unit (Units::mm_per_h ());
  const IM dry (u_flux, DryDeposit);
  const IM solute (u_solute, WetDeposit);
  const IM wet (solute.multiply (Scalar (Precipitation, u_precip), u_flux));
  deposit_ = dry + wet;
}

void
WeatherBase::output (Log& log) const
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
  output_submodule (deposit_, "deposit", log);
}

IM
WeatherBase::deposit () const // [g [stuff] /cm²/h]
{ return deposit_; }

double
WeatherBase::CO2 () const // [Pa]
{ 
#if 1
  const double standard_pressure = FAO::AtmosphericPressure (0.0);
  return 35.0 * air_pressure () / standard_pressure; 
#else
  return 35.0;
#endif
}

double
WeatherBase::O2 () const // [Pa]
{
#if 1
  const double standard_pressure = FAO::AtmosphericPressure (0.0);
  return 20500.0 * air_pressure () / standard_pressure; 
#else
  return 20500.0;
#endif
}

double
WeatherBase::air_pressure () const // [Pa]
{ return FAO::AtmosphericPressure (elevation ()); }

bool 
WeatherBase::has_reference_evapotranspiration () const
{ return false; }

bool 
WeatherBase::has_vapor_pressure () const
{ return false; }

bool 
WeatherBase::has_diffuse_radiation () const
{ return false; }

bool 
WeatherBase::has_relative_humidity () const
{ return false; }

bool 
WeatherBase::has_wind () const
{ return false; }

double 
WeatherBase::timestep () const
{ return -42.42e42; }

bool 
WeatherBase::has_min_max_temperature () const
{ return false; }

double 
WeatherBase::day_cycle (const Time& time) const	// Sum over a day is 1.0.
{
  // Day length.
  const double dl = day_length (time);
  daisy_assert (dl >= 0.0);
  daisy_assert (dl <= 24.0);
  
  // Day error sum
  double sum = 0.0;
  for (int i = 0; i < 24; i++)
    sum += std::max (0.0, M_PI_2 / dl * cos (M_PI * (i + 0.5 - 12) / dl));

  const double hour = time.hour () + 0.5; // Value in the middle of time step.

  // Day cycle.
  const double dc = std::max (0.0, M_PI_2 / dl * cos (M_PI * (hour - 12) / dl));
  daisy_assert (dc >= 0.0);
  daisy_assert (dc <= 1.0);  

  return dc / sum;
}

double
WeatherBase::day_length (const Time& time) const
{
  double t = 2 * M_PI / 365 * time.yday ();

  const double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		      - 0.03885 * cos (2 * t)
		      + 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t)
		      + 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  double my_tan 
    = -tan (M_PI / 180.0 * Dec) * tan (M_PI / 180.0 * latitude ());
  if (my_tan <= -1.0)
    my_tan = -1.0;
  else if (my_tan >= 1.0)
    my_tan = 1.0;
  t = (24 / M_PI * acos (my_tan));
  const double dl = (t < 0) ? t + 24.0 : t;
  daisy_assert (dl >= 0.0);
  daisy_assert (dl <= 24.0);
  return dl;
}

double
WeatherBase::T_normal (const Time& time, double delay) const
{
  const double rad_per_day = 2.0 * M_PI / 365.0;

  daisy_assert (T_average > -400);
  return T_average
    + T_amplitude
    * exp (delay)
    * cos (rad_per_day * (time.yday () + (time.hour () - 15) / 24.0
                          - max_Ta_yday) + delay);
}

double 
WeatherBase::average_temperature () const
{ 
  daisy_assert (T_average > -400);
  return T_average; 
}

double
WeatherBase::SolarDeclination (const Time& time) // [rad]
{
  return (0.409 * sin (2.0 * M_PI * time.yday () / 365.0 - 1.39));
}

double
WeatherBase::RelativeSunEarthDistance (const Time& time)
{
  return (1.0 + 0.033 * cos (2.0 * M_PI * time.yday () / 365.0));
}

double
WeatherBase::SunsetHourAngle (double Dec, double Lat) // [rad]
{
  return (acos (-tan (Dec) * tan (Lat)));
}

const double SolarConstant = 1366.7; // {W/m2]

double
WeatherBase::ExtraterrestrialRadiation (const Time& time) const // [W/m2]
{
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * latitude ();
  const double x1 = SunsetHourAngle (Dec, Lat) * sin (Lat) * sin (Dec);
  const double x2 = cos (Lat) * cos (Dec) * sin (SunsetHourAngle (Dec, Lat));
  return (SolarConstant * RelativeSunEarthDistance (time) * (x1 + x2) / M_PI);
}

double
WeatherBase::HourlyExtraterrestrialRadiation (const Time& time) const // [W/m2]
{
  return (SolarConstant * RelativeSunEarthDistance (time) 
          * sin_solar_elevation_angle (time));
}

double
WeatherBase::sin_solar_elevation_angle (const Time& time) const // []
{
  static const double EQT0   = 0.002733;
  static const double EQT1[] = {-7.343,-9.470,-0.3289,-0.1955};
  static const double EQT2[] = {0.5519,-3.020,-0.07581,-0.1245};
  const double Dec = SolarDeclination (time);
  
  const double Lat = M_PI / 180.0 * latitude ();
  const double timelag = (timezone () - longitude ()) / 15.0;
  double EQT = EQT0;
  for (unsigned int i = 0; i < 3; i++)
    {
       const double P = 2.0 * M_PI / 365.0 * (i+1) * time.yday();
       EQT += EQT1[i] * sin(P) + EQT2[i] * cos(P);
    }
  EQT /= 60.0;
  const double SunHourAngle = M_PI / 12.0 
    * (time.hour() + 0.5 + EQT - timelag + 12);
  return (sin(Lat)*sin(Dec) + cos(Lat)*cos(Dec)*cos(SunHourAngle));
}

bool
WeatherBase::initialize (const Time&, Treelog &)
{ return true; }

bool
WeatherBase::check (const Time&, const Time&, Treelog&) const
{ return true; }


WeatherBase::WeatherBase (const BlockModel& al)
  : Weather (al),
    units (al.units ()),
    latitude_ (-42.42e42),
    longitude_ (-42.42e42),
    elevation_ (-42.42e42),
    timezone_ (-42.42e42),
    surface_ (reference),
    screen_height_ (2.0),
    DryDeposit (units.get_unit (dry_deposit_unit ())),
    WetDeposit (units.get_unit (Units::ppm ())),
    T_average (-42.42e42),           // May be used before Weather::check.
    T_amplitude (-42.42e42),
    max_Ta_yday (-42.42e42),
    day_length_ (-42.42e42),
    day_cycle_ (-42.42e42),
    cloudiness_ (0.0),	// It may be dark at the start.
    daily_cloudiness_ (0.0),
    deposit_ (al, "deposit")
{ }

static struct WeatherBaseSyntax : public DeclareBase
{
  WeatherBaseSyntax ()
    : DeclareBase (Weather::component, "base", "\
This is not a model, but a list of parameters shared by all weather models.")
  { }
  static void load_flux (Frame& frame)
  { IM::add_syntax (frame, Attribute::LogOnly, IM::flux_unit ()); }
  void load_frame (Frame& frame) const
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
} WeatherBase_syntax;

// weather_base.C ends here.
