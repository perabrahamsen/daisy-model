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


#include "weather.h"
#include "fao.h"
#include "time.h"
#include "log.h"
#include "mathlib.h"

EMPTY_TEMPLATE
Librarian<Weather>::Content* Librarian<Weather>::content = NULL;

const char *const Weather::description = "\
Meteorological data, as well as the global positioning, are the\n\
responsibility of the 'weather' component, typically be reading the\n\
data from a file.  The meteorological data are common to all columns.";

const double SolarConstant = 1366.7; // {W/m2]

double
Weather::elevation () const	// [m];
{ return elevation_; }

double
Weather::screen_height () const	// [m];
{ return screen_height_; }

void
Weather::tick (const Time& time, Treelog&)
{
  // Day length.
  day_length_ = day_length (time);
  day_cycle_ = day_cycle (time);
}

void 
Weather::tick_after (const Time& time, Treelog&)
{
  // Hourly claudiness.
  const double Si = hourly_global_radiation (); 
  const double rad = HourlyExtraterrestrialRadiation (time);
  if (Si > 25.0 && rad > 25.0)
    {
      hourly_cloudiness_ = FAO::CloudinessFactor_Humid (Si, rad);
      daisy_assert (hourly_cloudiness_ >= 0.0);
      daisy_assert (hourly_cloudiness_ <= 1.0);
    }

  // Daily claudiness.
  if (time.hour () == 0.0)
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
  
  // [kg N/ha/year -> [g/cm²/h]
  const double hours_to_years = 365.2425 * 24.0;
  const double kg_per_ha_to_g_cm2 
    = 1000.0 / ((100.0 * 100.0) * (100.0 * 100.0));
  const IM dry (DryDeposit, kg_per_ha_to_g_cm2 / hours_to_years);
  const IM wet (WetDeposit, 1.0e-7); // [ppm] -> [g/cm²/mm]

  deposit_ = dry + wet * Precipitation;
  daisy_assert (deposit_.NO3 >= 0.0);
  daisy_assert (deposit_.NH4 >= 0.0);

  daisy_assert (approximate (deposit_.NO3, 
		       DryDeposit.NO3 * kg_per_ha_to_g_cm2/hours_to_years
		       + Precipitation * WetDeposit.NO3 * 1e-7));
  daisy_assert (approximate (deposit_.NH4, 
		       DryDeposit.NH4 * kg_per_ha_to_g_cm2/hours_to_years
		       + Precipitation * WetDeposit.NH4 * 1e-7));
}

void
Weather::output (Log& log) const
{
  output_value (hourly_air_temperature (), "hourly_air_temperature", log);
  output_value (daily_air_temperature (), "daily_air_temperature", log);
  output_value (daily_min_air_temperature (),
                "daily_min_air_temperature", log);
  output_value (daily_max_air_temperature (), 
                "daily_max_air_temperature", log);
  output_value (hourly_global_radiation (), "hourly_global_radiation", log);
  output_value (daily_global_radiation (), "daily_global_radiation", log);
  output_value (reference_evapotranspiration (), 
		"reference_evapotranspiration", log);
  output_value (rain (), "rain", log);
  output_value (snow (), "snow", log);
  output_value (rain () + snow (), "precipitation", log);
  output_value (hourly_cloudiness (), "hourly_cloudiness", log);
  output_value (daily_cloudiness (), "daily_cloudiness", log);
  output_value (vapor_pressure (), "vapor_pressure", log);
  output_value (wind (), "wind", log);
  output_value (day_length (), "day_length", log);
  output_value (day_cycle (), "day_cycle", log);
  output_submodule (deposit_, "deposit", log);
}

IM
Weather::deposit () const // [g [stuff] /cm²/h]
{ return deposit_; }

bool 
Weather::has_reference_evapotranspiration () const
{ return false; }

bool 
Weather::has_vapor_pressure () const
{ return false; }

bool 
Weather::has_wind () const
{ return false; }

bool 
Weather::has_min_max_temperature () const
{ return false; }

double 
Weather::day_cycle (const Time& time) const	// Sum over a day is 1.0.
{
  // Day length.
  const double dl = day_length (time);
  daisy_assert (dl >= 0.0);
  daisy_assert (dl <= 24.0);
  
  const double hour = time.hour () + 0.5; // Value in the middle of time step.

  // Day cycle.
  double dc;
  if (hour > 12 + dl || hour < 12 - dl)
    dc = 0.0;
  else
    dc = max (0.0, M_PI_2 / dl * cos (M_PI * (hour - 12) / dl));
  daisy_assert (dc >= 0.0);
  daisy_assert (dc <= 1.0);  

  return dc;
}

double
Weather::day_length (const Time& time) const
{
  double t = 2 * M_PI / 365 * time.yday ();

  const double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		      - 0.03885 * cos (2 * t)
		      + 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t)
		      + 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  double my_tan 
    = -tan (M_PI / 180.0 * Dec) * tan (M_PI / 180.0 * latitude);
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
Weather::T_normal (const Time& time, double delay) const
{
  const double rad_per_day = 2.0 * M_PI / 365.0;

  return T_average
    + T_amplitude
    * exp (delay)
    * cos (rad_per_day * (time.yday () - max_Ta_yday) + delay);
}

double 
Weather::average_temperature () const
{ return T_average; }

double
Weather::SolarDeclination (const Time& time) // [rad]
{
  return (0.409 * sin (2.0 * M_PI * time.yday () / 365.0 - 1.39));
}

double
Weather::RelativeSunEarthDistance (const Time& time)
{
  return (1.0 + 0.033 * cos (2.0 * M_PI * time.yday () / 365.0));
}

double
Weather::SunsetHourAngle (double Dec, double Lat) // [rad]
{
  return (acos (-tan (Dec) * tan (Lat)));
}

double
Weather::ExtraterrestrialRadiation (const Time& time) const // [W/m2]
{
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * latitude;
  const double x1 = SunsetHourAngle (Dec, Lat) * sin (Lat) * sin (Dec);
  const double x2 = cos (Lat) * cos (Dec) * sin (SunsetHourAngle (Dec, Lat));
  return (SolarConstant * RelativeSunEarthDistance (time) * (x1 + x2) / M_PI);
}

double
Weather::HourlyExtraterrestrialRadiation (const Time& time) const // [W/m2]
{
  static const double EQT0   = 0.002733;
  static const double EQT1[] = {-7.343,-9.470,-0.3289,-0.1955};
  static const double EQT2[] = {0.5519,-3.020,-0.07581,-0.1245};
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * latitude;
  const double timelag = (timezone - longitude) / 15.0;
  double EQT = EQT0;
  for (unsigned int i = 0; i < 3; i++)
    {
       const double P = 2.0 * M_PI / 365.0 * (i+1) * time.yday();
       EQT += EQT1[i] * sin(P) + EQT2[i] * cos(P);
    }
  EQT /= 60.0;
  const double SunHourAngle = M_PI / 12.0 
    * (time.hour() + 0.5 + EQT - timelag + 12);
  return ( SolarConstant * RelativeSunEarthDistance (time) *
            (sin(Lat)*sin(Dec) + cos(Lat)*cos(Dec)*cos(SunHourAngle)));
}

void
Weather::initialize (const Time&, Treelog &)
{ }

Weather::Weather (const AttributeList& al)
  : name (al.identifier ("type")),
    latitude (-42.42e42),
    longitude (-42.42e42),
    elevation_ (-42.42e42),
    timezone (-42.42e42),
    surface (Surface::reference),
    screen_height_ (2.0),
    T_average (10.0),           // May be used before Weather::check.
    T_amplitude (-42.42e42),
    max_Ta_yday (-42.42e42),
    day_length_ (-42.42e42),
    day_cycle_ (-42.42e42),
    hourly_cloudiness_ (0.0),	// It may be dark at the start.
    daily_cloudiness_ (0.0)
{
  WetDeposit.NO3 = -42.42e42;
  WetDeposit.NH4 = -42.42e42;
  DryDeposit.NO3 = -42.42e42;
  DryDeposit.NH4 = -42.42e42;
}

Weather::~Weather ()
{ }

bool
Weather::check (const Time&, const Time&, Treelog&) const
{ return true; }

void
Weather::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Logs.
  syntax.add ("hourly_air_temperature", "dg C", Syntax::LogOnly,
	      "Temperature this hour.");
  syntax.add ("daily_air_temperature", "dg C", Syntax::LogOnly,
	      "Average temperature this day.");
  syntax.add ("daily_min_air_temperature", "dg C", Syntax::LogOnly,
	      "Minumum temperature this day.");
  syntax.add ("daily_max_air_temperature", "dg C", Syntax::LogOnly,
	      "Maximum temperature this day.");
  syntax.add ("hourly_global_radiation", "W/m^2", Syntax::LogOnly,
	      "Global radiation this hour.");
  syntax.add ("daily_global_radiation", "W/m^2", Syntax::LogOnly,
	      "Average radiation this day.");
  syntax.add ("reference_evapotranspiration", "mm/h", Syntax::LogOnly,
	      "Reference evapotranspiration this hour");
  syntax.add ("daily_extraterrastial_radiation", "W/m^2", Syntax::LogOnly,
	      "Extraterrestrial radiation this day.");
  syntax.add ("rain", "mm/h", Syntax::LogOnly, "Rain this hour.");
  syntax.add ("snow", "mm/h", Syntax::LogOnly, "Snow this hour.");
  syntax.add ("precipitation", "mm/h", Syntax::LogOnly, 
	      "Precipitation this hour.");
  syntax.add_fraction ("hourly_cloudiness", Syntax::LogOnly,
	      "Fraction of sky covered by clouds [0-1].");
  syntax.add_fraction ("daily_cloudiness", Syntax::LogOnly,
	      "Fraction of sky covered by clouds [0-1].");
  syntax.add ("vapor_pressure", "Pa", Syntax::LogOnly, "Humidity.");
  syntax.add ("wind", "m/s", Syntax::LogOnly, "Wind speed.");
  syntax.add ("day_length", "h", Syntax::LogOnly,
	      "Number of light hours this day.");
  syntax.add ("day_cycle", Syntax::None (), Syntax::LogOnly,
	      "Fraction of daily radiation received this hour.");
  syntax.add_submodule ("deposit", alist, Syntax::LogOnly, 
			"\
Total atmospheric deposition of nitrogen this hour [g N/cm^2/h].", 
			&IM::load_soil_flux);
}

