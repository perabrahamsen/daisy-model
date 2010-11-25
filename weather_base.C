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

Weatherdata::surface_t 
WeatherBase::surface () const
{ return surface_; }

void
WeatherBase::tick (const Time& time, Treelog&)
{
  // Day length.
  day_length_ = Weather::day_length (time);
}

void 
WeatherBase::tick_after (const Time& time, Treelog&)
{
  // Hourly claudiness.
  const double Si = global_radiation (); 
  const double rad = extraterrestrial_radiation (time);
  if (Si > 25.0 && rad > 25.0)
    {
      cloudiness_ = FAO::CloudinessFactor_Humid (Si, rad);
      daisy_assert (cloudiness_ >= 0.0);
      daisy_assert (cloudiness_ <= 1.0);
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
{ output_common (log); }

const IM&
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

#if 0
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

  const double hour = time.day_fraction () * 24.0;

  // Day cycle.
  const double dc = std::max (0.0, M_PI_2 / dl * cos (M_PI * (hour - 12) / dl));
  daisy_assert (dc >= 0.0);
  daisy_assert (dc <= 1.0);  

  return dc / sum;
}
#endif

double
WeatherBase::T_normal (const Time& time, double delay) const
{
  const double displacement = time.year_fraction () - max_Ta_yday / 365.0;

  daisy_assert (delay <= 0);
  daisy_assert (T_average > -400);
  return T_average
    + T_amplitude
    * exp (delay)
    * cos (2.0 * M_PI * displacement + delay);
}

double 
WeatherBase::average_temperature () const
{ 
  daisy_assert (T_average > -400);
  return T_average; 
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
    surface_ (Weatherdata::reference),
    screen_height_ (2.0),
    DryDeposit (units.get_unit (dry_deposit_unit ())),
    WetDeposit (units.get_unit (Units::ppm ())),
    T_average (-42.42e42),           // May be used before Weather::check.
    T_amplitude (-42.42e42),
    max_Ta_yday (-42.42e42),
    day_length_ (-42.42e42),
    cloudiness_ (0.0),	// It may be dark at the start.
    deposit_ (al, "deposit")
{ }

static struct WeatherBaseSyntax : public DeclareBase
{
  WeatherBaseSyntax ()
    : DeclareBase (Weather::component, "base", "\
This is not a model, but a list of parameters shared by all weather models.")
  { }
  void load_frame (Frame& frame) const
  { Weather::load_common (frame); }
} WeatherBase_syntax;

// weather_base.C ends here.
