// weather_none.C
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


#include "weather_old.h"
#include "block.h"
#include "alist.h"
#include "librarian.h"

class WeatherNone : public WeatherOld
{
  double air_temperature;
  double global_radiation;
  double reference_evapotranspiration_;
  double rain_;
  double snow_;

  // Simulation.
public:
  void tick (const Time& t, Treelog& o)
  { WeatherOld::tick (t, o); Weather::tick_after (t, o); }
  double hourly_air_temperature () const
  { return air_temperature; }
  double daily_air_temperature () const
  { return air_temperature; }
  double daily_max_air_temperature () const
  { return air_temperature; }
  double daily_min_air_temperature () const
  { return air_temperature; }
  double hourly_global_radiation () const
  { return global_radiation; }
  double daily_global_radiation () const
  { return global_radiation; }
  double reference_evapotranspiration () const
  { return reference_evapotranspiration_; }
  double daily_precipitation () const 
  { return (rain_ + snow_) * 24; }
  double rain () const
  { return rain_; }
  double snow () const
  { return snow_; }

  // Create and Destroy.
public:
  WeatherNone (Block&);
  ~WeatherNone ();
};

WeatherNone::WeatherNone (Block& al)
  : WeatherOld (al),
    air_temperature (al.number ("air_temperature")),
    global_radiation (al.number ("global_radiation")),
    reference_evapotranspiration_ (al.number ("reference_evapotranspiration_value")),
    rain_ (al.number ("rain_value")),
    snow_ (al.number ("snow_value"))
{ }

WeatherNone::~WeatherNone ()
{ }

// Add the WeatherNone syntax to the syntax table.
static struct WeatherNoneSyntax
{
  static Model& make (Block& al)
  { return *new WeatherNone (al); }
  WeatherNoneSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Weather that does not change during the simulation.");
    WeatherOld::load_syntax (syntax, alist);
    syntax.add ("air_temperature", "dg C", Syntax::Const,
		"Constant air temperature");
    alist.add ("air_temperature", 0.0);
    syntax.add ("global_radiation", "W/m^2", Syntax::Const,
		"Constant global radiation.");
    alist.add ("global_radiation", 0.0);
    // These must be Syntax::State because they are logged in
    // Weather::output.  Otherwise, we get an error at checkpoins.
    syntax.add ("reference_evapotranspiration_value", "mm/h", Syntax::Const,
		"Constant reference evapotranspiration.");
    alist.add ("reference_evapotranspiration_value", 0.0);
    syntax.add ("rain_value", "mm/h", Syntax::Const, "Constant rain.");
    alist.add ("rain_value", 0.0);
    syntax.add ("snow_value", "mm/h", Syntax::Const, "Constant snow.");
    alist.add ("snow_value", 0.0);
    Librarian::add_type (Weather::component, "none", alist, syntax, make);
  }
} WeatherNone_syntax;
