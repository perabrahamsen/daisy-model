// weather_hourly.C
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
#include "time.h"
#include "log.h"
#include "mathlib.h"
#include <fstream>

struct WeatherHourly : public WeatherOld
{
  Time date;
  const string file_name;
  ifstream file;
  int line;			// Current line number in weather file.

  // Read values.
  double precipitation;
  double global_radiation;
  double air_temperature;
  double vapor_pressure_;
  double wind_;

  // Accumulated values.
  double accumulated_global_radiation;
  double accumulated_max_air_temperature;
  double accumulated_min_air_temperature;
  double accumulated_air_temperature;
  unsigned int accumulated_count;
  double daily_air_temperature_;
  double daily_max_air_temperature_;
  double daily_min_air_temperature_;

  // Simulation.
  void tick (const Time&, Treelog&);

  // Communication with Bioclimate.
  double daily_air_temperature () const
    { return daily_air_temperature_; }
  double daily_max_air_temperature () const
    { return daily_max_air_temperature_; }
  double daily_min_air_temperature () const
    { return daily_min_air_temperature_; }
  double hourly_air_temperature () const
    { return air_temperature; }
  double hourly_global_radiation () const
    { return global_radiation; }
  double vapor_pressure () const
    { return vapor_pressure_; }
  double wind () const 
    { return wind_; }

  void put_air_temperature (double T)
    { air_temperature = T; }

  void put_reference_evapotranspiration (double)
    { }

  WeatherHourly (const AttributeList& al)
    : WeatherOld (al),
      date (42, 1, 1, 0),
      file_name (al.name ("file")),
      file (Options::find_file (al.name ("file"))),
      line (0),
      precipitation (-42.42e42),
      global_radiation (-42.42e42),
      air_temperature (-42.42e42),
      vapor_pressure_ (-42.42e42),
      wind_ (-42.42e42),
      accumulated_global_radiation (0.0),
      accumulated_air_temperature (0.0),
      accumulated_max_air_temperature (-1000.0),
      accumulated_min_air_temperature (1000.0),
      accumulated_count (0),
      daily_air_temperature_ (-42.42e42)
      daily_max_air_temperature_ (-42.42e42)
      daily_min_air_temperature_ (42.42e42)
    { }

  ~WeatherHourly ()
    { close (file.rdbuf ()->fd ()); }
};

void
WeatherHourly::tick (const Time& time, Treelog& out)
{ 
  WeatherOld::tick (time, out);

  if (!file.good ())
    {
      std::ostringstream tmp;
      tmp << file_name << ":" << line << ": file error";
      out.error (tmp.str ());
      throw ("read error");
    }
  int year;
  int month; 
  int day;
  int hour;

  while (date < time)
    {
      double cloudiness_;

      file >> year >> month >> day >> hour
	   >> global_radiation >> air_temperature >> precipitation
	   >> cloudiness_ >> vapor_pressure_ >> wind_;
      if (year < 100)
	year += 1900;
      while (file.good () && file.get () != '\n')
	/* do nothing */;

      if (!file.good ())
	throw ("No more climate data.");

      date = Time (year, month, day, hour);

      daisy_assert (global_radiation >= 0 && global_radiation < 1400);
      daisy_assert (air_temperature >= -70 && air_temperature < 60);
      daisy_assert (precipitation >= 0 && precipitation < 300);
      daisy_assert (cloudiness_ >= 0 && cloudiness_ <= 1);
      if (!approximate (cloudiness_, hourly_cloudiness ()))
	{
	  std::ostringstream tmp;
	  tmp << "cloudiness read (" << cloudiness_ 
		 << ") != calculated (" << hourly_cloudiness () << ")";
	  out.error (tmp.str ());
	}
      daisy_assert (vapor_pressure_ >= 0 && vapor_pressure_ <= 5000);
      daisy_assert (wind_ >= 0 && wind_ <= 40);

      accumulated_global_radiation += global_radiation;
      accumulated_air_temperature += air_temperature;
      if (air_temperature > accumulated_max_air_temperature)
	accumulated_max_air_temperature = air_temperature;
      if (air_temperature < accumulated_min_air_temperature)
	accumulated_min_air_temperature = air_temperature;

      accumulated_count++;	// BUGLET: We ignore missing values.

      if (hour == 0)
	{
	  put_global_radiation (accumulated_global_radiation 
				/ accumulated_count);
	  daily_air_temperature_ 
	    = accumulated_air_temperature / accumulated_count;
          daily_min_air_temperature_ = accumulated_min_air_temperature;
          daily_max_air_temperature_ = accumulated_max_air_temperature;
	  accumulated_global_radiation = 0.0;
	  accumulated_air_temperature = 0.0;
	  accumulated_max_air_temperature = -1000.0;
	  accumulated_min_air_temperature = 1000.0;
	  accumulated_count = 0;
	}
    }

  // Update the hourly values.
  distribute (precipitation);

  Weather::tick_after (time, out);
}

static struct WeatherHourlySyntax
{
  static Weather& make (const AttributeList& al)
    { return *new WeatherHourly (al); }

  WeatherHourlySyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Read weather data from a file.\n\
Each line should have the following whitespace separated fields:\n\
year, month, day, hour, global radiation [W/m^2], air temperature [dg C],\n\
precipitation [mm/h], cloudiness [0-1] and vapor pressure [Pa].");
      WeatherOld::load_syntax (syntax, alist);
      syntax.add ("file", Syntax::String, Syntax::Const,
		  "File to read weather data from.");
      syntax.order ("file");
      Librarian<Weather>::add_type ("hourly", alist, syntax, &make);
    }
} WeatherHourly_syntax;
