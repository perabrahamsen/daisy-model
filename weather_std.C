// weather_std.C --- Standard weather file.
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
#include "lexer_data.h"
#include "time.h"
#include "plf.h"
#include "tmpstream.h"
#include "mathlib.h"
#include "units.h"
#include "check.h"
#include "vcheck.h"
#include <vector>
#include <algorithm>
#include <numeric>
#include <set>

struct WeatherStandard : public Weather
{
  // Snow Model.
  const double T_rain;
  const double T_snow;

  // Missing years.
  struct YearMap
  {
    // Types.
    struct YearInterval
    {
      // Parameters.
      const int from;
      const int to;

      // Use.
      int size () const
      { return to - from + 1; }

      // Create and Destroy.
      static bool check_alist (const AttributeList& al, Treelog&);
      static void load_syntax (Syntax&, AttributeList&);
      YearInterval (const AttributeList&);
    };

    // Parameters.
    const YearInterval from;
    const YearInterval to;

    // Use.
    bool contain (const Time& time) const;
    void map_time (Time& time) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    YearMap (const AttributeList&);
  };
  const vector<const YearMap*> missing_years;
  int active_map;
  int find_map (const Time& time) const;

  // Keywords
  struct keyword_description_type
  {
    const char* name;
    const char* dim;
    double WeatherStandard::* value;
    double min;
    double max;
    bool required;
  };
  static keyword_description_type keyword_description[];
  static const int keyword_description_size;
  int timestep;
  Time begin;
  Time end;
  vector<double> precipitation_correction;

  // Extra parameters.
  vector<double> precipitation_scale;

  // Data description.
  struct data_description_type
  {
    const char* name;
    const char* dim;
    double WeatherStandard::* value;
    string WeatherStandard::* read;
    double min;
    double max;
    bool required;
  };
  static data_description_type data_description[];
  static const int data_description_size;
  vector<int> data_index;
  bool has_data (const string& name);
  bool has_date;
  bool has_hour;
  bool has_temperature;
  bool has_min_temperature;
  bool has_max_temperature;
  bool has_vapor_pressure_;
  bool has_relative_humidity;
  bool has_wind_speed;
  bool has_reference_evapotranspiration_;

  // Convertion.
  string air_temperature_read;
  string min_air_temperature_read;
  string max_air_temperature_read;
  string global_radiation_read;
  string precipitation_read;
  string vapor_pressure_read;
  string relative_humidity_read;
  string wind_speed_read;
  string reference_evapotranspiration_read;

  // Parsing.
  const string where;
  LexerData* lex;
  Lexer::Position end_of_header;

  // These are the last read values for today.
  Time last_time;
  double last_air_temperature;
  double last_min_air_temperature;
  double last_max_air_temperature;
  double last_global_radiation;
  double last_precipitation;
  double last_vapor_pressure;
  double last_relative_humidity;
  double last_wind_speed;
  double last_reference_evapotranspiration;

  // And for yesterday.
  double yesterday_T_max;

  // These are the first values after today.
  bool initialized;
  Time next_time;
  double next_year;
  double next_month;
  double next_day;
  double next_hour;
  double next_air_temperature;
  double next_min_air_temperature;
  double next_max_air_temperature;
  double next_global_radiation;
  double next_precipitation;
  double next_vapor_pressure;
  double next_relative_humidity;
  double next_wind_speed;
  double next_reference_evapotranspiration;

  // Data.
  int hour;
  double air_temperature_[24];
  double global_radiation_[24];
  double precipitation_[24];
  double vapor_pressure_[24];
  double wind_speed_[24];
  double reference_evapotranspiration_[24];

  // Daily averages.
  double daily_air_temperature_;
  double daily_max_air_temperature_;
  double daily_min_air_temperature_;
  double daily_global_radiation_;
  
  // Fractions this hour.
  double snow_fraction;
  double rain_fraction;

  // Simulation.
  void tick (const Time& time, Treelog&);
  void output (Log& log) const
  { Weather::output (log); }
  void read_line ();
  void read_new_day (const Time&, Treelog&);

  // Communication with Bioclimate.
  double hourly_air_temperature () const // [dg C]
  { 
    daisy_assert (initialized);
    return air_temperature_[hour]; 
  }
  double daily_air_temperature () const // [dg C]
  { 
    daisy_assert (initialized);
    return daily_air_temperature_; 
  }
  double daily_max_air_temperature () const // [dg C]
  { 
    daisy_assert (initialized);
    return daily_max_air_temperature_; 
  }
  double daily_min_air_temperature () const // [dg C]
  { 
    daisy_assert (initialized);
    return daily_min_air_temperature_; 
  }
  double hourly_global_radiation () const // [W/m2]
  { 
    daisy_assert (initialized);
    return global_radiation_[hour]; 
  }
  double daily_global_radiation () const // [W/m2]
  { 
    daisy_assert (initialized);
    return daily_global_radiation_; 
  }
  double reference_evapotranspiration () const // [mm/h]
  { 
    daisy_assert (initialized);
    return reference_evapotranspiration_[hour]; 
  }
  double rain () const	// [mm/h]
  { 
    daisy_assert (initialized);
    return precipitation_[hour] * rain_fraction; 
  }
  double snow () const	// [mm/h]
  { 
    daisy_assert (initialized);
    return precipitation_[hour] * snow_fraction; 
  }
  double vapor_pressure () const // [Pa]
  { 
    daisy_assert (initialized);
    return vapor_pressure_[hour]; 
  }
  double wind () const	// [m/s]
  { 
    daisy_assert (initialized);
    return wind_speed_[hour]; 
  }

  bool has_reference_evapotranspiration () const
  { return has_reference_evapotranspiration_; }

  bool has_vapor_pressure () const
  { return has_vapor_pressure_; }

  bool has_wind () const
  { return has_wind_speed; }

  bool has_min_max_temperature () const
  { return has_min_temperature && has_max_temperature; }

  // Communication with external model.
  void put_precipitation (double prec) // [mm/d]
  { precipitation_[hour] = prec / 24.0; }
  void put_air_temperature (double T) // [°C]
  { 
    air_temperature_[hour] = T; 
    daily_air_temperature_ = T;
  }
  void put_reference_evapotranspiration (double ref) // [mm/d]
  { reference_evapotranspiration_[hour] = ref; }
  void put_global_radiation (double radiation) // [W/m²]
  { 
    global_radiation_[hour] = radiation;
    daily_global_radiation_ = radiation; 
  }

  // Create and Destroy.
  void initialize (const Time& time, Treelog& err);
  WeatherStandard (const AttributeList&);
  ~WeatherStandard ();
  bool check (const Time& from, const Time& to, Treelog& err) const;
};

bool
WeatherStandard::YearMap::YearInterval::check_alist (const AttributeList& al,
						     Treelog& err)
{
  bool ok = true;

  const int from = al.integer ("from");
  const int to = al.integer ("to");
  if (from > to)
    {
      TmpStream tmp;
      tmp () << "Start year " << from << " comes after end year " << to;
      err.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
WeatherStandard::YearMap::YearInterval::load_syntax (Syntax& syntax, 
						     AttributeList&)
{
  syntax.add_check (check_alist);
  syntax.add ("from", Syntax::Integer, Syntax::Const,
	      "First year of interval.");
  syntax.add_check ("from", VCheck::valid_year ());
  syntax.add ("to", Syntax::Integer, Syntax::Const,
	      "First year of interval.");
  syntax.add_check ("to", VCheck::valid_year ());
  syntax.order ("from", "to");
}

WeatherStandard::YearMap::YearInterval::YearInterval (const AttributeList& al)
  : from (al.integer ("from")),
    to (al.integer ("to"))
{ }
    
bool WeatherStandard::YearMap::contain (const Time& time) const
{
  const int year = time.year ();
  return from.from <= year && year <= from.to;
}

void WeatherStandard::YearMap::map_time (Time& time) const
{ time.tick_year (to.from - from.from); }

bool
WeatherStandard::YearMap::check_alist (const AttributeList& al, Treelog& msg)
{
  bool ok = true;
  const YearInterval from (al.alist ("from"));
  const YearInterval to (al.alist ("to"));
  
  if (from.size () != to.size ())
    {
      TmpStream tmp;
      tmp () << "You cannot map " << from.size () << " years to "
	     << to.size () << " years";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
WeatherStandard::YearMap::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  syntax.add_submodule ("from", alist, Syntax::Const, 
			"Interval of years to map from.",
			YearInterval::load_syntax);
  syntax.add_submodule ("to", alist, Syntax::Const, 
			"Interval of years to map to.",
			YearInterval::load_syntax);
  syntax.order ("from", "to");
}

WeatherStandard::YearMap::YearMap (const AttributeList& al)
  : from (al.alist ("from")),
    to (al.alist ("to"))
{ }

int 
WeatherStandard::find_map (const Time& time) const
{ 
  for (int i = 0; i < missing_years.size (); i++)
    if (missing_years[i]->contain (time))
      return i;
  return -1;
}

WeatherStandard::keyword_description_type 
WeatherStandard::keyword_description[] =
  { { "Latitude", "dgNorth", &WeatherStandard::latitude, -90, 90, true },
    { "Longitude", "dgEast", &WeatherStandard::longitude, -360, 360, true },
    { "Elevation", "m", &WeatherStandard::elevation_, 0, 10000, true },
    { "TimeZone", "dgEast", &WeatherStandard::timezone, -360, 360, true },
    { "ScreenHeight", "m", &WeatherStandard::screen_height_, 0, 100, true },
    { "TAverage", "dgC", &WeatherStandard::T_average, -10, 40, true },
    { "TAmplitude", "dgC", &WeatherStandard::T_amplitude, 0, 100, true },
    { "MaxTDay", "yday", &WeatherStandard::max_Ta_yday, 1, 365, true } };

const int 
WeatherStandard::keyword_description_size 
/**/ = sizeof (WeatherStandard::keyword_description) 
  /**/ / sizeof (keyword_description_type);

WeatherStandard::data_description_type 
WeatherStandard::data_description[] =
  { { "Year", "year", &WeatherStandard::next_year, NULL,
      1, 9999, false },
    { "Month", "month", &WeatherStandard::next_month, NULL,
      1, 12, false },
    { "Day", "mday", &WeatherStandard::next_day, NULL,
      1, 31, false },
    { "Hour", "hour", &WeatherStandard::next_hour, NULL,
      0, 23, false },
    { "GlobRad", "W/m^2", &WeatherStandard::next_global_radiation,
      &WeatherStandard::global_radiation_read,
      0, 1400, true },
    { "AirTemp", "dgC", &WeatherStandard::next_air_temperature,
      &WeatherStandard::air_temperature_read,
      -70, 60, false },
    { "T_min", "dgC", &WeatherStandard::next_min_air_temperature,
      &WeatherStandard::min_air_temperature_read,
      -70, 60, false },
    { "T_max", "dgC", &WeatherStandard::next_max_air_temperature,
      &WeatherStandard::max_air_temperature_read,
      -70, 60, false },
    { "Precip", "mm/h", &WeatherStandard::next_precipitation,
      &WeatherStandard::precipitation_read,
      0, 300, true },
    { "RefEvap", "mm/h", &WeatherStandard::next_reference_evapotranspiration,
      &WeatherStandard::reference_evapotranspiration_read,
      -10, 20, false },
    { "VapPres", "Pa", &WeatherStandard::next_vapor_pressure,
      &WeatherStandard::vapor_pressure_read,
      0, 5000, false },
    { "RelHum", "fraction", &WeatherStandard::next_relative_humidity,
      &WeatherStandard::relative_humidity_read,
      0, 5000, false },
    { "Wind", "m/s", &WeatherStandard::next_wind_speed,
      &WeatherStandard::wind_speed_read,
      0, 40, false } };

const int 
WeatherStandard::data_description_size 
/**/ = sizeof (WeatherStandard::data_description) 
  /**/ / sizeof (data_description_type);

bool
WeatherStandard::has_data (const string& name)
{
  for (unsigned int i = 0; i < data_index.size (); i++)
    {
      int index = data_index[i];
      if (index >= 0 && name == data_description[index].name)
	return true;
    }
  return false;
}

void
WeatherStandard::tick (const Time& time, Treelog& msg)
{
  Treelog::Open nest (msg, "Weather: " + name);

  Weather::tick (time, msg);

  hour = time.hour ();
  
  if (hour == 0)
    read_new_day (time, msg);
  else if (!initialized)
    {
      Time midnight (time.year (), time.month (), time.mday (), 0);
      read_new_day (midnight, msg);
    }

  // Snow and rain fractions.
  const double T = hourly_air_temperature ();
  if (T < T_snow)
    {
      snow_fraction = 1.0;
      rain_fraction = 0.0;
    }
  else if (T_rain < T)
    {
      snow_fraction = 0.0;
      rain_fraction = 1.0;
    }
  else
    {
      snow_fraction = (T_rain - T) / (T_rain - T_snow);
      rain_fraction = 1.0 - snow_fraction;
    }
  daisy_assert (rain_fraction >= 0 && rain_fraction <= 1);
  daisy_assert (snow_fraction >= 0 && snow_fraction <= 1);
  daisy_assert (approximate (rain_fraction + snow_fraction, 1.0));

  Weather::tick_after (time, msg);
}

void 
WeatherStandard::read_line ()
{
  daisy_assert (lex);

  if (!lex->good ())
    {
      next_time.tick_hour ();
      return;
    }

  // Remember old values.
  last_time = next_time;
  last_air_temperature = next_air_temperature;
  last_min_air_temperature = next_min_air_temperature;
  last_max_air_temperature = next_max_air_temperature;
  last_global_radiation = next_global_radiation;
  last_precipitation = next_precipitation;
  last_vapor_pressure = next_vapor_pressure;
  last_relative_humidity = next_relative_humidity;
  last_wind_speed = next_wind_speed;
  last_reference_evapotranspiration = next_reference_evapotranspiration;

  while (true)
    {
      // Read new values.
      for (unsigned int i = 0; i < data_index.size (); i++)
	{
	  lex->skip_space ();
	  const int index = data_index[i];
	  if (index < 0)
	    continue;
	  const string dim = data_description[index].dim;
	  const string read = data_description[index].read
	    ? this->*(data_description[index].read) : dim;
	  const double value =  Units::convert (read, dim, lex->get_number ());
	  this->*(data_description[index].value) = value;
	  if (value < data_description[index].min)
	    lex->error (string ("Column ") 
                        + data_description[index].name + " value too low");
	  else if (value > data_description[index].max)
	    lex->error (string ("Column ") 
                        + data_description[index].name + " value too hight");
	  if (next_precipitation < 0.0)
	    next_precipitation = 0.0;
	  if (!lex->good ())
	    {
	      lex->error ("No more climate data, reusing last values");
	      return;
	    }
	}
      
      // Update time.
      if (timestep > 0)
	next_time.tick_hour (timestep);
      if (has_date)
	{
	  const int year = double2int (next_year);
	  const int month = double2int (next_month);
	  const int mday = double2int (next_day);
	  const int hour = has_hour 
	    ? double2int (next_hour) : next_time.hour ();

	  if (!Time::valid (year, month, mday, hour))
	    {
	      lex->error ("Invalid date");
	      lex->next_line ();
	      continue;
	    }
	  if (timestep > 0 && !(next_time == Time (year, month, mday, hour)))
	    {
	      lex->error ("Bad timestep");
	      next_time = Time (year, month, mday, hour);
	    }
	}
      break;
    }

  if (has_min_temperature && has_max_temperature
      && next_min_air_temperature > next_max_air_temperature)
    lex->warning ("T_min > T_max");
  if (has_min_temperature && has_temperature
      && next_min_air_temperature > next_air_temperature)
    lex->warning ("T_min > AirTemp");
  if (has_temperature && has_max_temperature
      && next_air_temperature > next_max_air_temperature)
    lex->warning ("AirTemp > T_max");
  
  lex->next_line ();

  if (!lex->good ())
    {
      lex->error ("End of climate data, reusing last values");
      return;
    }

  // Precipitation correction.
  const int month = double2int (next_time.month ());
  next_precipitation *= precipitation_correction[month-1]
    * precipitation_scale[month-1];
}

void 
WeatherStandard::read_new_day (const Time& time, Treelog& msg)
{ 
  daisy_assert (time.hour () == 0);

  // Handle missing years.
  bool find_new_map = false;
  if (active_map >= 0)
    {
      if (time.between (begin, end))
	{
	  msg.message ("Using current data");
	  active_map = -1;
	  initialized = false;
	}
      else if (!missing_years[active_map]->contain (time))
	find_new_map = true;
    }
  else if (!time.between (begin, end))
    find_new_map = true;

  if (find_new_map)
    {
      active_map = find_map (time);
      if (active_map >= 0)
	{
	  TmpStream tmp;
	  tmp () << "Using data from [" << missing_years[active_map]->to.from
		 << "-" << missing_years[active_map]->to.to << "] for years ["
		 << missing_years[active_map]->from.from << "-"
		 << missing_years[active_map]->from.to << "]";
	  msg.message (tmp.str ());
	  initialized = false;
	}
    }

  // Initialize.
  if (!initialized)
    {
      initialized = true;
      lex->seek (end_of_header);
      next_time = begin;
      next_time.tick_hour (-timestep);
      read_line ();
    }
  
  // Now.
  Time now = time;
  if (active_map >= 0)
    {
      missing_years[active_map]->map_time (now);
      if (!now.between (begin, end))
        {
	  TmpStream tmp;
	  tmp () << "No mapped weather data for " << now.year () 
		 << "-" << now.month ()
		 << "-" << now.mday () << ":" << now.hour ();
	  msg.error (tmp.str ());
          now = time;
        }
    }
  if (!now.between (begin, end))
    {
      TmpStream tmp;
      tmp () << "No weather data for " << time.year () 
             << "-" << time.month ()
             << "-" << time.mday () << ":" << time.hour ()
             << "\nReusing yesterdays data.";
      msg.error (tmp.str ());
      return;
    }

  // Tomorrow.
  Time tomorrow = now;
  tomorrow.tick_day ();

  // BC5 sucks // while (next_time <= now)
  while (!(now < next_time))
    read_line ();

  bool long_timestep;
  while (true)
    {
      Time end = (next_time < tomorrow) ? next_time : tomorrow;

      const int hours = Time::hours_between (last_time, next_time);
      long_timestep = (hours > 12);
      for (;now < end; now.tick_hour ())
	{
	  int hour = now.hour ();
	  if (has_min_temperature && has_max_temperature)
            {
              if (long_timestep)
                {
                  if (yesterday_T_max < -400.0)
                    // First time.
                    yesterday_T_max = last_max_air_temperature;

                  const double t_min = 12.0 - day_length ();
                  const double t_max = 15.0;
                  PLF T;
                  T.add (15 - 24.0, yesterday_T_max);
                  T.add (t_min, last_min_air_temperature);
                  T.add (t_max, last_max_air_temperature);
                  T.add (24.0 + t_min, next_min_air_temperature);
                  air_temperature_[hour] = T (hour);
                  yesterday_T_max = last_max_air_temperature;
                }
              else if (has_temperature)
                air_temperature_[hour] = last_air_temperature;
              else
                air_temperature_[hour] = (last_min_air_temperature 
                                          + last_max_air_temperature) / 2.0;
            }
	  else if (has_temperature)
	    air_temperature_[hour] = last_air_temperature;
          else 
	    air_temperature_[hour] = T_normal (now);
	  global_radiation_[hour] = last_global_radiation;
	  if (long_timestep)
	    global_radiation_[hour] *= day_cycle (now) * 24.0;
	  precipitation_[hour] = last_precipitation;
	  if (has_vapor_pressure_)
	    vapor_pressure_[hour] = last_vapor_pressure;
	  else if (has_relative_humidity)
	    vapor_pressure_[hour] 
	      = FAO::SaturationVapourPressure (air_temperature_[hour])
	      * last_relative_humidity;
	  if (has_wind_speed)
	    wind_speed_[hour] = last_wind_speed;
	  else
	    wind_speed_[hour] = 3.0;
	  if (has_reference_evapotranspiration_)
	    {
	      reference_evapotranspiration_[hour] 
		= last_reference_evapotranspiration;
	      if (long_timestep)
		reference_evapotranspiration_[hour] *= day_cycle (now) * 24.0;
	    }
	  else
	    reference_evapotranspiration_[hour] 
	      = FAO::Makkink (air_temperature_[hour], global_radiation_[hour]);
	}
      // BC5 sucks // if (next_time >= tomorrow)
      if (!(next_time < tomorrow))
	break;
      read_line ();
    }

  daily_global_radiation_ 
    = long_timestep
    ? last_global_radiation
    : accumulate (&global_radiation_[0], &global_radiation_[24], 0.0) / 24.0;
  daily_air_temperature_
    = (long_timestep && has_temperature)
    ? last_air_temperature
    : accumulate (&air_temperature_[0], &air_temperature_[24], 0.0) / 24.0;
  daily_max_air_temperature_
    = has_max_temperature 
    ? last_max_air_temperature
    : *max_element (&air_temperature_[0], &air_temperature_[24]);
  daily_min_air_temperature_
    = has_min_temperature 
    ? last_min_air_temperature
    : *min_element (&air_temperature_[0], &air_temperature_[24]);

  if (!has_vapor_pressure_ && !has_relative_humidity)
    {
      double T_min = daily_min_air_temperature_;
      const double T_max = daily_max_air_temperature_;
      if (T_min == T_max)
	T_min -= 5.0;
      for (int hour = 0; hour < 24; hour++)
	vapor_pressure_[hour] = FAO::SaturationVapourPressure (T_min);
    }
}

void
WeatherStandard::initialize (const Time& time, Treelog& err)
{ 
  Treelog::Open nest (err, "Weather: " + name);

  daisy_assert (lex == NULL);
  lex = new LexerData (where, err);
  // Open errors?
  if (!lex->good ())
    return;

  // Read first line.
  const string type = lex->get_word ();
  if (type != "dwf-0.0")
    lex->error ("Wrong file type");
  lex->skip_line ();
  lex->next_line ();

  set<string, less<string>/**/> keywords;

  struct Deposition
  {
    double total;
    double dry;
    double dry_NH4;
    double wet_NH4;
    double precipitation;
    Deposition ()
      : total (-42.42e42),
        dry (0.4),
        dry_NH4 (0.6),
        wet_NH4 (0.5),
        precipitation (-42.42e42)
    { }
  } deposition;

  // Read keywords.
  bool last_was_note = false;
  while (lex->good () && lex->peek () != '-')
    {
      string key = lex->get_word ();

      if (key.size () < 1)
        {
	  lex->error ("Empty keyword");
	  lex->skip_line ();
	  lex->next_line ();
	  continue;
        }
      if (key[key.size () - 1] != ':')
	{
	  lex->error ("Keywords should end in :");
	  lex->skip_line ();
	  lex->next_line ();
	  continue;
	}
      key = key.substr (0, key.size () - 1);

      if (keywords.find (key) == keywords.end ())
	keywords.insert (key);
      else if (key != "Note")
	lex->error (string ("Duplicate keyword '") + key + "'");
      else if (!last_was_note)
	lex->error ("Only one Note: block allowed");
      
      last_was_note = false;
		   
      if (key == "Station")
	lex->skip_line ();
      else if (key == "Note")
	{
	  lex->skip_line ();
	  last_was_note = true;
	}
      else if (key == "Surface")
	{
	  lex->skip_space ();
	  const string type = lex->get_word ();
	  if (type == "reference")
	    surface_ = reference;
	  else if (type == "field")
	    surface_ = field;
	  else
	    lex->error ("Uknown surface type");
	}
      else if (key == "PrecipCorrect")
	{
	  for (unsigned int i = 0; i < 12; i++)
	    {
	      lex->skip_space ();
	      const double val = lex->get_number ();
	      if (val < 0.5)
		lex->warning ("Unreasonable low value");
	      else if (val > 1.8)
		lex->warning ("Unreasonable high value");
	      precipitation_correction[i] = val;
	    }
	}
      else if (key == "Begin")
	{
	  lex->skip_space ();
	  lex->read_date (begin);
	}
      else if (key == "End")
	{
	  lex->skip_space ();
	  lex->read_date (end);
          end.tick_day (-2);
	}
      else
	{
	  lex->skip_space ();
	  double val = lex->get_number ();
	  lex->skip_space ();
	  const string dim = lex->get_word ();
	      
	  if (key == "NH4WetDep")
	    {
	      if (Units::can_convert (dim, "ppm"))
		val = Units::convert (dim, "ppm", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex->error ("Unreasonable value");
	      WetDeposit.NH4 = val;
	    }
	  else if (key == "NH4DryDep")
	    {
	      if (Units::can_convert (dim, "kgN/year"))
		val = Units::convert (dim, "kgN/year", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex->error ("Unreasonable value");
	      DryDeposit.NH4 = val;
	    }
	  else if (key == "NO3WetDep")
	    {
	      if (Units::can_convert (dim, "ppm"))
		val = Units::convert (dim, "ppm", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex->error ("Unreasonable value");
	      WetDeposit.NO3 = val;
	    }
	  else if (key == "NO3DryDep")
	    {
	      if (Units::can_convert (dim, "kgN/year"))
		val = Units::convert (dim, "kgN/year", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex->error ("Unreasonable value");
	      DryDeposit.NO3 = val;
	    }
          // Alternative way of specifying deposition.
	  else if (key == "Deposition")
	    {
	      if (Units::can_convert (dim, "kgN/year"))
		val = Units::convert (dim, "kgN/year", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 100.0)
		lex->error ("Unreasonable value");
	      deposition.total = val;
	    }
	  else if (key == "DepDry")
	    {
	      if (Units::can_convert (dim, "fraction"))
		val = Units::convert (dim, "fraction", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 1.0)
		lex->error ("Unreasonable value");
	      deposition.dry = val;
	    }
	  else if (key == "DepDryNH4")
	    {
	      if (Units::can_convert (dim, "fraction"))
		val = Units::convert (dim, "fraction", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 1.0)
		lex->error ("Unreasonable value");
	      deposition.dry_NH4 = val;
	    }
	  else if (key == "DepWetNH4")
	    {
	      if (Units::can_convert (dim, "fraction"))
		val = Units::convert (dim, "fraction", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 1.0)
		lex->error ("Unreasonable value");
	      deposition.wet_NH4 = val;
	    }
	  else if (key == "PAverage")
	    {
	      if (Units::can_convert (dim, "mm"))
		val = Units::convert (dim, "mm", val);
	      else
		lex->error ("Unknown dimension");
	      if (val < 0.0 || val > 3000.0)
		lex->error ("Unreasonable value");
	      deposition.precipitation = val;
	    }
	  else if (key == "Timestep")
	    {
	      if (Units::can_convert (dim, "hours"))
		val = Units::convert (dim, "hours", val);
	      else
		lex->error ("Unknown dimension");
	      timestep = double2int (val);
	      if (timestep != val || timestep < 0.0)
		lex->error ("Timestep should be a cardinal number");
	    }
	  else
	    {
	      bool found = false;
	      for (unsigned int i = 0; i < keyword_description_size; i++)
		{
		  if (key == keyword_description[i].name)
		    {
		      if (Units::can_convert (dim, keyword_description[i].dim))
			val = Units::convert (dim, keyword_description[i].dim,
					      val);
		      else
			lex->error ("Unknown dimension");
		      if (val < keyword_description[i].min)
			lex->error (key + " value too low");
		      else if (val > keyword_description[i].max)
			lex->error (key + " value too high");
		      this->*(keyword_description[i].value) = val;
		      found = true;
		    }
		}
	      if (!found)
		lex->error (string ("Unknown keyword: '") + key + "'");
	    }
	}
      lex->next_line ();
    }

  // Check keywords.
  for (unsigned int i = 0; i < keyword_description_size; i++)
    if (keyword_description[i].required 
	&& keywords.find (keyword_description[i].name) == keywords.end ())
      lex->error (string ("Keyword ") 
                  + keyword_description[i].name + " missing");

  static const string required[] = 
    { "Station", "Surface", "Begin", "End" };
  static const int required_size = sizeof (required) / sizeof (string);
  
  for (unsigned int i = 0; i < required_size; i++)
    if (keywords.find (required[i]) == keywords.end ())
      lex->error (string ("Missing keyword '") + required[i] + "'");

  static const string dep1[] = 
    { "NH4WetDep", "NO3WetDep", "NH4DryDep", "NO3DryDep" };
  static const int dep1_size = sizeof (dep1) / sizeof (string);

  bool dep1_has_all = true;
  bool dep1_has_any = false;
  for (unsigned int i = 0; i < dep1_size; i++)
    if (keywords.find (dep1[i]) == keywords.end ())
      dep1_has_all = false;
    else
      dep1_has_any = true;
  daisy_assert (dep1_has_any || !dep1_has_all);
  
  static const string dep2[] = 
    { "Deposition", "PAverage" };
  static const int dep2_size = sizeof (dep2) / sizeof (string);

  bool dep2_has_all = true;
  bool dep2_has_any = false;
  for (unsigned int i = 0; i < dep2_size; i++)
    if (keywords.find (dep2[i]) == keywords.end ())
      dep2_has_all = false;
    else
      dep2_has_any = true;
  daisy_assert (dep2_has_any || !dep2_has_all);

  if ((dep1_has_any && dep2_has_any)
      || (!dep1_has_all && !dep2_has_all))
    {
      lex->error ("\
You must specify either all of 'NH4WetDep', 'NO3WetDep', 'NH4DryDep',\n\
and 'NO3DryDep'; or alternatively all of 'Deposition' and 'PAverage',\n\
but not both");
      DryDeposit.NH4 = DryDeposit.NO3 = 1.0;
      WetDeposit.NH4 = WetDeposit.NO3 = 1.0;
    }
  else if (dep1_has_all)
    daisy_assert (!dep2_has_any);
  else
    {
      daisy_assert (!dep1_has_any);
      const double dry = deposition.total * deposition.dry;
      const double wet = deposition.total * (1.0 - deposition.dry);
      daisy_assert (approximate (dry + wet, deposition.total));
      DryDeposit.NH4 = dry * deposition.dry_NH4;
      DryDeposit.NO3 = dry * (1.0 - deposition.dry_NH4);
      WetDeposit.NH4 = wet * 100.0 * deposition.wet_NH4 
        / deposition.precipitation;
      WetDeposit.NO3 = wet * 100.0 * (1.0 - deposition.wet_NH4)
        / deposition.precipitation;
      daisy_assert (approximate (DryDeposit.NH4 + DryDeposit.NO3
                                 + ((WetDeposit.NH4 + WetDeposit.NO3)
                                    * deposition.precipitation / 100.0),
                                 deposition.total));
      TmpStream tmp;
      tmp () << "NH4WetDep: " << WetDeposit.NH4 << " ppm\n\
NH4DryDep: " << DryDeposit.NH4 << " kgN/year\n\
NO3WetDep: " << WetDeposit.NO3 << " ppm\n\
NO3DryDep: " << DryDeposit.NO3 << " kgN/year";
      err.debug (tmp.str ());
    }

  // BC5 sucks // if (begin >= end)
  if (!(begin < end))
    lex->error ("Weather data ends before they begin");

  lex->skip_hyphens ();

  // Columns
  do
    {
      const string column = lex->get_word ();
      bool found = false;
      for (unsigned int j = 0; j < data_description_size; j++)
	if (column == data_description[j].name)
	  {
	    data_index.push_back (j);
	    found = true;
	    break;
	  }
      if (!found)
	{
	  data_index.push_back (-1);
	  lex->error (string ("Unknown column ") + column);
	}
      lex->skip_space ();
    }
  while (lex->good () && lex->peek () != '\n');
  lex->next_line ();

  has_date = (has_data ("Year") && has_data ("Month") && has_data ("Day"));
  has_hour = (has_date && has_data ("Hour"));
  if (!has_date && (has_data ("Year")
		    || has_data ("Month")
		    || has_data ("Day")))
    lex->error ("You should specify all of Year, Month and Day, or none");
  if (timestep < 1 && !has_date)
    lex->error ("You must specify either a timestep or date");
  has_temperature = has_data ("AirTemp");
  has_min_temperature = has_data ("T_min");
  has_max_temperature = has_data ("T_max");
  has_vapor_pressure_ = has_data ("VapPres");
  has_relative_humidity = has_data ("RelHum");
  if (has_relative_humidity && has_vapor_pressure_)
    lex->error ("You should only specify one of VapPres or RelHum");
  has_wind_speed = has_data ("Wind");
  has_reference_evapotranspiration_ = has_data ("RefEvap");
  for (unsigned int j = 0; j < data_description_size; j++)
    if (data_description[j].required && !has_data (data_description[j].name))
      lex->error (string ("Required data column '") 
                  + data_description[j].name + "' missing");

  // Dimensions.
  for (unsigned int i = 0; i < data_index.size (); i++)
    {
      const string dimension = lex->get_word ();
      const int index = data_index[i];
      if (Units::can_convert (dimension, data_description[index].dim))
	{
	  if (data_description[index].read)
	    this->*(data_description[index].read) = dimension;
	}
      else
	lex->error ("Bad unit");

      lex->skip_space ();
    }
  lex->next_line ();

  // Start of data.
  end_of_header = lex->position ();

  // Start reading.
  Time previous (time);
  previous.tick_hour (-1);
  tick (previous, err);
}

WeatherStandard::WeatherStandard (const AttributeList& al)
  : Weather (al),
    T_rain (al.number ("T_rain")),
    T_snow (al.number ("T_snow")),
    missing_years (map_construct_const<YearMap> 
		   (al.alist_sequence ("missing_years"))),
    active_map (-1),
    timestep (0),
    begin (1900, 1, 1, 0),
    end (2100, 1, 1, 0),
    precipitation_correction (vector<double> (12, 1.0)),
    precipitation_scale (al.number_sequence ("PrecipScale")),
    has_date (false),
    has_hour (false),
    has_temperature (false),
    has_min_temperature (false),
    has_max_temperature (false),
    has_vapor_pressure_ (false),
    has_relative_humidity (false),
    has_wind_speed (false),
    has_reference_evapotranspiration_ (false),
    where (al.name ("where")),
    lex (NULL),
    end_of_header (Lexer::no_position ()),
    last_time (end),
    last_air_temperature (-42.42e42),
    last_min_air_temperature (-42.42e42),
    last_max_air_temperature (-42.42e42),
    last_global_radiation (-42.42e42),
    last_precipitation (-42.42e42),
    last_vapor_pressure (-42.42e42),
    last_relative_humidity (-42.42e42),
    last_wind_speed (-42.42e42),
    last_reference_evapotranspiration (-42.42e42),
    yesterday_T_max (-42.42e42),
    initialized (false),
    next_time (begin),
    next_year (-42.42e42),
    next_month (-42.42e42),
    next_day (-42.42e42),
    next_hour (-42.42e42),
    next_air_temperature (-42.42e42),
    next_min_air_temperature (-42.42e42),
    next_max_air_temperature (-42.42e42),
    next_global_radiation (-42.42e42),
    next_precipitation (42.42e42),
    next_vapor_pressure (-42.42e42),
    next_relative_humidity (-42.42e42),
    next_wind_speed (-42.42e42),
    next_reference_evapotranspiration (-42.42e42),
    hour (-42),
    daily_air_temperature_ (-42.42e42),
    daily_max_air_temperature_ (-42.42e42),
    daily_min_air_temperature_ (42.42e42),
    daily_global_radiation_ (-42.42e42),
    snow_fraction (-42.42e42),
    rain_fraction (-42.42e42)
{ }

WeatherStandard::~WeatherStandard ()
{ 
  sequence_delete (missing_years.begin (), missing_years.end ());
  delete lex; 
}

bool
WeatherStandard::check (const Time& from, const Time& to, Treelog& err) const
{ 
  Treelog::Open nest (err, name);

  daisy_assert (lex);
  bool ok = true;
  if (!lex->good ())
    {
      err.error ("file error for '" + lex->file +"'");
      return false;
    }
  if (lex->error_count > 0)
    {
      TmpStream tmp;
      tmp () << lex->error_count << " parser errors encountered";
      err.error (tmp.str ());
      return false;
    }
  if (from < begin && find_map (from) < 0)
    {
      err.error ("Simulation starts before weather data");
      ok = false;
    }
  if (to > end && find_map (to) < 0)
    {
      err.error ("Simulation ends after weather data");
      ok = false;
    }
  for (unsigned int i = 0; i < missing_years.size (); i++)
    {
      TmpStream tmp;
      tmp () << "missing_years[" << i << "]";
      Treelog::Open nest (err, tmp.str ());
      const int from_from = missing_years[i]->from.from;
      const int from_to = missing_years[i]->from.to;
      if (from_from > begin.year () && from_to < end.year ())
	{
	  TmpStream tmp;
	  tmp () << "domain [" << from_from << "-" << from_to 
		 << "] fully within [" << begin.year () << "-" << end.year ()
		 << "]";
	  err.warning (tmp.str ());
	}
      const int to_from = missing_years[i]->to.from;
      const int to_to = missing_years[i]->to.to;
      if (to_from < begin.year () || to_to > end.year ())
	{
	  TmpStream tmp;
	  tmp () << "range [" << to_from << "-" << to_to << "] not within [" 
		 << begin.year () << "-" << end.year () << "]";
	  err.error (tmp.str ());
	  ok = false;
	}
    }
  if (latitude < -66 || latitude > 66)
    {
      TmpStream tmp;
      tmp () << "Researching arctic agriculture? (latitude = " << 
	latitude << ")";
      err.error (tmp.str ());
    }
  return ok;
}

static struct WeatherStandardSyntax
{
  static Weather& make (const AttributeList& al)
  { return *new WeatherStandard (al); }

  WeatherStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Read a Daisy Weather File.");
    Weather::load_syntax (syntax, alist);

    syntax.add ("where", Syntax::String, Syntax::Const,
		"File to read weather data from.");
    syntax.order ("where");

    syntax.add_submodule_sequence ("missing_years", Syntax::Const, "\
How to get data for dates outside the range of the weather file.\n\
\n\
The value is a list of maps.  Each map consist of two intervals, and\n\
indicates that missing data from the first interval should be read\n\
from the second interval instead.  Each interval consists of two\n\
years, the first and last year of that interval.\n\
\n\
When the simulation requests weather data from a date outside the\n\
range covered by the weather file, the model will look up each member\n\
of the list, to see if the year is covered by the first interval.  If\n\
so, it will use weather data from the same day in the corresponding\n\
year in the second interval.\n\
\n\
If a given year is covered by multiple intervals in the list, the first\n\
one will be used.",
				   WeatherStandard::YearMap::load_syntax);
    alist.add ("missing_years", vector<AttributeList*> ());

    // Division between Rain and Snow.
    syntax.add ("T_rain", "dg C", Syntax::Const, 
		"Above this air temperature all precipitation is rain.");
    alist.add ("T_rain", 2.0);
    syntax.add ("T_snow", "dg C", Syntax::Const,
		"Below this air temperature all precipitation is snow.");
    alist.add ("T_snow", -2.0);

    // PrecipScalee.
    syntax.add ("PrecipScale", Syntax::None (), Check::non_negative (), 
		Syntax::Const, 12, "\
The precipitation listed in the file will be multiplied by the number\n\
from this list before it is used in the simulation, depending on the\n\
month.  The first number corresponds to January, the second to\n\
February, etc.  For example, the number 0.5 in the first place in the\n\
list will mean the precipitation used in the simulation will be half\n\
of what is listed in the file for January, while 2.0 will mean that\n\
the precipitation used is twice the amount listed.\n\
\n\
If PrecipScale is specified both as a parameter in the Daisy setup\n\
file and PrecipCorrect as a keyword in the Daisy weather file, the two\n\
values will be multiplied together.\n\
\n\
It is suggested that the keyword is used to correct systematic\n\
mistakes in the measurement process, while the parameter is used for\n\
experimenting with different precipitation values and for reusing data\n\
from one weather station in nearby areas where only average values are\n\
known.");
    alist.add ("PrecipScale", vector<double> (12, 1.0));

    Librarian<Weather>::add_type ("default", alist, syntax, &make);
  }
} WeatherStandard_syntax;
