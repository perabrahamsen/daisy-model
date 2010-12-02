// weather_source.C -- Weather data from WSource.
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
#include "wsource.h"
#include "time.h"
#include "timestep.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include "mathlib.h"
#include <boost/scoped_ptr.hpp>
#include <map>
#include <deque>

struct WeatherSource : public Weather
{
  // Data.
  boost::scoped_ptr<WSource> source;
  typedef std::map<symbol, std::deque<double>/**/> number_map_t;
  number_map_t numbers;
  typedef std::map<symbol, std::deque<symbol>/**/> name_map_t;
  name_map_t names;
  std::deque<Time> when;
  
  // Current values.
  double number_average (symbol) const;
  void extract_average (symbol, double&, Treelog&) const;
  symbol name_first (symbol) const;
  double my_latitude;           // [dg North]
  double my_longitude;
  double my_elevation;
  double my_timezone;
  double my_screenheight;
  double my_Taverage;
  double my_Tamplitude;
  double my_maxTday;
  Weatherdata::surface_t my_surface;
  double my_global_radiation;
  double my_diffuse_radiation;
  double my_reference_evapotranspiration;
  double my_wind;
  // TODO: More.

  // Simulation.
  Time previous;
  Time next;
  double suggest_dt () const;   // [h]
  void tick (const Time& time, Treelog&);

  // TODO: Output.

  // Create and Destroy.
  bool initialize (const Time& time, Treelog& msg);
  bool check (const Time& from, const Time& to, Treelog&) const;
  WeatherSource (const BlockModel&);
  ~WeatherSource ();
};

double 
WeatherSource::number_average (const symbol key) const
{
  // This function considers the source data constant within the
  // source interval, and will give you the average value for the
  // weather interval.

  // Available data.
  const number_map_t::const_iterator e = numbers.find (key);
  daisy_assert (e != numbers.end ());
  const std::deque<double>& values = e->second;
  const size_t data_size = when.size ();
  daisy_assert (values.size () == data_size);

  switch (data_size)
    {
    case 0:
      daisy_panic ("No weather data");
    case 1:
      return values[0];
    }

  // Find start.
  size_t i = 0;
  while (i < data_size && when[i] <= previous)
    i++;
  
  if (i == data_size)
    // All data is before current period.
    return values.back ();
  
  // Aggregate complete values.
  double sum_value = 0.0;
  double sum_hours = 0.0;
  Time time = previous;
  
  while (i < data_size && when[i] < next)
    {
      const double hours = Time::hours_between (time, when[i]);
      sum_hours += hours;
      sum_value += hours * values[i];
      time = when[i];
      i++;
    }

  // Add end interval.
  const double hours = Time::hours_between (time, next);
  const double value = (i < data_size ? values[i] : values.back ());
  sum_hours += hours;
  sum_value += hours * value;
  daisy_approximate (sum_hours, Time::hours_between (previous, next));
  return sum_value / sum_hours;
}
    
void 
WeatherSource::extract_average (const symbol key, double& variable, 
                                Treelog& msg) const
{
  const double value = number_average (key);
  if (std::isfinite (value))
    variable = value;
  else
    msg.error ("Missing value for '" + key + "', reusing old");
}

symbol
WeatherSource::name_first (const symbol key) const
{
  // This function return the name used at the beginning of weather interval.

  // Available data.
  const name_map_t::const_iterator e = names.find (key);
  daisy_assert (e != names.end ());
  const std::deque<symbol>& values = e->second;
  const size_t data_size = when.size ();
  daisy_assert (values.size () == data_size);

  switch (data_size)
    {
    case 0:
      daisy_panic ("No weather data");
    case 1:
      return values[0];
    }

  // Find start.
  size_t i = 0;
  while (i < data_size && when[i] <= previous)
    i++;
  
  // And that's it.
  return values.back ();
}
    
double 
WeatherSource::suggest_dt () const // [h]
{ 
  // Suggest running until we get new data.
  for (size_t i = 0; i < when.size (); i++)
    if (when[i] > next)
      return (when[i] - next).total_hours ();
  
  // No applicable weather data.  
  return 24.0 * 365.2425 * 1000; // A millenium.
}

void 
WeatherSource::tick (const Time& time, Treelog& msg)
{
  daisy_assert (time > previous);

  // Update time interval
  bool new_day = false;

  if (time > next)
    {
      // The beginning of the timestep determine what day we are in.
      if (previous.year () != next.year () || previous.yday () != next.yday ())
        new_day = true;

      // New timestep.
      previous = next;
    }

  // Possible shorter version of same timestep.
  next = time;

  // Push back.
  Time next_day (next.year (), next.month (), next.mday (), 0);
  next_day.tick_day (); // We keep one day worth of weather data.
  while (!source->done () && when.back () < next_day)
    {
      daisy_assert (when.back () == source->begin ());
      when.push_back (source->end ());

      for (number_map_t::iterator i = numbers.begin ();
           i != numbers.end ();
           i++)
        { 
          const symbol meta = i->first;
          const symbol key = Weatherdata::meta_key (meta);
          std::deque<double>& data = i->second;
          if (key == Attribute::Unknown ())
            {
              if (source->end_check (meta))
                data.push_back (source->end_number (meta));
              else 
                data.push_back (NAN);
            }
          else
            {
              if (source->meta_end_check (key, meta))
                data.push_back (source->meta_end_number (key, meta));
              else
                data.push_back (NAN);
            }
        }
      for (name_map_t::iterator i = names.begin ();
           i != names.end ();
           i++)
        { 
          const symbol meta = i->first;
          const symbol key = Weatherdata::meta_key (meta);
          std::deque<symbol>& data = i->second;
          if (key == Attribute::Unknown ())
            {
              if (source->end_check (meta))
                data.push_back (source->end_name (meta));
              else 
                data.push_back (Attribute::Unknown ());
            }
          else
            {
              if (source->meta_end_check (key, meta))
                data.push_back (source->meta_end_name (key, meta));
              else
                data.push_back (Attribute::Unknown ());
            }
       }
    }

  // Calculate new values.
  if (when.size () < 1)
    {
      // No data, use old values.
      return;
    }
  
  // Calculate values for this timestep.
  extract_average (Weatherdata::Latitude (), my_latitude, msg);
  extract_average (Weatherdata::Longitude (), my_longitude, msg);
  extract_average (Weatherdata::Elevation (), my_elevation, msg);
  extract_average (Weatherdata::TimeZone (), my_timezone, msg);
  extract_average (Weatherdata::ScreenHeight (), my_screenheight, msg);
  extract_average (Weatherdata::TAverage (), my_Taverage, msg);
  extract_average (Weatherdata::TAmplitude (), my_Tamplitude, msg);
  extract_average (Weatherdata::MaxTDay (), my_maxTday, msg);
  {
    const symbol name = name_first (Weatherdata::Surface ());
    if (name == Attribute::Unknown ())
      msg.warning ("Unknown surface, using old");
    else
      my_surface = Weatherdata::symbol2surface (name);
  }
  extract_average (Weatherdata::GlobRad (), my_global_radiation, msg);

  // Optional values.
  my_diffuse_radiation = number_average (Weatherdata::DiffRad ());
  my_reference_evapotranspiration = number_average (Weatherdata::RefEvap ());
  my_wind = number_average (Weatherdata::Wind ());

  // Special values.
  const double new_air_temperature = number_average (Weatherdata::AirTemp ());
  const double new_vapor_pressure = number_average (Weatherdata::VapPres ());
  const double new_relative_humidity = number_average (Weatherdata::RelHum ());

#ifdef TODO

  // Calculate:
  // 1. Air temperature (from T_min / T_max)
  // 2. Vapor pressure (from RelHum + T)
  // 2. Rain/Snow (from Precip + T)

  if (new_day)
    {
      // TODO: day_start, day_end and day_length
      my_sunrise = 12.0 - my_day_length * 0.5; // Use astronomy.C.

      // Min/Max T and day length.
      // TODO: Add avg.
      double new_max = NAN;
      double new_min = NAN;
      
      // Available data.
      const size_t data_size = when.size ();
      const number_map_t::const_iterator eAvg 
        = numbers.find (Weatherdata::AirTemp ());
      daisy_assert (eAvg != numbers.end ());
      const std::deque<double>& values_avg = eAvg->second;
      daisy_assert (values_avg.size () == data_size);
      const number_map_t::const_iterator eMin 
        = numbers.find (Weatherdata::AirTemp ());
      daisy_assert (eMin != numbers.end ());
      const std::deque<double>& values_min = eMin->second;
      daisy_assert (values_min.size () == data_size);
      const number_map_t::const_iterator eMax 
        = numbers.find (Weatherdata::AirTemp ());
      daisy_assert (eMax != numbers.end ());
      const std::deque<double>& values_max = eMax->second;
      daisy_assert (values_max.size () == data_size);

      if (data_size < 1)
        daisy_panic ("No weather data");

      // Find start.
      size_t i = 0;
      while (i < data_size && when[i] <= day_start)
        i++;
  
      if (i == data_size)
        // All data is before current period.
        {
          new_min = values_min.back ();
          new_max = values_max.back ();
        }
      else while (i < data_size && (i == 0 || when[i-1] < day_end))
        {
          // Take all data in period into account.
          if (!std::finite (new_min) || new_min > values_min[i])
            new_min = values_min[i];
          if (!std::finite (new_min) || new_min > values_avg[i])
            new_min = values_avg[i];
          if (!std::finite (new_max) || new_max < values_max[i])
            new_max = values_max[i];
          if (!std::finite (new_max) || new_max < values_avg[i])
            new_max = values_avg[i];

          i++;
        }
      
      // Use it.
      if (approximate (new_min, new_max))
        my_daily_min_air_temperature = my_daily_max_air_temperature = NAN;
      else
        {
          my_daily_min_air_temperature = new_min;
          my_daily_max_air_temperature = new_max;
        }
    }
  
  if (!std::isfinite (new_air_temperature) && has_min_max_temperature ())
    {
      // We assume max T is at 15:00 and min T is at sunrise.
      // We assume previous and next day are identical to this one,
      // ignoring actual data from the two days.
      // We assume linear change between max T of the preceding day,
      // min T of the current day, max T of current day, and min T of
      // the next day.

      PLF T;
      T.add (15.0 - 24.0, my_daily_max_air_temperature);
      T.add (sunrise, my_daily_min_air_temperature);
      T.add (15.0, my_daily_max_air_temperature);
      T.add (sunrise + 24.0. my_daily_min_air_temperature);
      const double hours = 0.5 * ((previous - day_start).total_hours ()
                                  + (next - day_start).total_hours ());
      new_air_temperature = T (hours);
    }
  if (std::isfinite (new_air_temperature))
    msg.error ("No air temperature, resuing old value");
  else
    my_air_temperature = new_air_temperature;
    
  virtual double rain () const = 0;	// [mm/h]
  virtual double snow () const = 0;	// [mm/h]

  virtual const IM& deposit () const = 0; // [g [stuff] /cm²/h]

  virtual double cloudiness () const = 0; // [0-1]


  virtual double CO2 () const = 0; //[Pa]
  virtual double O2 () const = 0; //[Pa]
  virtual double air_pressure () const = 0; //[Pa]


  virtual double timestep () const = 0; // [d]

  virtual bool has_reference_evapotranspiration () const = 0;
  virtual bool has_vapor_pressure () const = 0;
  virtual bool has_wind () const = 0;
  virtual bool has_min_max_temperature () const = 0;
  virtual bool has_diffuse_radiation () const = 0;

  virtual double daily_air_temperature () const = 0; // [dg C]
  virtual double daily_max_air_temperature () const = 0; // [dg C]
  virtual double daily_min_air_temperature () const = 0; // [dg C]
  virtual double daily_global_radiation () const = 0; // [W/m2]
  virtual double daily_precipitation () const = 0; // [mm/d]

  virtual double day_length () const = 0; // [h]
  virtual double T_normal (const Time&, double delay = 0.0) const = 0;
#endif

  // Only do this at a new day.
  if (!new_day)
    return;

  // TODO: Calculate daily values. (period: day_start, next_day)

  // Pop front.
  while (when.size () > 0 && when[0] < previous)
    {
      for (number_map_t::iterator i = numbers.begin ();
           i != numbers.end (); 
           i++)
        {
          daisy_assert (when.size () == i->second.size ());
          i->second.pop_front ();
        }
      for (name_map_t::iterator i = names.begin ();
           i != names.end (); 
           i++)
        {
          daisy_assert (when.size () == i->second.size ());
          i->second.pop_front ();
        }
      when.pop_front ();
    }
  if (when.size () < 1)
    msg.warning ("No more weather data, last value");
}

bool 
WeatherSource::initialize (const Time& time, Treelog& msg)
{
  bool ok = true;
  // TODO: Initialize previous, next, numbers, names.

  return ok;
}

bool 
WeatherSource::check (const Time& from, const Time& to, Treelog& msg) const
{
  bool ok = true;

  static struct required_t : public std::vector<symbol>
  {
    required_t ()
    {
      push_back (Weatherdata::Latitude ());
      push_back (Weatherdata::Longitude ());
      push_back (Weatherdata::Elevation ());
      push_back (Weatherdata::TimeZone ());
      push_back (Weatherdata::ScreenHeight ());
      push_back (Weatherdata::TAverage ());
      push_back (Weatherdata::TAmplitude ());
      push_back (Weatherdata::MaxTDay ());
      push_back (Weatherdata::Station ());
      push_back (Weatherdata::Surface ());
    }
  } required;

  for (size_t i = 0; i < required.size (); i++)
    if (!source->check (required[i]))
    {
      ok = false;
      msg.error ("Required weather data '" + required[i] + "' missing");
    }

  // TODO: More checks.
  return ok;
}

WeatherSource::WeatherSource (const BlockModel& al)
  : Weather (al),
    source (Librarian::build_item<WSource> (al, "source"))
{ }

WeatherSource::~WeatherSource ()
{ }

// Add the WeatherSource syntax to the syntax table.
static struct WeatherSourceSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { 
#if 0
    return new WeatherSource (al); 
#else
    daisy_notreached ();
#endif
  }
  WeatherSourceSyntax ()
    : DeclareModel (Weather::component, "source",
                    "Assemble weather data from weather source.")
  { }
  void load_frame (Frame& frame) const
  { 
    // TODO: Weather:load_common.

    frame.declare_object ("source", WSource::component, "\
Source of weather data.");
  }
} WeatherSource_syntax;

// weather_source.C ends here.

