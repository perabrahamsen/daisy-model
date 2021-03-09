// wsource_weather.C -- Weather data from WSource.
// 
// Copyright 2011 KU.
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

#include "wsource_weather.h"
#include "chemical.h"
#include "im.h"
#include "units.h"
#include "plf.h"
#include "time.h"
#include "timestep.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include "mathlib.h"
#include "astronomy.h"
#include "fao.h"
#include "log.h"
#include <map>
#include <deque>
#include <sstream>

class WSourceWeather::Implementation
{
public:
  const Weather& weather;
  const Units& units;
  WSource& source;
  
  static const symbol dry_deposit_unit ()
  {
    static const symbol unit ("kg/ha/y");
    return unit;
  }

  // Parameters.
  const PLF snow_fraction;
  const double max_rain;

  double NH4WetDep;
  double NH4DryDep;
  double NO3WetDep;
  double NO3DryDep;
  double Deposition;
  double DepDry;
  double DepDryNH4;
  double DepWetNH4;
  double PAverage;
  IM DryDeposit;
  IM WetDeposit;
  static bool unchanged (double a, double b);
  void reset_deposition (Treelog& msg);

  // Data.
  typedef std::map<symbol, std::deque<double>/**/> number_map_t;
  number_map_t numbers;
  number_map_t timesteps;
  typedef std::map<symbol, std::deque<symbol>/**/> name_map_t;
  name_map_t names;
  std::deque<Time> when;
  
  // Current values.
  double max_timestep (const Time& previous, const Time& next, symbol) const;
  double max_timestep (symbol) const;
  double find_sum_dt (const Time& from, const double goal, 
                      const symbol key) const;
  double number_average (const Time& previous, const Time& next, symbol) const;
  double number_average (symbol) const;
  void extract_average (const Time& previous, const Time& next,
                        symbol, double&, Treelog&) const;
  void extract_average (symbol, double&, Treelog&) const;
  symbol name_first (symbol) const;

  // Current values.
  double my_latitude;           // [dg North]
  double my_longitude;
  double my_elevation;
  double my_timezone;
  double my_screenheight;
  double my_Taverage;
  double my_Tamplitude;
  double my_maxTday;
  Weatherdata::surface_t my_surface;
  double my_day_cycle;
  double my_global_radiation;
  double my_sin_solar_elevation_angle;
  double my_extraterrestrial_radiation;
  double my_diffuse_radiation;
  double my_reference_evapotranspiration;
  double my_wind;
  double my_CO2;
  double my_air_temperature;
  double my_vapor_pressure;
  double my_relative_humidity;
  double my_rain;
  double my_snow;
  IM my_deposit;
  double my_cloudiness;

  // Daily values.
  Time day_start;
  Time day_end;
  double my_day_length;         // [h]
  double my_sunrise;            // [h]
  bool my_has_min_max_temperature;
  double my_daily_min_air_temperature; // [dg C]
  double my_daily_max_air_temperature; // [dg C]
  double my_daily_air_temperature;     // [dg C]
  double my_daily_global_radiation;    // [W/m^2]
  double my_daily_extraterrestrial_radiation;    // [W/m^2]
  double my_daily_precipitation;       // [mm/d]

  // Extract weather.
  static double safe_value (const double value, const double reserve)
  { return std::isfinite (value) ? value: reserve; }
  double T_normal (const Time& time, double delay) const;

  // Simulation.
  Time previous;
  Time next;
  double data_dt () const;      // [h]
  double rain_dt () const;      // [h]
  double suggest_dt () const;   // [h]
  void tick_weather (const Time& time, Treelog&);
  void check_state (const symbol key, const double value, Treelog& msg);
  void check_state (Treelog& msg);
    
  // Create and Destroy.
  void set_all_maps ();
  void rewind (const Time&, Treelog& msg);
  mutable bool initialized_ok;
  void initialize_one (Treelog& msg);
  void initialize_two (const Time& time, Treelog& msg);
  bool check (const Time& from, const Time& to, Treelog&) const;
  Implementation (const Weather&, WSource&, const BlockModel&);
  ~Implementation ();
};

double 
WSourceWeather::Implementation::max_timestep (const Time& from, const Time& to,
                                              const symbol key) const
{
  // Available data.
  const number_map_t::const_iterator e = timesteps.find (key);
  daisy_assert (e != timesteps.end ());
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
  while (i < data_size && when[i] <= from)
    i++;
  
  if (i == data_size)
    // All data is before current period.
    return values.back ();
  
  // Complete values.
  double max_value = 0.0;
  for (; i < data_size && when[i] < to; i++)
    if (values[i] > max_value)
      max_value = values[i];
  
  // Last value.
  const double value = (i < data_size ? values[i] : values.back ());
  if (value > max_value)
    max_value = value;

  return max_value;
}

double 
WSourceWeather::Implementation::max_timestep (const symbol key) const
{ return max_timestep (previous, next, key); }

double 
WSourceWeather::Implementation::find_sum_dt (const Time& from,
                                             const double goal,
                                             const symbol key) const
{
  // Find the timestep (dt) so that the time weighted sum of a
  // parameter (key) is equal to the stated goal (goal).  This is
  // intended to be used with precipitation to find the timestep
  // needed to reach max_rain.
  
  // Available data.
  const number_map_t::const_iterator e = numbers.find (key);
  if (e == numbers.end ())
    return NAN;

  daisy_assert (e != numbers.end ());
  const std::deque<double>& values = e->second;
  const size_t data_size = when.size ();
  daisy_assert (values.size () == data_size);

  switch (data_size)
    {
    case 0:
      throw "No weather data";
    case 1:
      if (values[0] > 0.0)
        return goal / values[0];
      return NAN;
    }

  // Find start.
  size_t i = 0;
  while (i < data_size && when[i] <= from)
    i++;
  
  if (i == data_size)
    return NAN;
  
  // Aggregate complete values.
  double missing = goal;
  double sum_hours = 0.0;
  Time time = from;
  
  while (i < data_size)
    {
      const double hours = Time::fraction_hours_between (time, when[i]);
      const double delta = hours * values[i];
      if (delta >= missing)
        {
          daisy_assert (values[i] > 0.0);
          return sum_hours + missing / values[i];
        }
      sum_hours += hours;
      missing -= delta;
      if (missing < goal * 0.01)
        // Avoid numeric problems with small numbers.
        return sum_hours;
      time = when[i];
      i++;
    }
  return NAN;
}
    
double 
WSourceWeather::Implementation::number_average (const Time& from, 
                                                const Time& to,
                                                const symbol key) const
{
  // This function considers the source data constant within the
  // source interval, and will give you the average value for the
  // weather interval.

  // Available data.
  const number_map_t::const_iterator e = numbers.find (key);
  
  if (e == numbers.end ())
    {
#if 0
      std::ostringstream tmp;
      tmp << "Can't average '" + key + "'; know";
      for (number_map_t::const_iterator i = numbers.begin ();
           i != numbers.end ();
           i++)
        tmp << " " << i->first;

      Assertion::message (tmp.str ());
#endif
      return NAN;
    }

  daisy_assert (e != numbers.end ());
  const std::deque<double>& values = e->second;
  const size_t data_size = when.size ();
  daisy_assert (values.size () == data_size);

  switch (data_size)
    {
    case 0:
      throw "No weather data";
    case 1:
      return values[0];
    }

  // Find start.
  size_t i = 0;
  while (i < data_size && when[i] <= from)
    i++;
  
  if (i == data_size)
    // All data is before current period.
    return values.back ();
  
  // Aggregate complete values.
  double sum_value = 0.0;
  double sum_hours = 0.0;
  Time time = from;
  
  while (i < data_size && when[i] < to)
    {
      const double hours = Time::fraction_hours_between (time, when[i]);
      sum_hours += hours;
      sum_value += hours * values[i];
      time = when[i];
      i++;
    }

  // Add end interval.
  const double hours = Time::fraction_hours_between (time, to);
  const double value = (i < data_size ? values[i] : values.back ());
  sum_hours += hours;
  sum_value += hours * value;
  daisy_approximate (sum_hours, Time::fraction_hours_between (from, to));
  if (!std::isnormal (sum_hours))
    daisy_panic ("No time between " + from.print () + " and " + to.print ());
  return sum_value / sum_hours;
}
    

double 
WSourceWeather::Implementation::number_average (const symbol key) const
{ return number_average (previous, next, key); }

void 
WSourceWeather::Implementation::extract_average (const Time& from, const Time& to,
                                                 const symbol key, double& variable, 
                                                 Treelog& msg) const
{
  const double value = number_average (from, to, key);
  if (std::isfinite (value))
    variable = value;
  else
    msg.warning ("Missing value for '" + key + "', reusing old");
}

void 
WSourceWeather::Implementation::extract_average (const symbol key, double& variable, 
                                                 Treelog& msg) const
{
  const double value = number_average (key);
  if (std::isfinite (value))
    variable = value;
  else
    msg.warning ("Missing value for '" + key + "', reusing old");
}

bool 
WSourceWeather::Implementation::unchanged (const double a, const double b)
{
  if (std::isnormal (a) && std::isnormal (b))
    return approximate (a, b);
  
  return std::isfinite (a) == std::isfinite (b);
}

void
WSourceWeather::Implementation::reset_deposition (Treelog& msg)
{
  Treelog::Open next (msg, __FUNCTION__);

  // Normal deposition.
  const double new_NH4WetDep = number_average (Weatherdata::NH4WetDep ());
  const double new_NH4DryDep = number_average (Weatherdata::NH4DryDep ());
  const double new_NO3WetDep = number_average (Weatherdata::NO3WetDep ());
  const double new_NO3DryDep = number_average (Weatherdata::NO3DryDep ());

  // Alternative way of specifying deposition.
  const double new_Deposition = number_average (Weatherdata::Deposition ());
  const double new_DepDry = number_average (Weatherdata::DepDry ());
  const double new_DepDryNH4 = number_average (Weatherdata::DepDryNH4 ());
  const double new_DepWetNH4 = number_average (Weatherdata::DepWetNH4 ());
  const double new_PAverage = number_average (Weatherdata::PAverage ());
  
  // No changes.
  if (unchanged (new_NH4WetDep, NH4WetDep)
      && unchanged (new_NH4DryDep, NH4DryDep)
      && unchanged (new_NO3WetDep, NO3WetDep)
      && unchanged (new_NO3DryDep, NO3DryDep)
      && unchanged (new_Deposition, Deposition)
      && unchanged (new_DepDry, DepDry)
      && unchanged (new_DepDryNH4, DepDryNH4)
      && unchanged (new_DepWetNH4, DepWetNH4)
      && unchanged (new_PAverage, PAverage))
    return;

  // New values.
  NH4WetDep = new_NH4WetDep;
  NH4DryDep = new_NH4DryDep;
  NO3WetDep = new_NO3WetDep;
  NO3DryDep = new_NO3DryDep;
  Deposition = new_Deposition;
  DepDry = new_DepDry;
  DepDryNH4 = new_DepDryNH4;
  DepWetNH4 = new_DepWetNH4;
  PAverage = new_PAverage;

  struct DepositionData
  {
    double total;
    double dry;
    double dry_NH4;
    double wet_NH4;
    double precipitation;
    DepositionData ()
      : total (-42.42e42),
        dry (0.4),
        dry_NH4 (0.6),
        wet_NH4 (0.5),
        precipitation (-42.42e42)
    { }
  } deposition;


  const Unit& u_ppm = units.get_unit (Units::ppm ());
  const Unit& u_dpu = units.get_unit (dry_deposit_unit ());

  if (std::isfinite (NH4WetDep))
    WetDeposit.set_value (Chemical::NH4 (), u_ppm, NH4WetDep);
  if (std::isfinite (NH4DryDep))
    DryDeposit.set_value (Chemical::NH4 (), u_dpu, NH4DryDep);
  if (std::isfinite (NO3WetDep))
    WetDeposit.set_value (Chemical::NO3 (), u_ppm, NO3WetDep);
  if (std::isfinite (NO3DryDep))
    DryDeposit.set_value (Chemical::NO3 (), u_dpu, NO3DryDep);

  // Alternative way of specifying deposition.
  if (std::isfinite (Deposition))
    deposition.total = Deposition;
  if (std::isfinite (DepDry))
    deposition.dry = DepDry;
  if (std::isfinite (DepDryNH4))
    deposition.dry_NH4 = DepDryNH4;
  if (std::isfinite (DepWetNH4))
    deposition.wet_NH4 = DepWetNH4;
  if (std::isfinite (PAverage))
    deposition.precipitation = PAverage;
  
  // Check consistency.
  bool dep_ok = true;
  const bool dep1_has_all = std::isfinite (NH4WetDep) 
    && std::isfinite (NO3WetDep)
    && std::isfinite (NH4DryDep)
    && std::isfinite (NO3DryDep);
  const bool dep1_has_any = std::isfinite (NH4WetDep) 
    || std::isfinite (NO3WetDep)
    || std::isfinite (NH4DryDep)
    || std::isfinite (NO3DryDep);
  if (dep1_has_any && !dep1_has_all)
    dep_ok = false;
  
  const bool dep2_has_all = std::isfinite (Deposition) 
    && std::isfinite (PAverage);
  bool dep2_has_any = std::isfinite (Deposition) 
    || std::isfinite (PAverage);
  if (dep2_has_any && !dep2_has_all)
    dep_ok = false;

  if (dep1_has_all && dep2_has_any)
    dep_ok = false;
  if (dep2_has_all && dep1_has_any)
    dep_ok = false;
  if (!dep1_has_all && !dep2_has_all)
    dep_ok = false;

  if (!dep_ok)
    {
      msg.error ("\
You must specify either all of 'NH4WetDep', 'NO3WetDep', 'NH4DryDep',\n\
and 'NO3DryDep'; or alternatively all of 'Deposition' and 'PAverage',\n\
but not both");
      initialized_ok = false;
    }
  else if (dep1_has_all)
    /* Already handled */;
  else
    {
      daisy_assert (dep2_has_all);
      const double dry = deposition.total * deposition.dry;
      const double wet = deposition.total * (1.0 - deposition.dry);
      daisy_assert (approximate (dry + wet, deposition.total));
      DryDeposit.set_value (Chemical::NH4 (), u_dpu, dry * deposition.dry_NH4);
      DryDeposit.set_value (Chemical::NO3 (), u_dpu,
			    dry * (1.0 - deposition.dry_NH4));
      WetDeposit.set_value (Chemical::NH4 (), u_ppm, 
                            wet * 100.0 * deposition.wet_NH4 
			    / deposition.precipitation);
      WetDeposit.set_value (Chemical::NO3 (), u_ppm, 
			    wet * 100.0 * (1.0 - deposition.wet_NH4)
			    / deposition.precipitation);
      std::ostringstream tmp;
      tmp << "NH4WetDep: " 
	  << WetDeposit.get_value (Chemical::NH4 (), u_ppm) << " ppm\n\
NH4DryDep: " 
	  << DryDeposit.get_value (Chemical::NH4 (), u_dpu) << " kgN/ha/year\n\
NO3WetDep: " << WetDeposit.get_value (Chemical::NO3 (), u_ppm) << " ppm\n\
NO3DryDep: " << DryDeposit.get_value (Chemical::NO3 (), u_dpu) 
          << " kgN/ha/year";
      msg.debug (tmp.str ());
    }
}

symbol
WSourceWeather::Implementation::name_first (const symbol key) const
{
  // This function return the name used at the beginning of weather interval.

  // Available data.
  const name_map_t::const_iterator e = names.find (key);
  if (e == names.end ())
    {
      daisy_bug ("Weather source for '" + key.name () + "' not found");
      return Attribute::Unknown ();
    }
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
WSourceWeather::Implementation::T_normal (const Time& time, double delay) const
{
  if (!std::isfinite (my_Taverage) || !std::isfinite (my_Tamplitude))
    // used by initialization.
    return NAN;

  const double displacement = time.year_fraction () - my_maxTday / 365.0;
  
  daisy_assert (delay <= 0);
  daisy_assert (my_Taverage > -400);
  return my_Taverage
    + my_Tamplitude
    * exp (delay)
    * cos (2.0 * M_PI * displacement + delay);
}

double
WSourceWeather::Implementation::data_dt () const // [h]
{ 
  // Suggest running until we get new data.
  for (size_t i = 0; i < when.size (); i++)
    if (when[i] > previous)
      return (when[i] - previous).total_hours ();
  
  // No applicable weather data.  
  return NAN;
}

double
WSourceWeather::Implementation::rain_dt () const // [h]
{ 
  if (max_rain <= 0.0)
    return NAN;
    
  return find_sum_dt (previous, max_rain, Weatherdata::Precip ()); 
}

double 
WSourceWeather::Implementation::suggest_dt () const // [h]
{ 
  const double data_dt = this->data_dt ();
  const double rain_dt = this->rain_dt ();

  if (!std::isfinite (data_dt))
    return rain_dt;

  if (!std::isfinite (rain_dt))
    return data_dt;

  return std::min (data_dt, rain_dt);
}

void 
WSourceWeather::Implementation::tick_weather (const Time& time, Treelog& msg)
{
  // This function is only called from top level, not on nested wsources.

  if (source.done ())
    return;

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

  static const double long_timestep = 12.0; // [h]

  // Push back.
  Time next_day (next.year (), next.month (), next.mday (), 0);
  next_day.tick_day (); // We keep one day worth of weather data.
  Time last = when.size () > 0 ? when.back () : source.begin ();
  for (;!source.done () && last < next_day; source.source_tick (msg))
    {
      if (last != source.begin ())
        daisy_panic (last.print () + " != " + source.begin ().print ());

      when.push_back (source.end ());
      last = when.back ();
      // Numbers.
      for (number_map_t::iterator i = numbers.begin ();
           i != numbers.end ();
           i++)
        { 
          const symbol meta = i->first;
          daisy_assert (meta != Attribute::None ());
          const symbol key = Weatherdata::meta_key (meta);
          std::deque<double>& data = i->second;
          if (key == Attribute::None ())
            {
              if (source.end_check (meta))
                data.push_back (source.end_number (meta));
              else 
                data.push_back (NAN);
            }
          else
            {
              if (source.meta_end_check (key, meta))
                data.push_back (source.meta_end_number (key, meta));
              else
                data.push_back (NAN);
            }
        }

      // Timesteps.
      for (number_map_t::iterator i = timesteps.begin ();
           i != timesteps.end ();
           i++)
        { 
          const symbol key = i->first;
          std::deque<double>& data = i->second;
          if (source.check (key))
            data.push_back (source.meta_timestep (key));
          else
            data.push_back (NAN);
        }

      // Names.
      for (name_map_t::iterator i = names.begin ();
           i != names.end ();
           i++)
        { 
          const symbol meta = i->first;
          const symbol key = Weatherdata::meta_key (meta);
          std::deque<symbol>& data = i->second;
          if (key == Attribute::None ())
            {
              if (source.end_check (meta))
                data.push_back (source.end_name (meta));
              else 
                data.push_back (Attribute::Unknown ());
            }
          else
            {
              if (source.meta_end_check (key, meta))
                data.push_back (source.meta_end_name (key, meta));
              else
                data.push_back (Attribute::Unknown ());
            }
        }
    }

  if (source.done ())
    msg.message ("source done");

  // Calculate new values.
  if (when.size () < 1)
    {
      // No data, use old values.
      check_state (msg);
      return;
    }
  
  if (new_day)
    // Daily values.
    {
      // Day duration.
      day_start 
        = Time (next.year (), next.month (), next.mday (), 0);
      day_end = day_start;
      day_end.tick_day (1);
      my_day_length = Astronomy::DayLength (next, weather.latitude ());
      my_sunrise = 12.0 - my_day_length * 0.5; // Use astronomy.C.

      // Global radiation.
      extract_average (day_start, day_end, 
                       Weatherdata::GlobRad (), my_daily_global_radiation, msg);

      // Extraterrestrial radiation.
      my_daily_extraterrestrial_radiation
	= Astronomy::DailyExtraterrestrialRadiation (next, weather.latitude ());

      // Precipitation.
      double new_daily_precipitation // [mm/h]
        = number_average (day_start, day_end, Weatherdata::Precip ());
      if (std::isfinite (new_daily_precipitation))
        my_daily_precipitation = new_daily_precipitation * 24.0; // [mm/d]
      else
        msg.warning ("No daily precipitation, reusing old value");

      // Min/Max temperature.
      double new_max = NAN;
      double new_min = NAN;

      // Available data.
      const size_t data_size = when.size ();
      const std::deque<double> missing (data_size, NAN);
      const number_map_t::const_iterator eAvg 
        = numbers.find (Weatherdata::AirTemp ());
      const std::deque<double>& values_avg = (eAvg != numbers.end ())
        ? eAvg->second : missing;
      daisy_assert (values_avg.size () == data_size);
      const number_map_t::const_iterator eMin 
        = numbers.find (Weatherdata::T_min ());
      const std::deque<double>& values_min = (eMin != numbers.end ())
        ? eMin->second : missing;
      daisy_assert (values_min.size () == data_size);
      const number_map_t::const_iterator eMax 
        = numbers.find (Weatherdata::T_max ());
      const std::deque<double>& values_max = (eMax != numbers.end ())
        ? eMax->second : missing;
      daisy_assert (values_max.size () == data_size);

      if (data_size < 1)
        daisy_panic ("No weather data");

      // Find start.
      size_t i = 0;
      while (i < data_size && when[i] <= day_start)
        i++;
  
      bool has_min = false;
      bool has_max = false;
      if (i == data_size)
        // All data is before current period.
        {
          new_min = values_min.back ();
          new_max = values_max.back ();
          has_min = std::isfinite (values_min[i]);
          has_max = std::isfinite (values_max[i]);
        }
      else while (i < data_size && (i == 0 || when[i-1] < day_end))
             {
               // Explicit min/max temperature.
               if (std::isfinite (values_min[i]))
                 has_min = true;
               if (std::isfinite (values_max[i]))
                 has_max = true;
              
               // Take all data in period into account.
               if (!std::isfinite (new_min) || new_min > values_min[i])
                 new_min = values_min[i];
               if (!std::isfinite (new_min) || new_min > values_avg[i])
                 new_min = values_avg[i];
               if (!std::isfinite (new_max) || new_max < values_max[i])
                 new_max = values_max[i];
               if (!std::isfinite (new_max) || new_max < values_avg[i])
                 new_max = values_avg[i];
               i++;
             }

      // Use it.
      if (std::isfinite (new_min))
        my_daily_min_air_temperature = new_min;
      if (std::isfinite (new_max))
        my_daily_max_air_temperature = new_max;
      double T_step = max_timestep (day_start, day_end, 
                                    Weatherdata::AirTemp ());
      my_has_min_max_temperature = ((has_min && has_max) 
                                    || T_step < long_timestep);

      const double new_T = number_average (day_start, day_end, 
                                           Weatherdata::AirTemp ());

      // Average temperature.
      if (std::isfinite (new_T))
        my_daily_air_temperature = new_T;
      else if (my_has_min_max_temperature)
        my_daily_air_temperature = 0.5 * (my_daily_min_air_temperature
                                          + my_daily_max_air_temperature);
      else
        msg.warning ("No daily air temperature, reusing using old value");
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

  // Sun angle.
  const double sin_beta_1
    = Astronomy::SinSolarElevationAngle (previous,
					 weather.latitude (),
					 weather.longitude (),
					 weather.timezone ());
  const double sin_beta_2
    = Astronomy::SinSolarElevationAngle (next,
					 weather.latitude (),
					 weather.longitude (),
					 weather.timezone ());
  
  if ((sin_beta_1 > 0.0) == (sin_beta_2 > 0.0))
    my_sin_solar_elevation_angle = 0.5 * (sin_beta_1 + sin_beta_2);
  else
    my_sin_solar_elevation_angle = 0.5 * (std::max (sin_beta_1, 0.0)
					  + std::max (sin_beta_2, 0.0));
  
  // Day cycle.
  my_extraterrestrial_radiation
    = Astronomy::ExtraterrestrialRadiation (previous, next, weather.latitude (),
					    weather.longitude (), 
					    weather.timezone ());
  my_day_cycle = my_extraterrestrial_radiation
    / my_daily_extraterrestrial_radiation;
  daisy_assert (std::isfinite (my_day_cycle));

  if (max_timestep (Weatherdata::GlobRad ()) < long_timestep)
    extract_average (Weatherdata::GlobRad (), my_global_radiation, msg);
  else
    my_global_radiation = my_daily_global_radiation * my_day_cycle;

  // Optional values.
  my_diffuse_radiation = number_average (Weatherdata::DiffRad ());
  if (std::isfinite (my_diffuse_radiation) 
      && max_timestep (Weatherdata::DiffRad ()) >= long_timestep)
    my_diffuse_radiation *= my_day_cycle;

  my_reference_evapotranspiration = number_average (Weatherdata::RefEvap ());
  if (std::isfinite (my_reference_evapotranspiration) 
      && max_timestep (Weatherdata::RefEvap ()) >= long_timestep)
    my_reference_evapotranspiration *= my_day_cycle;

  my_wind = number_average (Weatherdata::Wind ());
  my_CO2 = number_average (Weatherdata::CO2 ());
  
  const double new_air_temperature = number_average (Weatherdata::AirTemp ());
  if (std::isfinite (new_air_temperature))
    my_air_temperature = new_air_temperature;
  else if (weather.has_min_max_temperature ())
    {
      // We assume max T is at 15:00 and min T is at sunrise.
      // We assume previous and next day are identical to this one,
      // ignoring actual data from the two days.
      // We assume linear change between max T of the preceding day,
      // min T of the current day, max T of current day, and min T of
      // the next day.

      PLF T;
      T.add (15.0 - 24.0, my_daily_max_air_temperature);
      T.add (my_sunrise, my_daily_min_air_temperature);
      T.add (15.0, my_daily_max_air_temperature);
      T.add (my_sunrise + 24.0, my_daily_min_air_temperature);
      const double prev_hours = (previous - day_start).total_hours ();
      const double next_hours = (next - day_start).total_hours ();
      const double total_hours = next_hours - prev_hours;
      my_air_temperature = T.integrate (prev_hours, next_hours) / total_hours;
    }
  else
    msg.warning ("No air temperature, resuing old value");

  // Vapor pressure and relative humidity.
  const double new_vapor_pressure = number_average (Weatherdata::VapPres ());
  const double new_relative_humidity = number_average (Weatherdata::RelHum ());

  if (std::isfinite (new_vapor_pressure))
    my_vapor_pressure = new_vapor_pressure;
  else if (std::isfinite (new_relative_humidity))
    my_vapor_pressure = FAO::SaturationVapourPressure (my_air_temperature) 
      * new_relative_humidity;
  else
    my_vapor_pressure = NAN;

  if (std::isfinite (new_relative_humidity))
    my_relative_humidity = new_relative_humidity;
  else if (std::isfinite (new_vapor_pressure))
    my_relative_humidity 
      = new_vapor_pressure / FAO::SaturationVapourPressure (my_air_temperature);
  else
    my_relative_humidity = NAN;

  // Rain and snow.
  const double new_precipitation = number_average (Weatherdata::Precip ());

  // TODO PrecipCorrect PrecipScale.
  if (std::isfinite (new_precipitation))
    {
      my_snow = snow_fraction (my_air_temperature) * new_precipitation;
      my_rain = new_precipitation - my_snow;
    }
  else
    msg.warning ("Precipitation missing, reusing old");

  // Deposition.
  reset_deposition (msg);
  const Unit& u_flux = units.get_unit (IM::flux_unit ());
  const Unit& u_solute = units.get_unit (IM::solute_unit ());
  const Unit& u_precip = units.get_unit (Units::mm_per_h ());
  const IM dry (u_flux, DryDeposit);
  const IM solute (u_solute, WetDeposit);
  const IM wet (solute.multiply (Scalar (my_snow + my_rain, u_precip), u_flux));
  my_deposit = dry + wet;

  // Cloudiness.
  {
    const double rad = my_extraterrestrial_radiation;
    const double Si = my_global_radiation;
    if (Si > 25.0 && rad > 25.0)
      my_cloudiness = FAO::CloudinessFactor_Humid (Si, rad);
  }
  
  // Got everything?
  check_state (msg);

  // Only do this at a new day.
  if (!new_day)
    return;

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
      for (number_map_t::iterator i = timesteps.begin ();
           i != timesteps.end (); 
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

void
WSourceWeather::Implementation::check_state (const symbol key, const double value, Treelog& msg)
{
#if 0
  std::ostringstream tmp;
  tmp << "Value of " << key << " is " << value;
  msg.message (tmp.str ());
#endif
  if (std::isfinite (value))
    return;
  msg.error ("No data for '" + key + "'");
  initialized_ok = false;
}

void
WSourceWeather::Implementation::check_state (Treelog& msg)
{
  check_state (Weatherdata::Latitude (), my_latitude, msg);
  check_state (Weatherdata::Longitude (), my_longitude, msg);
  check_state (Weatherdata::Elevation (), my_elevation, msg);
  check_state (Weatherdata::TimeZone (), my_timezone, msg);
  check_state (Weatherdata::ScreenHeight (), my_screenheight, msg);
  check_state (Weatherdata::TAverage (), my_Taverage, msg);
  check_state (Weatherdata::TAmplitude (), my_Tamplitude, msg);
  check_state (Weatherdata::MaxTDay (), my_maxTday, msg);
  check_state (Weatherdata::GlobRad (), my_global_radiation, msg);
  check_state (Weatherdata::AirTemp (), my_air_temperature, msg);
  static const symbol my_rain_name ("Rain");
  check_state (my_rain_name, my_rain, msg);
  static const symbol my_snow_name ("Snow");
  check_state (my_snow_name, my_snow, msg);
  static const symbol my_cloudiness_name ("Cloudiness");
  check_state (my_cloudiness_name, my_cloudiness, msg);
  static const symbol my_day_length_name ("DayLength"); 
  check_state (my_day_length_name, my_day_length, msg);
  static const symbol my_sunrise_name ("Sunrise"); 
  check_state (my_sunrise_name, my_sunrise, msg);
  static const symbol my_daily_air_temperature_name ("DailyAirTemp"); 
  check_state (my_daily_air_temperature_name, my_daily_air_temperature, msg);
  static const symbol my_daily_global_radiation_name ("DailyGlobRad"); 
  check_state (my_daily_global_radiation_name, my_daily_global_radiation, msg);
  static const symbol my_daily_precipitation_name ("DailyPrecip"); 
  check_state (my_daily_precipitation_name, my_daily_precipitation, msg);
}

void
WSourceWeather::Implementation::rewind (const Time& time, Treelog& msg)
{
  // Reset data.
  numbers.clear ();
  names.clear ();
  when.clear ();
  timesteps.clear ();
  set_all_maps ();

  // Reset time.
  next = Time (time.year (), time.month (), time.mday (), 0);
  previous = next;
  previous.tick_hour (-1);
  if (initialized_ok)
    {
      try
        { tick_weather (time, msg); }
      catch (...)
        { initialized_ok = false; }
    }
}

void
WSourceWeather::Implementation::set_all_maps ()
{
  // Numbers and names.
  std::set<symbol> all;
  source.entries (all);
  for (std::set<symbol>::const_iterator i = all.begin (); i != all.end (); i++)
    {
      const symbol key = *i;

      if (source.type_size (key) != Attribute::Singleton)
        continue;

      switch (source.lookup (key))
        {
        case Attribute::Number:
          numbers[key];         // Instantiate.
          break;
        case Attribute::String:
          names[key];           // Instantiate.
          break;
        default:
          break;
        }
    }

  // Timesteps for day cycle variables.
  timesteps[Weatherdata::GlobRad ()];
  timesteps[Weatherdata::RefEvap ()];
  timesteps[Weatherdata::DiffRad ()];
  timesteps[Weatherdata::AirTemp ()];
}

void
WSourceWeather::Implementation::initialize_one (Treelog& msg)
{
  // Source.
  source.source_initialize (msg);
  if (!source.source_check (msg))
    initialized_ok = false;
  
  set_all_maps ();
}

void
WSourceWeather::Implementation::initialize_two (const Time& time, Treelog& msg)
{
  // Check interval.
  const Time& data_begin = source.data_begin ();
  const Time& data_end = source.data_end ();
  if ((data_begin != Time::null () && time < data_begin)
      || (data_end != Time::null () && time > data_end))
    {
      initialized_ok = false;
      std::ostringstream tmp;
      tmp << "Simulation start " << time.print () 
          << " is not covered by weather data from ";
      if (data_begin != Time::null ())
        tmp << data_begin.print ();
      else 
        tmp << "unknown";
      tmp << " to ";
      if (data_end != Time::null ())
        tmp << data_end.print ();
      else 
        tmp << "unknown";
        
      msg.error (tmp.str ());
    }

  // Fast skip.
  Time a_week_ago = time;
  a_week_ago.tick_day (-7);
  source.skip_ahead (a_week_ago, msg);
  
  // Initialize previous, next
  next = Time (time.year (), time.month (), time.mday (), 0);
  if (time == next)
    next.tick_day (-1);
  previous = next;
  previous.tick_hour (-1);
  if (initialized_ok)
    {
      try
        { 
          tick_weather (time, msg); 
          return;
        }
      catch (const std::string s)
        { msg.error (s); }
      catch (const char *const s)
        { msg.error (s); }
      catch (...)
        { msg.error ("Initialization failed"); }
      initialized_ok = false;
    }
}

bool 
WSourceWeather::Implementation::check (const Time& from, const Time& to,
                                       Treelog& msg) const
{
  // Check interval.
  const Time& data_begin = source.data_begin ();
  const Time& data_end = source.data_end ();
  if ((data_begin != Time::null () && from < data_begin)
      || (data_end != Time::null () && to > data_end))
    {
      initialized_ok = false;
      std::ostringstream tmp;
      tmp << "Simulation period from " << from.print () 
          << " to " << to.print () 
          << " is not covered by weather data from ";
      if (data_begin != Time::null ())
        tmp << data_begin.print ();
      else 
        tmp << "unknown";
      tmp << " to ";
      if (data_end != Time::null ())
        tmp << data_end.print ();
      else 
        tmp << "unknown";
        
      msg.error (tmp.str ());
    }

  if (!initialized_ok)
    return false;

  // Required parameters.
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
    if (!source.check (required[i]))
      {
        initialized_ok = false;
        msg.error ("Required weather data '" + required[i] + "' missing");
      }

  // TODO: More checks.
  return initialized_ok;
}

WSourceWeather::Implementation::Implementation (const Weather& w,
                                                WSource& s,
                                                const BlockModel& al)
  : weather (w),
    units (al.units ()),
    source (s),
    snow_fraction (al.plf ("snow_fraction")),
    max_rain (al.number ("max_rain", -1.0)),
    NH4WetDep (NAN),
    NH4DryDep (NAN),
    NO3WetDep (NAN),
    NO3DryDep (NAN),
    Deposition (NAN),
    DepDry (NAN),
    DepDryNH4 (NAN),
    DepWetNH4 (NAN),
    PAverage (NAN),
    DryDeposit (units.get_unit (dry_deposit_unit ())),
    WetDeposit (units.get_unit (Units::ppm ())),
    my_latitude (NAN),
    my_longitude (NAN),
    my_elevation (NAN),
    my_timezone (NAN),
    my_screenheight (NAN),
    my_Taverage (NAN),
    my_Tamplitude (NAN),
    my_maxTday (NAN),
    my_day_cycle (NAN),
    my_global_radiation (NAN),
    my_sin_solar_elevation_angle (NAN),
    my_extraterrestrial_radiation (NAN),
    my_diffuse_radiation (NAN),
    my_reference_evapotranspiration (NAN),
    my_wind (NAN),
    my_CO2 (NAN),
    my_air_temperature (NAN),
    my_vapor_pressure (NAN),
    my_relative_humidity (NAN),
    my_rain (NAN),
    my_snow (NAN),
    my_deposit (al, "deposit"),    // For the units...
    my_cloudiness (0.5),           // Wait for light.
    my_day_length (NAN),
    my_sunrise (NAN),
    my_has_min_max_temperature (false),
    my_daily_min_air_temperature (NAN),
    my_daily_max_air_temperature (NAN),
    my_daily_air_temperature (NAN),
    my_daily_global_radiation (NAN),
    my_daily_extraterrestrial_radiation (NAN),
    my_daily_precipitation (NAN),
    initialized_ok (false)
{ }

WSourceWeather::Implementation::~Implementation ()
{ }

double
WSourceWeather::latitude () const
{ return Implementation::safe_value (impl->my_latitude, 56.0); }

double
WSourceWeather::longitude () const
{ return Implementation::safe_value (impl->my_longitude, 12.0); }

double
WSourceWeather::elevation () const
{ return impl->my_elevation; }

double
WSourceWeather::timezone () const
{ return Implementation::safe_value (impl->my_timezone, 15.0); }

double
WSourceWeather::screen_height () const
{ return impl->my_screenheight; }

Weatherdata::surface_t 
WSourceWeather::surface () const
{ return impl->my_surface; }

double
WSourceWeather::air_temperature () const
{ return impl->my_air_temperature; }

double
WSourceWeather::daily_air_temperature () const
{ return impl->my_daily_air_temperature; }

double
WSourceWeather::daily_max_air_temperature () const
{ return impl->my_daily_max_air_temperature; }

double
WSourceWeather::daily_min_air_temperature () const
{ return impl->my_daily_min_air_temperature; }

double
WSourceWeather::global_radiation () const
{ return impl->my_global_radiation; }

double
WSourceWeather::daily_global_radiation () const
{ return impl->my_daily_global_radiation; }

double
WSourceWeather::diffuse_radiation () const
{ return has_diffuse_radiation () ? impl->my_diffuse_radiation : 0.0; }

double
WSourceWeather::reference_evapotranspiration () const
{ return impl->my_reference_evapotranspiration; }

double
WSourceWeather::daily_precipitation () const
{ return impl->my_daily_precipitation; }

double
WSourceWeather::rain () const
{ return impl->my_rain; }

double
WSourceWeather::snow () const
{ return impl->my_snow; }

const IM&
WSourceWeather::deposit () const
{ return impl->my_deposit; }

double
WSourceWeather::cloudiness () const
{ return impl->my_cloudiness; }

double
WSourceWeather::vapor_pressure () const
{ return has_vapor_pressure () 
    ? impl->my_vapor_pressure 
    : has_min_max_temperature ()
    ? FAO::SaturationVapourPressure (daily_min_air_temperature ())
    : FAO::SaturationVapourPressure (daily_air_temperature () - 5.0); }

double
WSourceWeather::wind () const
{ return has_wind () ? impl->my_wind : 5.0; }

double
WSourceWeather::CO2 () const
{ 
  return has_CO2 () ? impl->my_CO2 * air_pressure () : -42.42e42; 
}

double
WSourceWeather::O2 () const
{ 
  static const double standard_pressure = FAO::AtmosphericPressure (0.0);
  return 20500.0 * air_pressure () / standard_pressure; 
}

double
WSourceWeather::air_pressure () const
{ return FAO::AtmosphericPressure (elevation ()); }

bool
WSourceWeather::has_reference_evapotranspiration () const
{ return std::isfinite (impl->my_reference_evapotranspiration); }

bool
WSourceWeather::has_vapor_pressure () const
{ return std::isfinite (impl->my_vapor_pressure); }

bool
WSourceWeather::has_wind () const
{ return std::isfinite (impl->my_wind); }

bool
WSourceWeather::has_CO2 () const
{ return std::isfinite (impl->my_CO2); }

bool
WSourceWeather::has_min_max_temperature () const
{ return impl->my_has_min_max_temperature; }

bool
WSourceWeather::has_diffuse_radiation () const
{ return std::isfinite (impl->my_diffuse_radiation); }

double
WSourceWeather::timestep () const
{ return Time::fraction_hours_between (impl->previous, impl->next); }

double
WSourceWeather::extraterrestrial_radiation () const // [W/m2]
{ return impl->my_extraterrestrial_radiation; }

double
WSourceWeather::sin_solar_elevation_angle () const // []
{ return impl->my_sin_solar_elevation_angle; }

double
WSourceWeather::day_length () const
{ return impl->my_day_length; }

double 
WSourceWeather::T_normal (const Time& time, double delay) const
{ return impl->T_normal (time, delay); }

double
WSourceWeather::average_temperature () const
{ 
  if (impl->initialized_ok)
    return impl->my_Taverage; 
  
  // Used by initialization.
  return 10.0;
}
double 
WSourceWeather::suggest_dt () const
{ return impl->suggest_dt (); }

void 
WSourceWeather::weather_tick (const Time& time, Treelog& msg)
{
  // This function is only called from top level, not on nested wsources.
  TREELOG_MODEL (msg);
  impl->tick_weather (time, msg); 
}

void 
WSourceWeather::output (Log& log) const
{
  output_value (air_temperature (), "air_temperature", log);
  output_value (daily_air_temperature (), "daily_air_temperature", log);
  output_value (daily_min_air_temperature (),
                "daily_min_air_temperature", log);
  output_value (daily_max_air_temperature (), 
                "daily_max_air_temperature", log);
  output_value (impl->my_day_cycle, "day_cycle", log);
  output_value (impl->my_extraterrestrial_radiation,
		"extraterrestrial_radiation", log);
  output_value (global_radiation (), "global_radiation", log);
  output_value (impl->my_daily_extraterrestrial_radiation,
		"daily_extraterrestrial_radiation", log);
  output_value (daily_global_radiation (), "daily_global_radiation", log);
  if (has_reference_evapotranspiration ())
    output_value (reference_evapotranspiration (), 
                  "reference_evapotranspiration", log);
  output_value (rain (), "rain", log);
  output_value (snow (), "snow", log);
  output_value (rain () + snow (), "precipitation", log);
  output_value (cloudiness (), "cloudiness", log);
  output_value (vapor_pressure (), "vapor_pressure", log);
  if (std::isfinite (impl->my_relative_humidity))
    output_value (impl->my_relative_humidity, "relative_humidity", log);
  output_value (air_pressure (), "air_pressure", log);
  output_value (diffuse_radiation (), "diffuse_radiation", log);
  output_value (wind (), "wind", log);
  if (has_CO2 ())
    output_value (CO2 (), "co2", log);
  output_value (day_length (), "day_length", log);
  output_submodule (deposit (), "deposit", log);
}

void 
WSourceWeather::rewind (const Time& time, Treelog& msg)
{ impl->rewind (time, msg); }

bool
WSourceWeather::initialized_ok () const
{ return impl->initialized_ok; }

void
WSourceWeather::initialize_one (Treelog& msg)
{
  impl->initialized_ok = true;
  impl->initialize_one (msg);
}

void
WSourceWeather::initialize_two (const Time& time, Treelog& msg)
{
  impl->initialize_two (time, msg);
}

void
WSourceWeather::weather_initialize (const Time& time, Treelog& msg)
{
  std::ostringstream tmp;
  tmp << title () << ": initializing at " << time.print ();
  Treelog::Open nest (msg, tmp.str ());
  initialize_one (msg);
  initialize_two (time, msg);
}

bool 
WSourceWeather::weather_check (const Time& from, const Time& to,
                               Treelog& msg) const
{
  TREELOG_MODEL (msg);
  return impl->check (from, to, msg);
}

WSourceWeather::WSourceWeather (const BlockModel& al)
  : WSource (al.type_name ()),
    impl (new Implementation (*this, *this, al))
{ }

WSourceWeather::~WSourceWeather ()
{ }

// Add the WSourceWeather syntax to the syntax table.
static struct WSourceWeatherSyntax : public DeclareBase
{
  WSourceWeatherSyntax ()
    : DeclareBase (WSource::component, "weather",
                   "Weather interface implementation.")
  { }
  static void load_flux (Frame& frame)
  { IM::add_syntax (frame, Attribute::LogOnly, IM::flux_unit ()); }

  void load_frame (Frame& frame) const
  { 
    frame.declare ("snow_fraction", "dg C", Attribute::Fraction (),
                   Attribute::Const, "\
Fraction of precipitation that falls as snow as function of air temperature.");
    PLF snow_fraction;
    snow_fraction.add (-2.0, 1.0);
    snow_fraction.add (2.0, 0.0);
    frame.set ("snow_fraction", snow_fraction);
    frame.declare ("max_rain", "mm", Attribute::OptionalConst,
                   "Largest amount of rain in one timestep.\n\
By default, no limit on rain.");
    // Logs.
    frame.declare ("air_temperature", "dg C", Attribute::LogOnly,
                   "Air temperature.");
    frame.declare ("day_cycle", Attribute::None (), Attribute::LogOnly, "\
Current level extraterrestrial radiation realtive to day average.");
    frame.declare ("extraterrestrial_radiation", "W/m^2", Attribute::LogOnly,
                   "Extraterrestrial radiation.");
    frame.declare ("global_radiation", "W/m^2", Attribute::LogOnly,
                   "Global radiation.");
    frame.declare ("daily_air_temperature", "dg C", Attribute::LogOnly,
                   "Average temperature this day.");
    frame.declare ("daily_min_air_temperature", "dg C", Attribute::LogOnly,
                   "Minumum temperature this day.");
    frame.declare ("daily_max_air_temperature", "dg C", Attribute::LogOnly,
                   "Maximum temperature this day.");
    frame.declare ("daily_global_radiation", "W/m^2", Attribute::LogOnly,
                   "Average global radiation this day.");
    frame.declare ("daily_extraterrestrial_radiation", "W/m^2",
		   Attribute::LogOnly,
                   "Average extraterrestrial radiation this day.");
    frame.declare ("diffuse_radiation", "W/m^2", Attribute::LogOnly,
                   "Diffuse radiation.");
    frame.declare ("reference_evapotranspiration", "mm/h", Attribute::LogOnly,
                   "Reference evapotranspiration");
    frame.declare ("rain", "mm/h", Attribute::LogOnly, "Rain.");
    frame.declare ("snow", "mm/h", Attribute::LogOnly, "Snow.");
    frame.declare ("precipitation", "mm/h", Attribute::LogOnly, 
                   "Precipitation.");
    frame.declare_fraction ("cloudiness", Attribute::LogOnly,
                            "Fraction of sky covered by clouds [0-1].");
    frame.declare ("vapor_pressure", "Pa", Attribute::LogOnly, "Humidity.");
    frame.declare ("relative_humidity", Attribute::Fraction (), 
                   Attribute::LogOnly, "Relative humidity.");
    frame.declare ("air_pressure", "Pa", Attribute::LogOnly, "Air pressure.");
    frame.declare ("wind", "m/s", Attribute::LogOnly, "Wind speed.");
    frame.declare ("co2", "Pa", Attribute::LogOnly, "Atmospheric CO2 pressure.");
    frame.declare ("day_length", "h", Attribute::LogOnly,
                   "Number of light hours this day.");
    frame.declare_submodule_sequence ("deposit", Attribute::LogOnly, "\
Total atmospheric deposition.", load_flux);
    
  }
} WSourceWeather_syntax;

// wsource_weather.C ends here.

