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
  
  // TODO: Current values.

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
  if (time > next)
    // New timestep.
    previous = next;

  // Possible shorter version of same timestep.
  next = time;

  // Pop front.
  while (when.size () > 1 && when[1] < previous)
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

  // Push back.
  const Time day_start (next.year (), next.month (), next.mday (), 0);
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
          const symbol key = i->first;
          std::deque<double>& data = i->second;
          // TODO: Add meta when appropriate.
          data.push_back (source->end_number (key));
        }
      for (name_map_t::iterator i = names.begin ();
           i != names.end ();
           i++)
        { 
          const symbol key = i->first;
          std::deque<symbol>& data = i->second;
          // TODO: Add meta when appropriate.
          data.push_back (source->end_name (key));
        }
    }

  // (Re)Calculate values for this timestep.
  // TODO: Calculate value for this timestep (previous, next)
  // If we don't have the full period, use old values.

  
  if (previous.year () == next.year () && previous.yday () == next.yday ())
    // TODO: Hvad hvis "next" er forandret?
    return;                     // Same day.
  
  // New day
  // TODO: Calculate daily values. (period: day_start, next_day)
  // If we don't have the full period, use old values.
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

