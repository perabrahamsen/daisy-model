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
#include "block_model.h"
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include <boost/scoped_ptr.hpp>

struct WeatherSource : public Weather
{
  boost::scoped_ptr<WSource> source;

  double extract_number (const symbol key, const symbol meta) const
  { 
    if (source->meta_check (key, meta))
      return source->meta_number (key, meta);
    
    return source->number (meta);
  }
  symbol extract_name (const symbol key, const symbol meta) const
  { 
    if (source->meta_check (key, meta))
      return source->meta_name (key, meta);
    
    return static_cast<const Scope&> (*source).name (meta);
  }


  // Simulation.
  double latitude () const
  { return extract_number (Weatherdata::GlobRad (),
                           Weatherdata::Latitude ()); }
  double longitude () const 
  { return extract_number (Weatherdata::GlobRad (),
                           Weatherdata::Longitude ()); }
  double elevation () const     // [m]
  { return extract_number (Weatherdata::AirTemp (),
                           Weatherdata::Elevation ()); }
  double timezone () const
  { return extract_number (Weatherdata::GlobRad (),
                           Weatherdata::TimeZone ()); }
  double screen_height () const // [m]
  { return extract_number (Weatherdata::Wind (),
                           Weatherdata::ScreenHeight ()); }    
  Weatherdata::surface_t surface () const
  { 
    const symbol key = extract_name (Weatherdata::Wind (),
                                           Weatherdata::Surface ());
    return Weatherdata::symbol2surface (key); 
  }

  // Create and Destroy.
  bool check (const Time& from, const Time& to, Treelog&) const;
  WeatherSource (const BlockModel&);
  ~WeatherSource ();
};

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
    frame.declare_object ("source", WSource::component, "\
Source of weather data.");
  }
} WeatherSource_syntax;

// weather_source.C ends here.

