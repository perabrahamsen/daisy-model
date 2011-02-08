// wsource.C -- Selected weather data.
// 
// Copyright 2010 KU
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

#include "wsource_weather.h"
#include "weatherdata.h"
#include "time.h"
#include "librarian.h"
#include "assertion.h"

// The "weather" component.

const char *const WSource::component = "weather";

symbol
WSource::library_id () const
{
  static const symbol id (component);
  return id;
}

WSource::WSource (const symbol name)
  : ModelDerived (name)
{ }

WSource::~WSource ()
{ }

static struct WSourceInit : public DeclareComponent
{
  WSourceInit ()
    : DeclareComponent (WSource::component, "\
A 'wsource' is a source of raw weatherdata.")
  { }
} WSource_init;

// The 'null' model.

struct WSourceNull : public WSourceWeather
{
  // Scope interface.
  void entries (std::set<symbol>&) const
  { }
  Attribute::type lookup (symbol) const
  { return Attribute::Error; }
  symbol dimension (symbol) const
  { return Attribute::Unknown (); }
  symbol description (symbol) const
  { return Attribute::Unknown (); }
  using WSource::check;
  bool check (symbol) const
  { return false; }
  double number (symbol) const
  { daisy_notreached (); }
  symbol name (symbol) const
  { daisy_notreached (); }

  // WSource interface.
  int type_size (symbol) const // Don't use default from Scope.
  { return Attribute::Unspecified; }
  int value_size (symbol) const
  { daisy_notreached (); }
  bool end_check (symbol) const
  { return false; }
  double end_number (symbol) const
  { daisy_notreached (); }
  symbol end_name (symbol) const
  { daisy_notreached (); }
  const std::vector<double>& number_sequence (symbol) const
  { daisy_notreached (); }
  const std::vector<double>& end_number_sequence (symbol) const
  { daisy_notreached (); }

  double meta_timestep (symbol) const
  { daisy_notreached (); }
  bool meta_check (symbol, symbol) const
  { return false; }
  double meta_number (symbol, symbol) const
  { daisy_notreached (); }
  symbol meta_name (symbol, symbol) const
  { daisy_notreached (); }
  bool meta_end_check (symbol, symbol) const
  { return false; }
  double meta_end_number (symbol, symbol) const
  { daisy_notreached (); }
  symbol meta_end_name (symbol, symbol) const
  { daisy_notreached (); }

  const Time& data_begin () const // Start of first timestep.
  { daisy_notreached (); }
  const Time& data_end () const   // End of last timestep.
  { daisy_notreached (); }
  const Time& begin () const
  { daisy_notreached (); }
  const Time& end () const
  { daisy_notreached (); }
  double timestep () const           // Length of timetstep [h]
  { daisy_notreached (); }
  void tick (Treelog&)
  { daisy_notreached (); }
  bool done () const
  { return true; }

  void initialize (Treelog&)
  { }
  using Scope::check;
  bool check (Treelog&) const
  { return true; }

  // Create and destroy.
  WSourceNull (const BlockModel& al)
    : WSourceWeather (al)
  { }
  ~WSourceNull ()
  { }
};

static struct WSourceNullSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceNull (al); }
  WSourceNullSyntax ()
    : DeclareModel (WSource::component, "null", "weather", "\
Weather that does not change during the simulation.")
  { }
  void load_frame (Frame&) const
  { }
} WSourceNull_syntax;

// wsource.C ends here.

