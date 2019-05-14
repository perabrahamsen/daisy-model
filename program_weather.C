// program_weather.C -- Examine weather file.
// 
// Copyright 2019 Per Abrahamsen and KU.
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

#include "program.h"
#include "wsource.h"
#include "time.h"
#include "librarian.h"
#include "block_model.h"
#include "submodeler.h"
#include "time.h"
#include "assertion.h"

#include <memory>
#include <sstream>
#include <vector>
#include <cmath>

struct ProgramWeather : public Program
{
  // Data.
  const std::unique_ptr<WSource> weather;
  const Time begin;
  const Time end;

  // Use.
  bool run (Treelog& msg)
  {
    std::vector<double> T_sum  (367, 0.0);
    std::vector<double> T_count (367, 0.0);

    for (Time time = begin; time < end; time.tick_day (1))
      {
	weather->weather_tick (time, msg);
	const double T = weather->daily_air_temperature ();
	const int yday = time.yday ();
	daisy_assert (yday > 0 && yday < 367);
	T_sum[yday] += T;
	T_count[yday] += 1.0;
      }
    double a0 = 0.0;
    double a1 = 0.0;
    double b1 = 0.0;
    for (int i = 1; i < 366; i++)
      {
	daisy_assert (T_count[i] > 0.0);
	const double p = T_sum[i] / T_count[i];
	const double t = i;
	a0 += p * std::cos (2.0 * M_PI * 0.0 * t / 365.0);
	a1 += p * std::cos (2.0 * M_PI * 1.0 * t / 365.0);
	b1 += p * std::sin (2.0 * M_PI * 1.0 * t / 365.0);
      }
    a0 *= 2.0 / 365.0;
    a1 *= 2.0 / 365.0;
    b1 *= 2.0 / 365.0;

    const double A0 = a0;
    const double A1 = std::sqrt (a1 * a1 + b1 * b1);
    double phi = std::atan2 (b1, a1);
    if (phi < 0.0)
      phi += 2.0 * M_PI;
    std::ostringstream tmp;
    tmp << "TAverage: " << 0.5 * A0 << " dgC\n"
	<< "TAmplitude: " << A1 << " dgC\n"
	<< "MaxTDay: " << std::round (phi * 365.0 / (2.0 * M_PI)) << " yday";
    msg.message (tmp.str ());

    return true;
  }
  
  // Create and Destroy.
  void initialize (Block& block)
  {
    weather->weather_initialize (begin, block.msg ());
  }
  
  bool check (Treelog& msg)
  {
    bool ok = true;
    if (Time::whole_days_between (begin, end) < 366)
      {
	msg.error ("Need more than one year of data");
	ok = false;
      }
    if (Time::whole_days_between (begin, end) < 365 * 10)
      msg.warning ("Should have at least 10 years of data");
    
    if (!weather->weather_check (begin, end, msg))
      ok = false;

    return ok;
  }
  
  ProgramWeather (const BlockModel& al)
    : Program (al),
      weather (Librarian::build_item<WSource> (al, "weather")),
      begin (*submodel<Time> (al, "begin")),
      end (*submodel<Time> (al, "end"))
  { }
  ~ProgramWeather ()
  { }
};

static struct ProgramWeatherSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramWeather (al); }
  ProgramWeatherSyntax ()
    : DeclareModel (Program::component, "weather", "\
Weather data from a data file.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("weather", WSource::component, 
			  Attribute::Const, Attribute::Singleton, "\
Weather source to analyse.");
    frame.declare_submodule ("begin", Attribute::Const,
			     "First date to analyze.", Time::load_syntax);
    frame.declare_submodule ("end", Attribute::Const,
			     "Last date to analyse.", Time::load_syntax);
  }
} ProgramWeather_syntax;

// program_weather.C ends here.
