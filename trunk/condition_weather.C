// condition_weather.C
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
//
// Checking the weather.

#define BUILD_DLL

#include "condition.h"
#include "block_model.h"
#include "field.h"
#include "daisy.h"
#include "check.h"
#include "log.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>

// The 'TSum_above' Model.

struct ConditionTSum : public Condition
{
  // Parameters.
  const int check_hour;
  const int reset_mday;
  const int reset_month;
  const double TSum_limit;

  // State.
  double TSum_now;

  void tick (const Daisy& daisy, const Scope&, Treelog&)
  {
    if (daisy.time ().hour () == check_hour)
      {
	if (daisy.time ().mday () == reset_mday
	    && daisy.time ().month () == reset_month)
	  TSum_now = 0.0;
	const double T = daisy.field ().daily_air_temperature ();
	if (T > 0.0)
	  TSum_now += T;
      }
  }

  bool match (const Daisy&, const Scope&, Treelog&) const
  { return TSum_now > TSum_limit; }

  void output (Log& log) const
  { output_variable (TSum_now, log); }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionTSum (const BlockModel& al)
    : Condition (al),
      check_hour (al.integer ("check_hour")),
      reset_mday (al.integer ("reset_mday")),
      reset_month (al.integer ("reset_month")),
      TSum_limit (al.number ("TSum_limit")),
      TSum_now (al.number ("TSum_now", -100.0e100))
  { }
};

static struct ConditionTSumAboveSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionTSum (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  {
    bool ok = true;
    const int mm = al.integer ("reset_month");
    const int dd = al.integer ("reset_mday");

    if (mm < 1 || mm > 12)
      {
	err.entry ("month should be between 1 and 12");
	ok = false;
      }
    // don't test for bad month.
    else if (dd < 1 || dd > Time::month_length (1 /* not a leap year */, mm))
      {
	std::ostringstream tmp;
	tmp << "day should be between 1 and " << Time::month_length (1, mm);
	err.entry (tmp.str ());
	ok = false;
      }
    return ok;
  }
  ConditionTSumAboveSyntax ()
    : DeclareModel (Condition::component, "TSum_above", "\
Test if the temperature sum is above the specified value\n\
The temperature sum is the sum of the daily average air temperature since\n\
last reset.  It is reset once a year.  Days where the average is below 0\n\
does not count in the sum.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.add_check (check_alist);

      frame.declare_integer ("check_hour", Attribute::Const, 
		  "Hour in day to update TSum.");
      frame.set ("check_hour", 6);
      frame.declare_integer ("reset_mday", Attribute::Const, 
		  "Day in month to reset TSum.");
      frame.set ("reset_mday", 1);
      frame.declare_integer ("reset_month", Attribute::Const, 
		  "Month in year to reset TSum.");
      frame.set ("reset_month", 3);
      frame.declare ("TSum_limit", "dg C d", Attribute::Const, "\
Temeperature sum above which the condition becomes true.");
      frame.declare ("TSum_now", "dg C d", Attribute::OptionalState, "\
Current temeprature sum since last reset.");
      frame.order ("TSum_limit");
  }
} ConditionTSumAbove_syntax;

// The 'daily_air_temperature' Model.

struct ConditionDailyAirTemperature : public Condition
{
  const double temperature;

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return (daisy.field ().daily_air_temperature () > temperature); }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionDailyAirTemperature (const BlockModel& al)
    : Condition (al),
      temperature (al.number ("temperature"))
  { }
};

static struct ConditionDailyAirTemperatureSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionDailyAirTemperature (al); }
  ConditionDailyAirTemperatureSyntax ()
    : DeclareModel (Condition::component, "daily_air_temperature_above", "\
Test if the daily air is warmer than the specified temperature.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("temperature", "dg C", Attribute::Const, "\
Lowest air temperature for which the condition is true.");
    frame.order ("temperature");
  }
} ConditionDailyAirTemperature_syntax;

// The 'daily_precipitation' Model.

struct ConditionDailyPrecipitation : public Condition
{
  const double precipitation;

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return (daisy.field ().daily_precipitation () > precipitation); }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionDailyPrecipitation (const BlockModel& al)
    : Condition (al),
      precipitation (al.number ("precipitation"))
  { }
};

static struct ConditionDailyPrecipitationSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionDailyPrecipitation (al); }
  ConditionDailyPrecipitationSyntax ()
    : DeclareModel (Condition::component, "daily_precipitation_above", "\
Test if the daily precipitation is warmer than the specified value.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("precipitation", "mm", Attribute::Const, "\
Lowest precipitation for which the condition is true.");
    frame.order ("precipitation");
  }
} ConditionDailyPrecipitation_syntax;

// condition_weather.C ends here.
