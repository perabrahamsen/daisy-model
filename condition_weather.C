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

#include "condition.h"
#include "block.h"
#include "field.h"
#include "daisy.h"
#include "check.h"
#include "log.h"
#include "librarian.h"
#include <sstream>

struct ConditionTSum : public Condition
{
  // Parameters.
  const int check_hour;
  const int reset_mday;
  const int reset_month;
  const double TSum_limit;

  // State.
  double TSum_now;

  void tick (const Daisy& daisy, Treelog&)
  {
    if (daisy.time.hour () == check_hour)
      {
	if (daisy.time.mday () == reset_mday
	    && daisy.time.month () == reset_month)
	  TSum_now = 0.0;
	const double T = daisy.field->daily_air_temperature ();
	if (T > 0.0)
	  TSum_now += T;
      }
  }

  bool match (const Daisy&, Treelog&) const
  { return TSum_now > TSum_limit; }

  void output (Log& log) const
  { output_variable (TSum_now, log); }

  ConditionTSum (Block& al)
    : Condition (al),
      check_hour (al.integer ("check_hour")),
      reset_mday (al.integer ("reset_mday")),
      reset_month (al.integer ("reset_month")),
      TSum_limit (al.number ("TSum_limit")),
      TSum_now (al.number ("TSum_now", -100.0e100))
  { }
};
static struct ConditionWeatherSyntax
{
  static Model& make (Block& al)
  { return *new ConditionTSum (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
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
  ConditionWeatherSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      syntax.add_check (check_alist);
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Test if the temperature sum is above the specified value\n\
The temperature sum is the sum of the daily average air temperature since\n\
last reset.  It is reset once a year.  Days where the average is below 0\n\
does not count in the sum.");

      syntax.add ("check_hour", Syntax::Integer, Syntax::Const, 
		  "Hour in day to update TSum.");
      alist.add ("check_hour", 6);
      syntax.add ("reset_mday", Syntax::Integer, Syntax::Const, 
		  "Day in month to reset TSum.");
      alist.add ("reset_mday", 1);
      syntax.add ("reset_month", Syntax::Integer, Syntax::Const, 
		  "Month in year to reset TSum.");
      alist.add ("reset_month", 3);
      syntax.add ("TSum_limit", "dg C d", Syntax::Const, "\
Temeperature sum above which the condition becomes true.");
      syntax.add ("TSum_now", "dg C d", Syntax::OptionalState, "\
Current temeprature sum since last reset.");
      syntax.order ("TSum_limit");
      Librarian::add_type (Condition::component, "TSum_above",
				      alist, syntax, &make);
    }
  }
} ConditionWeather_syntax;
