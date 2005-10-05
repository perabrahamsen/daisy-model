// condition_time.C
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


#include "condition.h"
#include "time.h"
#include "daisy.h"
#include "vcheck.h"
#include <sstream>

using namespace std;

struct ConditionMMDD : public Condition
{
  const int month;
  const int day;
  const int hour;
public:
  bool match (const Daisy& daisy) const
  {
    return daisy.time.month () == month
      && daisy.time.mday () == day 
      && daisy.time.hour () == hour; 
  }
  void output (Log&) const
  { }
  ConditionMMDD (const AttributeList& al)
    : Condition (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionMMDD (al); }
};

struct ConditionBeforeMMDD : public Condition
{
  const int month;
  const int day;
  const int hour;
public:
  bool match (const Daisy& daisy) const
  {
    return daisy.time.month () < month
      || (daisy.time.month () == month
	  && daisy.time.mday () < day)
      || (daisy.time.month () == month
	  && daisy.time.mday () == day
	  && daisy.time.hour () < hour);
  }
  void output (Log&) const
  { }
  ConditionBeforeMMDD (const AttributeList& al)
    : Condition (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionBeforeMMDD (al); }
};

struct ConditionAfterMMDD : public Condition
{
  const int month;
  const int day;
  const int hour;
public:
  bool match (const Daisy& daisy) const
  {
    return daisy.time.month () > month
      || (daisy.time.month () == month
	  && daisy.time.mday () > day)
      || (daisy.time.month () == month
	  && daisy.time.mday () == day
	  && daisy.time.hour () > hour);
  }
  void output (Log&) const
  { }
  ConditionAfterMMDD (const AttributeList& al)
    : Condition (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionAfterMMDD (al); }
};

struct ConditionAt : public Condition
{
  const Time time;
public:
  bool match (const Daisy& daisy) const
  { return time == daisy.time; }
  void output (Log&) const
  { }
  ConditionAt (const AttributeList& al)
    : Condition (al),
      time (al.alist ("time"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionAt (al); }
};

struct ConditionBefore : public Condition
{
  const Time time;
public:
  bool match (const Daisy& daisy) const
  { return time > daisy.time; }
  void output (Log&) const
  { }
  ConditionBefore (const AttributeList& al)
    : Condition (al),
      time (al.alist ("time"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionBefore (al); }
};

struct ConditionAfter : public Condition
{
  const Time time;
public:
  bool match (const Daisy& daisy) const
  { return time < daisy.time; }  
  void output (Log&) const
  { }
  ConditionAfter (const AttributeList& al)
    : Condition (al),
      time (al.alist ("time"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionAfter (al); }
};

// BUG: The following classes behave strangely around new year.

struct ConditionHourly : public Condition
{
  const int step;
  const string timestep ()
  { 
    if (step == 1)
      return "h";
    return Condition::timestep ();
  } 
  bool match (const Daisy& daisy) const
  { return ((24 * daisy.time.yday ()
	     + (daisy.time.hour () + 1)) % step) == 0; }
  void output (Log&) const
  { }
  ConditionHourly (const AttributeList& al)
    : Condition (al),
      step (al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionHourly (al); }
};

struct ConditionDaily : public Condition
{
  const int step;
  const string timestep ()
  { 
    if (step == 1)
      return "d";
    return Condition::timestep ();
  } 
  bool match (const Daisy& daisy) const
  { return daisy.time.hour () == 23 && (daisy.time.yday () % step) == 0; }
  void output (Log&) const
  { }
  ConditionDaily (const AttributeList& al)
    : Condition (al),
      step (al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionDaily (al); }
};

struct ConditionWeekly : public Condition
{
  const int step;
  const string timestep ()
  { 
    if (step == 1)
      return "w";
    return Condition::timestep ();
  } 
  bool match (const Daisy& daisy) const
  { return daisy.time.hour () == 23 
      && daisy.time.wday () == 6
      && (daisy.time.week () % step) == 0; }
  void output (Log&) const
  { }
  ConditionWeekly (const AttributeList& al)
    : Condition (al),
      step (al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionWeekly (al); }
};

struct ConditionMonthly : public Condition
{
  const int step;
  const string timestep ()
  { 
    if (step == 1)
      return "m";
    return Condition::timestep ();
  } 
  bool match (const Daisy& daisy) const
  { 
    int month = daisy.time.month ();
    if (month % step != 0)
      return false;
    Time next = daisy.time;
    next.tick_hour ();
    return next.month () != month;
  }
  void output (Log&) const
  { }
  ConditionMonthly (const AttributeList& al)
    : Condition (al),
      step (al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionMonthly (al); }
};

struct ConditionYearly : public Condition
{
  const int step;
  const string timestep ()
  { 
    if (step == 1)
      return "y";
    return Condition::timestep ();
  } 
  bool match (const Daisy& daisy) const
  { 
    int year = daisy.time.year ();
    if ((year + 1) % step != 0)
      return false;
    Time next = daisy.time;
    next.tick_hour ();
    return next.year () != year;
  }

  void output (Log&) const
  { }
  ConditionYearly (const AttributeList& al)
    : Condition (al),
      step (al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionYearly (al); }
};

struct ConditionHour : public Condition
{
  const int at;
  const string timestep ()
  { return "d"; } 
  bool match (const Daisy& daisy) const
    { return daisy.time.hour () == at; }
  void output (Log&) const
    { }
  ConditionHour (const AttributeList& al)
    : Condition (al),
      at (al.integer ("at"))
    { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionHour (al); }
};

struct ConditionMDay : public Condition
{
  const int at;
  bool match (const Daisy& daisy) const
    { return daisy.time.mday () == at; }
  void output (Log&) const
    { }
  ConditionMDay (const AttributeList& al)
    : Condition (al),
      at (al.integer ("at"))
    { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionMDay (al); }
};

struct ConditionYDay : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy) const
    { return daisy.time.yday () == at; }
  void output (Log&) const
    { }
  ConditionYDay (const AttributeList& al)
    : Condition (al),
      at (al.integer ("at"))
    { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionYDay (al); }
};

struct ConditionMonth : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy) const
    { return daisy.time.month () == at; }
  void output (Log&) const
    { }
  ConditionMonth (const AttributeList& al)
    : Condition (al),
      at (al.integer ("at"))
    { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionMonth (al); }
};

struct ConditionYear : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy) const
    { return daisy.time.year () == at; }
  void output (Log&) const
    { }
  ConditionYear (const AttributeList& al)
    : Condition (al),
      at (al.integer ("at"))
    { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionYear (al); }
};

struct ConditionTimestep : public Condition
{
  auto_ptr<Condition> condition;
  const std::string dt;

  bool match (const Daisy& daisy) const
  { return condition->match (daisy); }

  void tick (const Daisy& daisy, Treelog& out)
  { condition->tick (daisy, out); }

  void output (Log&) const
  { }

  ConditionTimestep (const AttributeList& al)
    : Condition (al),
      condition (Librarian<Condition>::create (al.alist ("operand"))),
      dt (al.name ("timestep"))
  { }
  ~ConditionTimestep ()
  { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionTimestep (al); }
};

static struct ConditionTimeSyntax
{
  static bool check_mday (const AttributeList& al, Treelog& err)
  {
    bool ok = true;
    const int mm = al.integer ("month");
    const int dd = al.integer ("day");

    if (dd > Time::month_length (1 /* not a leap year */, mm))
      {
	std::ostringstream tmp;
        tmp << "last valid day of " << Time::month_name (mm) << " is "
            << Time::month_length (1, mm);
        err.entry (tmp.str ());
        ok = false;
      }
    return ok;
  }
  
  ConditionTimeSyntax ();
} ConditionTime_syntax;

ConditionTimeSyntax::ConditionTimeSyntax ()
{ 
  // Month and day.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& at_alist = *new AttributeList ();
    at_alist.add ("description", "\
True a specific month, day and hour in the year.");
    at_alist.add ("hour", 8);
    AttributeList& before_alist = *new AttributeList ();
    before_alist.add ("description", "\
True before specific month, day and hour in the year.");
    before_alist.add ("hour", 8);
    AttributeList& after_alist = *new AttributeList ();
    after_alist.add ("description", "\
True after specific month, day and hour in the year.");
    after_alist.add ("hour", 8);
    syntax.add ("month", Syntax::Integer, Syntax::Const, 
		"Month to test for.");
    syntax.add_check ("month", VCheck::valid_month ());
    syntax.add ("day", Syntax::Integer, Syntax::Const, 
		"Day in the month to test for.");
    syntax.add_check ("day", VCheck::valid_mday ());
    syntax.add ("hour", Syntax::Integer, Syntax::Const, 
		"Hour to test for.");
    syntax.add_check ("hour", VCheck::valid_hour ());
    syntax.order ("month", "day");
    syntax.add_check (check_mday);
    Librarian<Condition>::add_type ("mm_dd", at_alist, syntax,
				    &ConditionMMDD::make);
    Librarian<Condition>::add_type ("before_mm_dd", before_alist, syntax,
				    &ConditionBeforeMMDD::make);
    Librarian<Condition>::add_type ("after_mm_dd", after_alist, syntax,
				    &ConditionAfterMMDD::make);
  }
  // At, before, or after a given time.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist_at = *new AttributeList ();
    alist_at.add ("description", "\
True, iff the simulation time is at the specified time.");
    AttributeList& alist_before = *new AttributeList ();
    alist_before.add ("description", "\
True, iff the simulation time is before the specified time.");
    AttributeList& alist_after = *new AttributeList ();
    alist_after.add ("description", "\
True, iff the simulation time is after the specified time.");
    syntax.add_submodule ("time", alist_at, Syntax::Const,
			  "Fixed time to test for.", Time::load_syntax);
    syntax.order ("time");
    Librarian<Condition>::add_type ("at", alist_at, syntax,
				    &ConditionAt::make);
    Librarian<Condition>::add_type ("before", alist_before, syntax, 
				    &ConditionBefore::make);
    Librarian<Condition>::add_type ("after", alist_after, syntax,
				    &ConditionAfter::make);
  }
  // Every nth something.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist_hour = *new AttributeList ();
    alist_hour.add ("description", "True every 'step' hours.\n\
Warning, this may be imprecise around new year.");
    AttributeList& alist_day = *new AttributeList ();
    alist_day.add ("description", "True every 'step' days.\n\
Or, more precisely, at 23 hour when the Julian day modulo\n\
'step' is zero.");
    AttributeList& alist_week = *new AttributeList ();
    alist_week.add ("description", "True every 'step' week.\n\
Or, more precisely, sunday at 23 hour when the week number\n\
modulo 'step' is zero.");
    AttributeList& alist_month = *new AttributeList ();
    alist_month.add ("description", "True every 'step' month.\n\
Or, more precisely, the last hour in each month, where\n\
the month number modulo 'step' is 0.");
    AttributeList& alist_year = *new AttributeList ();
    alist_year.add ("description", "True every 'step' year.\n\
Or, more precisely, the last hour of each year, where the year\n\
plus one modulo 'step' is 0.");
    syntax.add ("step", Syntax::Integer, Syntax::Const,
		"Number of time periods between this condition is true.");
    syntax.order ("step");
    alist_hour.add ("step", 1);
    alist_day.add ("step", 1);
    alist_week.add ("step", 1);
    alist_month.add ("step", 1);
    alist_year.add ("step", 1);
    Librarian<Condition>::add_type ("hourly", alist_hour, syntax,
				    &ConditionHourly::make);
    Librarian<Condition>::add_type ("daily", alist_day, syntax,
				    &ConditionDaily::make);
    Librarian<Condition>::add_type ("weekly", alist_week, syntax,
				    &ConditionWeekly::make);
    Librarian<Condition>::add_type ("monthly", alist_month, syntax,
				    &ConditionMonthly::make);
    Librarian<Condition>::add_type ("yearly", alist_year, syntax,
				    &ConditionYearly::make);
  }
  // Specific hour.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True, at the specified hour.");
    syntax.add ("at", Syntax::Integer, Syntax::Const,
                "Hour when the condition is true [0-23].");
    syntax.add_check ("at", VCheck::valid_hour ());
    syntax.order ("at");
    Librarian<Condition>::add_type ("hour", alist, syntax,
                                    &ConditionHour::make);
  }
  // Every specific day in the month.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True, at the specified day in the month.");
    syntax.add ("at", Syntax::Integer, Syntax::Const,
		"Day in the month when the condition is true [1-31].");
    syntax.add_check ("at", VCheck::valid_mday ());
    syntax.order ("at");
    Librarian<Condition>::add_type ("mday", alist, syntax,
				    &ConditionMDay::make);
  }
  // Every specific day in the year.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True, at the specified julian day.");
    syntax.add ("at", Syntax::Integer, Syntax::Const,
                "Julian day when the condition is true [1-366].");
    static VCheck::IRange valid_jday (1, 366);
    syntax.add_check ("at", valid_jday);
    syntax.order ("at");
    Librarian<Condition>::add_type ("yday", alist, syntax,
				    &ConditionYDay::make);
  }
  // Every specific month.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True, at the specified month.");
    syntax.add ("at", Syntax::Integer, Syntax::Const,
                "Month when the condition is true [1-12].");
    syntax.add_check ("at", VCheck::valid_month ());
    syntax.order ("at");
    Librarian<Condition>::add_type ("month", alist, syntax, 
                                    &ConditionMonth::make);
  }
  // A specific year.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True, at the specified year.");
    syntax.add ("at", Syntax::Integer, Syntax::Const,
		"Year when the condition is true.");
    syntax.add_check ("at", VCheck::valid_year ());
    syntax.order ("at");
    Librarian<Condition>::add_type ("year", alist, syntax,
				    &ConditionYear::make);
  }
  // Add timestep to condition.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Add a timestep to a condition.\n\
It is true whenever 'operand' is true, but will let Daisy know what\n\
'timestep' it represents.  The timestep is used for the dimension\n\
in log files.");
    syntax.add ("operand", Librarian<Condition>::library (), 
		"Condition to use.");
    syntax.add ("timestep", Syntax::String, Syntax::Const, "\
Timestep to use.");
    syntax.order ("operand", "timestep");
    Librarian<Condition>::add_type ("timestep", alist, syntax,
				    &ConditionTimestep::make);
  }
}
