// condition_time.C -- Conditions realted to simulation time.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "condition.h"
#include "block.h"
#include "alist.h"
#include "time.h"
#include "daisy.h"
#include "vcheck.h"
#include "librarian.h"
#include "timestep.h"
#include "log.h"
#include "submodeler.h"
#include <sstream>

struct ConditionMMDD : public Condition
{
  const int month;
  const int day;
  const int hour;
  const int minute;
  const int second;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  {
    return daisy.time.month () == month
      && daisy.time.mday () == day 
      && daisy.time.hour () == hour
      && daisy.time.minute () == minute
      && daisy.time.second () == second; 
  }

  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionMMDD (Block& al)
    : Condition (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour")),
      minute (al.integer ("minute")),
      second (al.integer ("second"))
  { }
  static Model& make (Block& al)
  { return *new ConditionMMDD (al); }
};

struct ConditionBeforeMMDD : public Condition
{
  const int month;
  const int day;
  const int hour;
  const int minute;
  const int second;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  {
    if (daisy.time.month () < month)
      return true;
    if (daisy.time.month () > month)
      return false;

    if (daisy.time.mday () < day)
      return true;
    if (daisy.time.mday () > day)
      return false;

    if (daisy.time.hour () < hour)
      return true;
    if (daisy.time.hour () > hour)
      return false;

    if (daisy.time.minute () < minute)
      return true;
    if (daisy.time.minute () > minute)
      return false;

    if (daisy.time.second () < second)
      return true;
    if (daisy.time.second () > second)
      return false;

    // Equal
    return false;
  }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionBeforeMMDD (Block& al)
    : Condition (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour")),
      minute (al.integer ("minute")),
      second (al.integer ("second"))
  { }
  static Model& make (Block& al)
  { return *new ConditionBeforeMMDD (al); }
};

struct ConditionAfterMMDD : public Condition
{
  const int month;
  const int day;
  const int hour;
  const int minute;
  const int second;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  {
    if (daisy.time.month () < month)
      return false;
    if (daisy.time.month () > month)
      return true;

    if (daisy.time.mday () < day)
      return false;
    if (daisy.time.mday () > day)
      return true;

    if (daisy.time.hour () < hour)
      return false;
    if (daisy.time.hour () > hour)
      return true;

    if (daisy.time.minute () < minute)
      return false;
    if (daisy.time.minute () > minute)
      return true;

    if (daisy.time.second () < second)
      return false;
    if (daisy.time.second () > second)
      return true;

    // Equal
    return true;
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  void output (Log&) const
  { }
  ConditionAfterMMDD (Block& al)
    : Condition (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour")),
      minute (al.integer ("minute")),
      second (al.integer ("second"))
  { }
  static Model& make (Block& al)
  { return *new ConditionAfterMMDD (al); }
};

struct ConditionAt : public Condition
{
  const Time time;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return time == daisy.time; }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionAt (Block& al)
    : Condition (al),
      time (al.alist ("time"))
  { }
  static Model& make (Block& al)
  { return *new ConditionAt (al); }
};

struct ConditionBefore : public Condition
{
  const Time time;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return time > daisy.time; }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionBefore (Block& al)
    : Condition (al),
      time (al.alist ("time"))
  { }
  static Model& make (Block& al)
  { return *new ConditionBefore (al); }
};

struct ConditionAfter : public Condition
{
  const Time time;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return time < daisy.time; }  
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionAfter (Block& al)
    : Condition (al),
      time (al.alist ("time"))
  { }
  static Model& make (Block& al)
  { return *new ConditionAfter (al); }
};

struct ConditionHour : public Condition
{
  const int at;
  const std::string timestep ()
  { return "d"; } 
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return daisy.time.hour () == at; }
  void output (Log&) const
  { }
  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionHour (Block& al)
    : Condition (al),
      at (al.integer ("at"))
  { }
  static Model& make (Block& al)
  { return *new ConditionHour (al); }
};

struct ConditionMDay : public Condition
{
  const int at;
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return daisy.time.mday () == at; }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }


  ConditionMDay (Block& al)
    : Condition (al),
      at (al.integer ("at"))
  { }
  static Model& make (Block& al)
  { return *new ConditionMDay (al); }
};

struct ConditionYDay : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return daisy.time.yday () == at; }
  void output (Log&) const
  { }
  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionYDay (Block& al)
    : Condition (al),
      at (al.integer ("at"))
  { }
  static Model& make (Block& al)
  { return *new ConditionYDay (al); }
};

struct ConditionMonth : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return daisy.time.month () == at; }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionMonth (Block& al)
    : Condition (al),
      at (al.integer ("at"))
  { }
  static Model& make (Block& al)
  { return *new ConditionMonth (al); }
};

struct ConditionYear : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return daisy.time.year () == at; }
  void output (Log&) const
  { }
  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionYear (Block& al)
    : Condition (al),
      at (al.integer ("at"))
  { }
  static Model& make (Block& al)
  { return *new ConditionYear (al); }
};

struct ConditionTimestep : public Condition
{
  std::auto_ptr<Condition> condition;
  const std::string dt;

  const std::string timestep ()
  { return dt; } 

  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { return condition->match (daisy, scope, msg); }

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { condition->tick (daisy, scope, out); }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { condition->initialize (daisy, scope, msg); }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { return condition->check (daisy, scope, msg); }

  ConditionTimestep (Block& al)
    : Condition (al),
      condition (Librarian::build_item<Condition> (al, "operand")),
      dt (al.name ("timestep"))
  { }
  ~ConditionTimestep ()
  { }
  static Model& make (Block& al)
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
    at_alist.add ("minute", 0);
    at_alist.add ("second", 0);
    AttributeList& before_alist = *new AttributeList ();
    before_alist.add ("description", "\
True before specific month, day and hour in the year.");
    before_alist.add ("hour", 8);
    before_alist.add ("minute", 0);
    before_alist.add ("second", 0);
    AttributeList& after_alist = *new AttributeList ();
    after_alist.add ("description", "\
True after specific month, day and hour in the year.");
    after_alist.add ("hour", 8);
    after_alist.add ("minute", 0);
    after_alist.add ("second", 0);
    syntax.add ("month", Syntax::Integer, Syntax::Const, 
		"Month to test for.");
    syntax.add_check ("month", VCheck::valid_month ());
    syntax.add ("day", Syntax::Integer, Syntax::Const, 
		"Day in the month to test for.");
    syntax.add_check ("day", VCheck::valid_mday ());
    syntax.add ("hour", Syntax::Integer, Syntax::Const, 
		"Hour to test for.");
    syntax.add_check ("hour", VCheck::valid_hour ());
    syntax.add ("minute", Syntax::Integer, Syntax::Const, 
		"Minute to test for.");
    syntax.add_check ("hour", VCheck::valid_minute ());
    syntax.add ("second", Syntax::Integer, Syntax::Const, 
		"Second to test for.");
    syntax.add_check ("second", VCheck::valid_second ());
    syntax.order ("month", "day");
    syntax.add_check (check_mday);
    Librarian::add_type (Condition::component, "mm_dd", at_alist, syntax,
                         &ConditionMMDD::make);
    Librarian::add_type (Condition::component, "before_mm_dd", before_alist, syntax,
                         &ConditionBeforeMMDD::make);
    Librarian::add_type (Condition::component, "after_mm_dd", after_alist, syntax,
                         &ConditionAfterMMDD::make);
  }
  // At, before, or after a given time.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList alist_time;
    syntax.add_submodule ("time", alist_time, Syntax::Const,
			  "Fixed time to test for.", Time::load_syntax);
    AttributeList& alist_at = *new AttributeList (alist_time);
    alist_at.add ("description", "\
True, iff the simulation time is at the specified time.");
    AttributeList& alist_before = *new AttributeList (alist_time);
    alist_before.add ("description", "\
True, iff the simulation time is before the specified time.");
    AttributeList& alist_after = *new AttributeList (alist_time);
    alist_after.add ("description", "\
True, iff the simulation time is after the specified time.");
    syntax.order ("time");
    Librarian::add_type (Condition::component, "at", alist_at, syntax,
                         &ConditionAt::make);
    Librarian::add_type (Condition::component, "before", alist_before, syntax, 
                         &ConditionBefore::make);
    Librarian::add_type (Condition::component, "after", alist_after, syntax,
                         &ConditionAfter::make);
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
    Librarian::add_type (Condition::component, "hour", alist, syntax,
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
    Librarian::add_type (Condition::component, "mday", alist, syntax,
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
    Librarian::add_type (Condition::component, "yday", alist, syntax,
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
    Librarian::add_type (Condition::component, "month", alist, syntax, 
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
    Librarian::add_type (Condition::component, "year", alist, syntax,
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
    syntax.add_object ("operand", Condition::component, 
                       "Condition to use.");
    syntax.add ("timestep", Syntax::String, Syntax::Const, "\
Timestep to use.");
    syntax.order ("operand", "timestep");
    Librarian::add_type (Condition::component, "timestep", alist, syntax,
                         &ConditionTimestep::make);
  }
}

// The 'end' base model.

struct ConditionEnd : public Condition
{
  const std::string timestep_name;
  typedef int (Time::*entry_type) () const;
  entry_type entry;

  const std::string timestep ()
  { return timestep_name; } 

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return (daisy.time.*entry) () != ((daisy.time + daisy.timestep).*entry) (); }

  void output (Log&) const
  { }
  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionEnd (Block& al, const std::string& tstep, entry_type e)
    : Condition (al),
      timestep_name (tstep),
      entry (e)
  { }
};

// The 'hourly' model.

static struct ConditionHourlySyntax
{
  static Model& make (Block& al)
  { return *new ConditionEnd (al, "h", &Time::hour); }

  ConditionHourlySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True at the end of each hour.");
    Librarian::add_type (Condition::component, "hourly", alist, syntax, make);
  }
} ConditionHourly_syntax;

// The 'secondly' model.

static struct ConditionSecondlySyntax
{
  static Model& make (Block& al)
  { return *new ConditionEnd (al, "s", &Time::second); }

  ConditionSecondlySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True at the end of each second.");
    Librarian::add_type (Condition::component, "secondly", alist, syntax, make);
  }
} ConditionSecondly_syntax;

// The 'minutely' model.

static struct ConditionMinutelySyntax
{
  static Model& make (Block& al)
  { return *new ConditionEnd (al, "min", &Time::minute); }

  ConditionMinutelySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True at the end of each minute.");
    Librarian::add_type (Condition::component, "minutely", alist, syntax, make);
  }
} ConditionMinutely_syntax;

// The 'daily' model.

static struct ConditionDailySyntax
{
  static Model& make (Block& al)
  { return *new ConditionEnd (al, "d", &Time::mday); }

  ConditionDailySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True at the end of each day.");
    Librarian::add_type (Condition::component, "daily", alist, syntax, make);
  }
} ConditionDaily_syntax;

// The 'weekly' model.

static struct ConditionWeeklySyntax
{
  static Model& make (Block& al)
  { return *new ConditionEnd (al, "w", &Time::week); }

  ConditionWeeklySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True at the end of each week.");
    Librarian::add_type (Condition::component, "weekly", alist, syntax, make);
  }
} ConditionWeekly_syntax;

// The 'monthly' model.

static struct ConditionMonthlySyntax
{
  static Model& make (Block& al)
  { return *new ConditionEnd (al, "m", &Time::month); }

  ConditionMonthlySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True at the end of each month.");
    Librarian::add_type (Condition::component, "monthly", alist, syntax, make);
  }
} ConditionMonthly_syntax;

// The 'yearly' model.

static struct ConditionYearlySyntax
{
  static Model& make (Block& al)
  { return *new ConditionEnd (al, "y", &Time::year); }

  ConditionYearlySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "True at the end of each year.");
    Librarian::add_type (Condition::component, "yearly", alist, syntax, make);
  }
} ConditionYearly_syntax;

// The 'interval' base model.

struct ConditionInterval : public Condition
{
private:
  const Timestep interval;
  const std::string step;
  Time next;
  bool does_match;

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return does_match; }

  void output (Log& log) const
  { output_submodule (next, "next", log); }
  
  void tick (const Daisy& daisy, const Scope&, Treelog&)
  { 
    does_match = (next <= daisy.time);

    if (!does_match)
      return;
    
    next = daisy.time + interval;
  }
  void initiate_log (const Daisy& daisy)
  { next = daisy.time + interval - daisy.timestep; }

  void initialize (const Daisy& daisy, const Scope&, Treelog&)
  { initiate_log (daisy); }
  
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

public:
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add_submodule ("next", alist, Syntax::OptionalState,
                          "Time for next match.",
                          Time::load_syntax);
  }

private:
  static std::string find_timestep (const Timestep& tstep)
  {
    if (tstep == Timestep::second ())
      return "s";
    if (tstep == Timestep::minute ())
      return "M";
    if (tstep == Timestep::hour ())
      return "h";
    if (tstep == Timestep::day ())
      return "d";
    static Timestep week (0, 7, 0, 0, 0);
    if (tstep == week)
      return "w";
    if (tstep == Timestep::year ())
      return "y";
    
    return "dt";
  }
public:
  ConditionInterval (Block& al, const Timestep tstep)
    : Condition (al),
      interval (tstep),
      step (find_timestep (tstep)),
      next (al.check ("next")
            ? submodel_value<Time> (al, "next") 
            : Time::null ()),
      does_match (false)
  { }
};

// The 'every' model.

static struct ConditionEverySyntax
{
  static Model& make (Block& al)
  { return *new ConditionInterval (al, submodel_value_block<Timestep> (al)); }

  ConditionEverySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Timestep::load_syntax (syntax, alist); // We steal all timestep attributes.
    ConditionInterval::load_syntax (syntax, alist); // Base attributes.
    alist.add ("description", "Matches simulation with fixed time intervals.");
    Librarian::add_type (Condition::component, "every", alist, syntax, make);
  }
} ConditionEvery_syntax;

// condition_time.C ends here.
