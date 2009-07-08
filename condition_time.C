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
#include "time.h"
#include "daisy.h"
#include "vcheck.h"
#include "librarian.h"
#include "timestep.h"
#include "log.h"
#include "submodeler.h"
#include "treelog.h"
#include "frame.h"
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
      time (al.submodel ("time"))
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
      time (al.submodel ("time"))
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
      time (al.submodel ("time"))
  { }
  static Model& make (Block& al)
  { return *new ConditionAfter (al); }
};

struct ConditionHour : public Condition
{
  const int at;
  symbol timestep ()
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
  const symbol dt;

  symbol timestep ()
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

static struct ConditionMM_DDBase : public DeclareBase
{
  ConditionMM_DDBase ()
    : DeclareBase (Condition::component, "mm_dd_base", "\
Conditions based on month and day.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
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
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_integer ("month", Attribute::Const, 
               "Month to test for.");
    frame.set_check ("month", VCheck::valid_month ());
    frame.declare_integer ("day", Attribute::Const, 
               "Day in the month to test for.");
    frame.set_check ("day", VCheck::valid_mday ());
    frame.declare_integer ("hour", Attribute::Const, 
               "Hour to test for.");
    frame.set_check ("hour", VCheck::valid_hour ());
    frame.set ("hour", 8);
    frame.declare_integer ("minute", Attribute::Const, 
               "Minute to test for.");
    frame.set_check ("hour", VCheck::valid_minute ());
    frame.set ("minute", 0);
    frame.declare_integer ("second", Attribute::Const, 
               "Second to test for.");
    frame.set_check ("second", VCheck::valid_second ());
    frame.set ("second", 0);
    frame.order ("month", "day");
  }
} ConditionMM_DD_base;

static struct ConditionMMDDSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionMMDD (al); }
  ConditionMMDDSyntax ()
    : DeclareModel (Condition::component, "mm_dd", "mm_dd_base", "\
True a specific month, day and hour in the year.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionMMDD_syntax;

static struct ConditionBeforeMMDDSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionBeforeMMDD (al); }
  ConditionBeforeMMDDSyntax ()
    : DeclareModel (Condition::component, "before_mm_dd", "mm_dd_base", "\
True before specific month, day and hour in the year.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionBeforeMMDD_syntax;

static struct ConditionAfterMMDDSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionAfterMMDD (al); }
  ConditionAfterMMDDSyntax ()
    : DeclareModel (Condition::component, "after_mm_dd", "mm_dd_base", "\
True after specific month, day and hour in the year.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionAfterMMDD_syntax;

static struct ConditionTimeBase : public DeclareBase
{
  ConditionTimeBase ()
    : DeclareBase (Condition::component, "time", "\
Conditions based on a specific time.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule ("time", Attribute::Const,
                         "Fixed time to test for.", Time::load_syntax);
    frame.order ("time");
  }
} ConditionTime_base;

static struct ConditionAtSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionAt (al); }
  ConditionAtSyntax ()
    : DeclareModel (Condition::component, "at", "time", "\
True, iff the simulation time is at the specified time.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionAt_syntax;

static struct ConditionBeforeSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionBefore (al); }
  ConditionBeforeSyntax ()
    : DeclareModel (Condition::component, "before", "time", "\
True, iff the simulation time is before the specified time.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionBefore_syntax;

static struct ConditionAfterSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionAfter (al); }
  ConditionAfterSyntax ()
    : DeclareModel (Condition::component, "after", "time", "\
True, iff the simulation time is after the specified time.")
  { }
  void load_frame (Frame&) const
  { }
} ConditionAfter_syntax;

static struct ConditionHourSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionHour (al); }
  ConditionHourSyntax ()
    : DeclareModel (Condition::component, "hour", "\
True, at the specified hour.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("at", Attribute::Const,
                "Hour when the condition is true [0-23].");
    frame.set_check ("at", VCheck::valid_hour ());
    frame.order ("at");
  }
} ConditionHour_syntax;

static struct ConditionMDaySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionMDay (al); }
  ConditionMDaySyntax ()
    : DeclareModel (Condition::component, "mday", "\
True, at the specified day in the month.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("at", Attribute::Const,
		"Day in the month when the condition is true [1-31].");
    frame.set_check ("at", VCheck::valid_mday ());
    frame.order ("at");
  }
} ConditionMDay_syntax;

static struct ConditionYDaySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionYDay (al); }
  ConditionYDaySyntax ()
    : DeclareModel (Condition::component, "yday", "\
True, at the specified julian day.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("at", Attribute::Const,
                "Julian day when the condition is true [1-366].");
    static VCheck::IRange valid_jday (1, 366);
    frame.set_check ("at", valid_jday);
    frame.order ("at");
  }
} ConditionYDay_syntax;

static struct ConditionMonthSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionMonth (al); }
  ConditionMonthSyntax ()
    : DeclareModel (Condition::component, "month", "\
True, at the specified month.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("at", Attribute::Const,
                "Month when the condition is true [1-12].");
    frame.set_check ("at", VCheck::valid_month ());
    frame.order ("at");
  }
} ConditionMonth_syntax;

static struct ConditionYearSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionYear (al); }
  ConditionYearSyntax ()
    : DeclareModel (Condition::component, "year", "\
True, at the specified year.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("at", Attribute::Const,
		"Year when the condition is true.");
    frame.set_check ("at", VCheck::valid_year ());
    frame.order ("at");
  }
} ConditionYear_syntax;

static struct ConditionTimestepSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionTimestep (al); }
  ConditionTimestepSyntax ()
    : DeclareModel (Condition::component, "timestep", "\
Add a timestep to a condition.\n\
It is true whenever 'operand' is true, but will let Daisy know what\n\
'timestep' it represents.  The timestep is used for the dimension\n\
in log files.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("operand", Condition::component, 
                       "Condition to use.");
    frame.declare_string ("timestep", Attribute::Const, "\
Timestep to use.");
    frame.order ("operand", "timestep");
  }
} ConditionTimestep_syntax;

// The 'end' base model.

struct ConditionEnd : public Condition
{
  const symbol timestep_name;
  typedef int (Time::*entry_type) () const;
  entry_type entry;

  symbol timestep ()
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

  ConditionEnd (Block& al, const symbol tstep, entry_type e)
    : Condition (al),
      timestep_name (tstep),
      entry (e)
  { }
};

// The 'hourly' model.

static struct ConditionHourlySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionEnd (al, "h", &Time::hour); }

  ConditionHourlySyntax ()
    : DeclareModel (Condition::component, "hourly", "True at the end of each hour.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} ConditionHourly_syntax;

// The 'secondly' model.

static struct ConditionSecondlySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionEnd (al, "s", &Time::second); }

  ConditionSecondlySyntax ()
    : DeclareModel (Condition::component, "secondly", "True at the end of each second.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} ConditionSecondly_syntax;

// The 'minutely' model.

static struct ConditionMinutelySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionEnd (al, "min", &Time::minute); }

  ConditionMinutelySyntax ()
    : DeclareModel (Condition::component, "minutely", "True at the end of each minute.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} ConditionMinutely_syntax;

// The 'daily' model.

static struct ConditionDailySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionEnd (al, "d", &Time::mday); }

  ConditionDailySyntax ()
    : DeclareModel (Condition::component, "daily", "True at the end of each day.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} ConditionDaily_syntax;

// The 'weekly' model.

static struct ConditionWeeklySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionEnd (al, "w", &Time::week); }

  ConditionWeeklySyntax ()
    : DeclareModel (Condition::component, "weekly", "True at the end of each week.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} ConditionWeekly_syntax;

// The 'monthly' model.

static struct ConditionMonthlySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionEnd (al, "m", &Time::month); }

  ConditionMonthlySyntax ()
    : DeclareModel (Condition::component, "monthly", "True at the end of each month.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} ConditionMonthly_syntax;

// The 'yearly' model.

static struct ConditionYearlySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionEnd (al, "y", &Time::year); }

  ConditionYearlySyntax ()
    : DeclareModel (Condition::component, "yearly", "True at the end of each year.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} ConditionYearly_syntax;

// The 'interval' base model.

struct ConditionInterval : public Condition
{
private:
  const Timestep interval;
  const symbol step;
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

private:
  static symbol find_timestep (const Timestep& tstep)
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

static struct ConditionEverySyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ConditionInterval (al, submodel_value_block<Timestep> (al)); }

  ConditionEverySyntax ()
    : DeclareModel (Condition::component, "every", "Matches simulation with fixed time intervals.")
  { }
  void load_frame (Frame& frame) const
  {
    Timestep::load_frame (frame); // We steal all timestep attributes.
    frame.declare_submodule ("next", Attribute::OptionalState,
                         "Time for next match.",
                         Time::load_syntax);
  }
} ConditionEvery_syntax;

// condition_time.C ends here.
