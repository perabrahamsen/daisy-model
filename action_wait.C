// action_wait.C
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

#define BUILD_DLL

#include "action.h"
#include "block_model.h"
#include "condition.h"
#include "log.h"
#include "daisy.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include "vcheck.h"
#include <memory>
#include <sstream>

struct ActionWait : public Action
{
  std::unique_ptr<Condition> condition;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  { condition->tick (daisy, scope, out); }

  void doIt (Daisy&, const Scope&, Treelog&)
  { }

  bool done (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { return condition->match (daisy, scope, msg); }

  void output (Log& log) const
  { output_object (condition, "condition", log); }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& out)
  { condition->initialize (daisy, scope, out); }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& out) const
  { return condition->check (daisy, scope, out); }

  ActionWait (const BlockModel& al)
    : Action (al),
      condition (Librarian::build_item<Condition> (al, "condition"))
  { }

  ~ActionWait ()
  { }
};

struct ActionWaitDays : public Action
{
  const int days;
  const int hours;
  bool activated;
  Time end_time;

  void doIt (Daisy& daisy, const Scope&, Treelog&)
  { 
    if (!activated)
      {
	activated = true;
	end_time = daisy.time ();
	end_time.tick_hour (hours - 1);
	end_time.tick_day (days);
      }
  }

  bool done (const Daisy& daisy, const Scope&, Treelog&) const
  {
    daisy_assert (activated);
    return daisy.time () >= end_time; 
  }

  void output (Log& log) const
  { 
    if (activated)
      output_submodule (end_time, "end_time", log);
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionWaitDays (const BlockModel& al)
    : Action (al),
      days (al.integer ("days")),
      hours (al.integer ("hours")),
      activated (al.check ("end_time")),
      end_time (1, 1, 1, 1)
  { 
    if (activated)
      end_time = Time (al.submodel ("end_time"));
  }

  ~ActionWaitDays ()
  { }
};

struct ActionWaitMMDD : public Action
{
  const int month;
  const int day;
  const int hour;

  void doIt (Daisy&, const Scope&, Treelog&)
  { }

  bool done (const Daisy& daisy, const Scope&, Treelog&) const
  { 
    return daisy.time ().month () == month
      && daisy.time ().mday () == day 
      && daisy.time ().hour () == hour; 
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionWaitMMDD (const BlockModel& al)
    : Action (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour"))
  { }

  ~ActionWaitMMDD ()
  { }
};

static struct ActionWaitSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionWait (al); }
  ActionWaitSyntax ()
    : DeclareModel (Action::component, "wait", "\
Wait until the specified condition is true.")
  { }
  void load_frame (Frame& frame) const
  {
      frame.declare_object ("condition", Condition::component, 
                         "Condition to wait for.");
      frame.order ("condition");
  }
} ActionWait_syntax;

static struct ActionWaitPeriodSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionWaitDays (al); }
  ActionWaitPeriodSyntax ()
    : DeclareModel (Action::component, "wait_period", "\
Waits the specified period.")
  { }
  static bool check_alist (const Metalib&, const Frame& frame, Treelog& err)
  {
    bool ok = true;

    const int days = frame.integer ("days");
    const int hours = frame.integer ("hours");

    if (days * 24 + hours < 1)
      {
	err.entry ("you must wait at least 1 hour");
	ok = false;
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);	
    frame.declare_integer ("days", Attribute::Const, 
                "Wait this number of days.");
    frame.declare_integer ("hours", Attribute::Const, 
                "Wait this number of hours.");
    frame.declare_submodule ("end_time", Attribute::OptionalState,
                          "Wait until this date.\
Setting this overrides the 'days' and 'hours' parameters.", Time::load_syntax);
  }
} ActionWaitPeriod_syntax;

static struct ActionWaitDaysSyntax : public DeclareParam
{
  ActionWaitDaysSyntax ()
    : DeclareParam (Action::component, "wait_days", "wait_period", "\
Waits the specified number of days.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("hours", 0);
    frame.order ("days");
  }
} ActionWaitDays_syntax;

static struct ActionWaitHoursSyntax : public DeclareParam
{
  ActionWaitHoursSyntax ()
    : DeclareParam (Action::component, "wait_hours", "wait_period", "\
Waits the specified number of hours.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("days", 0);
    frame.order ("hours");
  }
} ActionWaitHours_syntax;

static struct ActionWaitMMDDSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionWaitMMDD (al); }
  ActionWaitMMDDSyntax ()
    : DeclareModel (Action::component, "wait_mm_dd", "\
Wait until a specific month and day in the year.")
  { }
  static bool check_alist (const Metalib&, const Frame& frame, Treelog& err)
  {
    bool ok = true;

    const int mm = frame.integer ("month");
    const int dd = frame.integer ("day");
    const int hh = frame.integer ("hour");

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
    if (hh < 0 || hh > 23)
      {
	err.entry ("hour should be between 0 and 23");
	ok = false;
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);	
    frame.declare_integer ("month", Attribute::Const, 
                "Wait until this month.");
    frame.declare_integer ("day", Attribute::Const, 
		  "Wait until this day in the month.");
    frame.declare_integer ("hour", Attribute::Const, 
                "Wait until this hour.");
    frame.set ("hour", 8);
    frame.order ("month", "day");
  }
} ActionWaitMMDD_syntax;

// The 'at' action.

struct ActionAt : public Action
{
  const Time time;
  std::unique_ptr<Action> action;

  void tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { action->tick (daisy, scope, msg); }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    // msg.message ("doIt");
    if (time == daisy.time ()){
      // msg.message ("doIt");
      action->doIt (daisy, scope, msg);
    }
  }
  bool done (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  { 
    // msg.message ("done: " + daisy.time ().print () + " < " + time.print ());
    if (daisy.time () < time)
      return false;
    // msg.message ("done!");

    return action->done (daisy, scope, msg);
  }
  void output (Log& log) const
  { 
    output_object (action, "do", log);
  }
  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { action->initialize (daisy, scope, msg); }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    bool ok = true; 
    if (!action->check (daisy, scope, msg))
      ok = false;
    return ok;
  }
  ActionAt (const BlockModel& al)
    : Action (al),
      time (al.integer ("year"), al.integer ("month"), al.integer ("day"),
	    al.integer ("hour")),
      action (Librarian::build_item<Action> (al, "do"))
  { }

  ~ActionAt ()
  { }
};

static struct ActionAtSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionAt (al); }
  ActionAtSyntax ()
    : DeclareModel (Action::component, "at", "\
Do action at specific time.")
  { }
  static bool check_alist (const Metalib&, const Frame& frame, Treelog& err)
  {
    bool ok = true;

    const int yy = frame.integer ("year");
    const int mm = frame.integer ("month");
    const int dd = frame.integer ("day");
    const int hh = frame.integer ("hour");

    if (!Time::valid (yy, mm, dd, hh))
      {
	err.entry ("Invalid time");
	ok = false;
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);	
    frame.declare_integer ("year", Attribute::Const, 
			   "Perform action this year.");
    frame.set_check ("year", VCheck::valid_year ());
    frame.declare_integer ("month", Attribute::Const, 
			   "Perform action this month.");
    frame.set_check ("month", VCheck::valid_month ());
    frame.declare_integer ("day", Attribute::Const, 
			   "Perform action this day in the month.");
    frame.set_check ("day", VCheck::valid_mday ());
    frame.declare_integer ("hour", Attribute::Const, 
			   "Perform action this hour.");
    frame.set_check ("hour", VCheck::valid_hour ());
    frame.set ("hour", 8);
    frame.declare_object ("do", Action::component, 
			  "Action to perform at specified tome..");

    frame.order ("year", "month", "day");
  }
} ActionAt_syntax;

// action_wait.C ends here.
