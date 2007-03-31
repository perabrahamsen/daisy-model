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


#include "action.h"
#include "block.h"
#include "condition.h"
#include "log.h"
#include "daisy.h"
#include "assertion.h"
#include "librarian.h"
#include <memory>
#include <sstream>

struct ActionWait : public Action
{
  std::auto_ptr<Condition> condition;

  void tick (const Daisy& daisy, Treelog& out)
  { condition->tick (daisy, out); }

  void doIt (Daisy&, Treelog&)
  { }

  bool done (const Daisy& daisy, Treelog& msg) const
  { return condition->match (daisy, msg); }

  void output (Log& log) const
  { output_derived (condition, "condition", log); }

  ActionWait (Block& al)
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

  void doIt (Daisy& daisy, Treelog&)
  { 
    if (!activated)
      {
	activated = true;
	end_time = daisy.time;
	end_time.tick_hour (hours - 1);
	end_time.tick_day (days);
      }
  }

  bool done (const Daisy& daisy, Treelog&) const
  {
    daisy_assert (activated);
    return daisy.time >= end_time; 
  }

  void output (Log& log) const
  { 
    if (activated)
      output_submodule (end_time, "end_time", log);
  }

  ActionWaitDays (Block& al)
    : Action (al),
      days (al.integer ("days")),
      hours (al.integer ("hours")),
      activated (al.check ("end_time")),
      end_time (1, 1, 1, 1)
  { 
    if (activated)
      end_time = Time (al.alist ("end_time"));
  }

  ~ActionWaitDays ()
  { }
};

struct ActionWaitMMDD : public Action
{
  const int month;
  const int day;
  const int hour;

  void doIt (Daisy&, Treelog&)
  { }

  bool done (const Daisy& daisy, Treelog&) const
  { 
    return daisy.time.month () == month
      && daisy.time.mday () == day 
      && daisy.time.hour () == hour; 
  }

  ActionWaitMMDD (Block& al)
    : Action (al),
      month (al.integer ("month")),
      day (al.integer ("day")),
      hour (al.integer ("hour"))
  { }

  ~ActionWaitMMDD ()
  { }
};

static struct ActionWaitSyntax
{
  static Model& make (Block& al)
  { return *new ActionWait (al); }
  static Model& make_days (Block& al)
  { return *new ActionWaitDays (al); }
  static Model& make_mm_dd (Block& al)
  { return *new ActionWaitMMDD (al); }

  static bool check_mm_dd (const AttributeList& alist, Treelog& err)
  {
    bool ok = true;

    const int mm = alist.integer ("month");
    const int dd = alist.integer ("day");
    const int hh = alist.integer ("hour");

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
  static bool check_days (const AttributeList& alist, Treelog& err)
  {
    bool ok = true;

    const int days = alist.integer ("days");
    const int hours = alist.integer ("hours");

    if (days * 24 + hours < 1)
      {
	err.entry ("you must wait at least 1 hour");
	ok = false;
      }
    return ok;
  }

  ActionWaitSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Wait until the specified condition is true.");
      syntax.add_object ("condition", Condition::component, 
                         "Condition to wait for.");
      syntax.order ("condition");
      Librarian::add_type (Action::component, "wait", alist, syntax, &make);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add_check (check_days);	
      alist.add ("description", "\
Waits the specified number of days.");
      syntax.add ("days", Syntax::Integer, Syntax::Const, 
		  "Wait this number of days.");
      syntax.add ("hours", Syntax::Integer, Syntax::Const, 
		  "Wait this number of hours.");
      alist.add ("hours", 0);
      syntax.add_submodule ("end_time", alist, Syntax::OptionalState,
		  "Wait until this date.\
Setting this overrides the 'days' and 'hours' parameters.", Time::load_syntax);
      syntax.order ("days");
      Librarian::add_type (Action::component, "wait_days", alist, syntax, &make_days);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add_check (check_days);	
      alist.add ("description", "\
Waits the specified number of hours.");
      syntax.add ("days", Syntax::Integer, Syntax::Const, 
		  "Wait this number of days.");
      alist.add ("days", 0);
      syntax.add ("hours", Syntax::Integer, Syntax::Const, 
		  "Wait this number of hours.");
      syntax.add_submodule ("end_time", alist, Syntax::OptionalState,
		  "Wait until this date.\
Setting this overrides the 'days' and 'hours' parameters.", Time::load_syntax);
      syntax.order ("hours");
      Librarian::add_type (Action::component, "wait_hours", alist, syntax, &make_days);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add_check (check_mm_dd);	
      alist.add ("description", "\
Wait until a specific month and day in the year.");
      syntax.add ("month", Syntax::Integer, Syntax::Const, 
		  "Wait until this month.");
      syntax.add ("day", Syntax::Integer, Syntax::Const, 
		  "Wait until this day in the month.");
      syntax.add ("hour", Syntax::Integer, Syntax::Const, 
		  "Wait until this hour.");
      alist.add ("hour", 8);
      syntax.order ("month", "day");
      Librarian::add_type (Action::component, "wait_mm_dd", alist, syntax, &make_mm_dd);
    }
  }
} ActionWait_syntax;
