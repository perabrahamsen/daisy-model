// action_wait.C

#include "action.h"
#include "condition.h"
#include "log.h"
#include "daisy.h"

struct ActionWait : public Action
{
  Condition& condition;

  void doIt (Daisy&)
    { }

  bool done (const Daisy& daisy) const
    { return condition.match (daisy); }

  void output (Log& log) const
    { output_derived (condition, "condition", log); }

  ActionWait (const AttributeList& al)
    : Action (al),
      condition (Librarian<Condition>::create (al.alist ("condition")))
    { }

  ~ActionWait ()
    { delete &condition; }
};

struct ActionWaitDays : public Action
{
  const int days;
  const int hours;
  bool activated;
  Time end_time;

  void doIt (Daisy& daisy)
    { 
      if (!activated)
	{
	  activated = true;
	  end_time = daisy.time;
	  end_time.tick_hour (hours);
	  end_time.tick_day (days);
	}
    }

  bool done (const Daisy& daisy) const
    {
      assert (activated);
      return daisy.time >= end_time; 
    }

  void output (Log& log) const
    { 
      if (activated)
	log.output ("end_time", end_time);
    }

  ActionWaitDays (const AttributeList& al)
    : Action (al),
      days (al.integer ("days")),
      hours (al.integer ("hours")),
      activated (al.check ("end_time")),
      end_time (1, 1, 1, 1)
    { 
      if (activated)
	end_time = al.time ("end_time");
    }

  ~ActionWaitDays ()
    { }
};

struct ActionWaitMMDD : public Action
{
  const int month;
  const int day;
  const int hour;

  void doIt (Daisy&)
    { }

  bool done (const Daisy& daisy) const
    { 
      return daisy.time.month () == month
	&& daisy.time.mday () == day 
	&& daisy.time.hour () == hour; 
    }

  ActionWaitMMDD (const AttributeList& al)
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
  static Action& make (const AttributeList& al)
  { return *new ActionWait (al); }
  static Action& make_days (const AttributeList& al)
  { return *new ActionWaitDays (al); }
  static Action& make_mm_dd (const AttributeList& al)
  { return *new ActionWaitMMDD (al); }

  static bool check_mm_dd (const AttributeList& alist, ostream& err)
  {
    bool ok = true;

    const int mm = alist.integer ("month");
    const int dd = alist.integer ("day");
    const int hh = alist.integer ("hour");

    if (mm < 1 || mm > 12)
      {
	err << "month should be between 1 and 12\n";
	ok = false;
      }
    // don't test for bad month.
    else if (dd < 1 || dd > Time::month_length (1 /* not a leap year */, mm))
      {
	err << "day should be between 1 and " 
	    << Time::month_length (1, mm) << "\n";
	ok = false;
      }
    if (hh < 0 || hh > 23)
      {
	err << "hour should be between 0 and 23\n";
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
	syntax.add ("condition", Librarian<Condition>::library (), 
		    "Condition to wait for.");
	syntax.order ("condition");
	Librarian<Action>::add_type ("wait", alist, syntax, &make);
      }
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Wait the specified number of days.");
	syntax.add ("days", Syntax::Integer, Syntax::Const, 
		    "Wait this number of days.");
	syntax.add ("hours", Syntax::Integer, Syntax::Const, 
		    "Wait this number of days.");
	alist.add ("hours", 0);
	syntax.add ("end_time", Syntax::Date, Syntax::OptionalState,
		    "Wait until this date.\
Setting this overrides the `days' and `hours' parameters.");
	syntax.order ("days");
	Librarian<Action>::add_type ("wait_days", alist, syntax, &make_days);
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
	Librarian<Action>::add_type ("wait_mm_dd", alist, syntax, &make_mm_dd);
      }
    }
} ActionWait_syntax;
