// condition_time.C

#include "condition.h"
#include "time.h"
#include "daisy.h"

struct ConditionAt : public Condition
{
  const Time time;
public:
  bool match (const Daisy& daisy) const
  { return time == daisy.time; }
  ConditionAt (const AttributeList& al)
    : time (al.time ("time"))
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
  ConditionBefore (const AttributeList& al)
    : time (al.time ("time"))
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
  ConditionAfter (const AttributeList& al)
    : time (al.time ("time"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionAfter (al); }
};

// BUG: The following classes behave strangely around new year.

struct ConditionHourly : public Condition
{
  const int step;
public:
  bool match (const Daisy& daisy) const
  { return ((24 * daisy.time.yday () + daisy.time.hour ()) % step) == 0; }
  ConditionHourly (const AttributeList& al)
    : step (al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionHourly (al); }
};

struct ConditionDaily : public Condition
{
  const int step;
public:
  bool match (const Daisy& daisy) const
  { return daisy.time.hour () == 0 && (daisy.time.yday () % step) == 0; }
  ConditionDaily (const AttributeList& al)
    : step (al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionDaily (al); }
};

struct ConditionWeekly : public Condition
{
  const int step;
public:
  bool match (const Daisy& daisy) const
  { return daisy.time.hour () == 0 && (daisy.time.yday () % step) == 0; }
  ConditionWeekly (const AttributeList& al)
    : step (7 * al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionWeekly (al); }
};

struct ConditionMonthly : public Condition
{
  const int step;
public:
  bool match (const Daisy& daisy) const
  { return daisy.time.hour () == 0 && (daisy.time.yday () % step) == 0; }
  ConditionMonthly (const AttributeList& al)
    : step (30 * al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionMonthly (al); }
};

struct ConditionYearly : public Condition
{
  const int step;
public:
  bool match (const Daisy& daisy) const
  { return daisy.time.hour () == 0 && (daisy.time.yday () % step) == 0; }
  ConditionYearly (const AttributeList& al)
    : step (365 * al.integer ("step"))
  { }
  static Condition& make (const AttributeList& al)
  { return *new ConditionYearly (al); }
};

struct ConditionHour : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy) const
    { return daisy.time.hour () == at; }
  ConditionHour (const AttributeList& al)
    : at (al.integer ("at"))
    { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionHour (al); }
};

struct ConditionMDay : public Condition
{
  const int at;
public:
  bool match (const Daisy& daisy) const
    { return daisy.time.mday () == at; }
  ConditionMDay (const AttributeList& al)
    : at (al.integer ("at"))
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
  ConditionYDay (const AttributeList& al)
    : at (al.integer ("at"))
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
  ConditionMonth (const AttributeList& al)
    : at (al.integer ("at"))
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
  ConditionYear (const AttributeList& al)
    : at (al.integer ("at"))
    { }
  static Condition& make (const AttributeList& al)
    { return *new ConditionYear (al); }
};

static struct ConditionTimeSyntax
{
  ConditionTimeSyntax ();
} ConditionTime_syntax;

ConditionTimeSyntax::ConditionTimeSyntax ()
{ 
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
    syntax.add ("time", Syntax::Date, Syntax::Const,
		"Fixed time to test for.");
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
    alist_hour.add ("description", "True every `step' hours.");
    AttributeList& alist_day = *new AttributeList ();
    alist_day.add ("description", "True every `step' days.\n\
Warning, this may be imprecise around new year.");
    AttributeList& alist_week = *new AttributeList ();
    alist_week.add ("description", "True every `step' week.\n\
Warning, this may be imprecise around new year.");
    AttributeList& alist_month = *new AttributeList ();
    alist_month.add ("description", "True every `step' month.\n\
A month is concidered to be 30 days.\n\
Warning, this may be imprecise around new year.");
    AttributeList& alist_year = *new AttributeList ();
    alist_year.add ("description", "True every `step' year.");
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
  // At a specific hour/mday/yday/year.
  {
    Syntax& syntax_hour = *new Syntax ();
    AttributeList& alist_hour = *new AttributeList ();
    alist_hour.add ("description", "True, at the specified hour.");
    syntax_hour.add ("at", Syntax::Integer, Syntax::Const,
		     "Hour when the condition is true [0-23].");
    syntax_hour.order ("at");
    Syntax& syntax_mday = *new Syntax ();
    AttributeList& alist_mday = *new AttributeList ();
    alist_mday.add ("description", "True, at the specified day in the month.");
    syntax_mday.add ("at", Syntax::Integer, Syntax::Const,
		" when the condition is true [1-31].");
    syntax_mday.order ("at");
    Syntax& syntax_yday = *new Syntax ();
    AttributeList& alist_yday = *new AttributeList ();
    alist_yday.add ("description", "True, at the specified julian day.");
    syntax_yday.add ("at", Syntax::Integer, Syntax::Const,
		"Julian day when the condition is true [1-366].");
    syntax_yday.order ("at");
    Syntax& syntax_month = *new Syntax ();
    AttributeList& alist_month = *new AttributeList ();
    alist_month.add ("description", "True, at the specified month.");
    syntax_month.add ("at", Syntax::Integer, Syntax::Const,
		      "Month when the condition is true [1-12].");
    syntax_month.order ("at");
    Syntax& syntax_year = *new Syntax ();
    AttributeList& alist_year = *new AttributeList ();
    alist_year.add ("description", "True, at the specified year.");
    syntax_year.add ("at", Syntax::Integer, Syntax::Const,
		"Year when the condition is true.");
    syntax_year.order ("at");
    Librarian<Condition>::add_type ("hour", alist_hour, syntax_hour,
				    &ConditionHour::make);
    Librarian<Condition>::add_type ("mday", alist_mday, syntax_mday,
				    &ConditionMDay::make);
    Librarian<Condition>::add_type ("yday", alist_yday, syntax_yday,
				    &ConditionYDay::make);
    Librarian<Condition>::add_type ("month", alist_month, syntax_month,
				    &ConditionMonth::make);
    Librarian<Condition>::add_type ("year", alist_year, syntax_year,
				    &ConditionYear::make);
  }
}
