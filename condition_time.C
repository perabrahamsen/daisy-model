// condition_time.C

#include "condition.h"
#include "time.h"
#include "syntax.h"
#include "alist.h"
#include "daisy.h"

class ConditionAt : public Condition
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

class ConditionBefore : public Condition
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

class ConditionAfter : public Condition
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

class ConditionHourly : public Condition
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

class ConditionDaily : public Condition
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

class ConditionWeekly : public Condition
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

class ConditionMonthly : public Condition
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

class ConditionYearly : public Condition
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

static struct ConditionTimeSyntax
{
  ConditionTimeSyntax ();
} ConditionTime_syntax;

ConditionTimeSyntax::ConditionTimeSyntax ()
{ 
  // At, before, or after a given time.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("time", Syntax::Date, Syntax::Const);
    syntax.order ("time");
    Condition::add_type ("at", alist, syntax, &ConditionAt::make);
    Condition::add_type ("before", alist, syntax, &ConditionBefore::make);
    Condition::add_type ("after", alist, syntax, &ConditionAfter::make);
  }
  // Every nth something.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("step", Syntax::Integer, Syntax::Const);
    syntax.order ("step");
    alist.add ("step", 1);
    Condition::add_type ("hourly", alist, syntax, &ConditionHourly::make);
    Condition::add_type ("daily", alist, syntax, &ConditionDaily::make);
    Condition::add_type ("weekly", alist, syntax, &ConditionWeekly::make);
    Condition::add_type ("monthly", alist, syntax, &ConditionMonthly::make);
    Condition::add_type ("yearly", alist, syntax, &ConditionYearly::make);
  }
}
