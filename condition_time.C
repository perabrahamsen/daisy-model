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
    AttributeList& alist = *new AttributeList ();
    syntax.add ("time", Syntax::Date, Syntax::Const);
    syntax.order ("time");
    Librarian<Condition>::add_type ("at", alist, syntax,
				    &ConditionAt::make);
    Librarian<Condition>::add_type ("before", alist, syntax, 
				    &ConditionBefore::make);
    Librarian<Condition>::add_type ("after", alist, syntax,
				    &ConditionAfter::make);
  }
  // Every nth something.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("step", Syntax::Integer, Syntax::Const);
    syntax.order ("step");
    alist.add ("step", 1);
    Librarian<Condition>::add_type ("hourly", alist, syntax,
				    &ConditionHourly::make);
    Librarian<Condition>::add_type ("daily", alist, syntax,
				    &ConditionDaily::make);
    Librarian<Condition>::add_type ("weekly", alist, syntax,
				    &ConditionWeekly::make);
    Librarian<Condition>::add_type ("monthly", alist, syntax,
				    &ConditionMonthly::make);
    Librarian<Condition>::add_type ("yearly", alist, syntax,
				    &ConditionYearly::make);
  }
  // At a specific hour/mday/yday/year.
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("at", Syntax::Integer, Syntax::Const);
    syntax.order ("at");
    Librarian<Condition>::add_type ("hour", alist, syntax,
				    &ConditionHour::make);
    Librarian<Condition>::add_type ("mday", alist, syntax,
				    &ConditionMDay::make);
    Librarian<Condition>::add_type ("yday", alist, syntax,
				    &ConditionYDay::make);
    Librarian<Condition>::add_type ("month", alist, syntax,
				    &ConditionMonth::make);
    Librarian<Condition>::add_type ("year", alist, syntax,
				    &ConditionYear::make);
  }
}
