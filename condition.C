// condition.C -- Logic expressions

#include "condition.h"

bool
Condition::match (ColumnList&, const Bioclimate&, const Time&) const
{
    return true;
}

Condition Condition::null;

Condition::Condition ()
{ }

Condition::~Condition ()
{ }

bool
ConditionAt::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    return time == t;
}

ConditionAt::ConditionAt (const Time& t) 
    : time (t)
{ }

bool
ConditionBefore::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    return time > t;
}

ConditionBefore::ConditionBefore (const Time& t)
    : time (t)
{ }

bool
ConditionAfter::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    return time < t;
}

ConditionAfter::ConditionAfter (const Time& t)
    : time (t)
{ }

bool
ConditionHourly::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    // BUG:  Behave strangely around new year.
    return ((24 * t.yday () + t.hour ()) % step) == 0;
}

ConditionHourly::ConditionHourly (int s)
    : step (s)
{ }

bool
ConditionDaily::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    // BUG:  Behave strangely around new year.
    return t.hour () == 0 && (t.yday () % step) == 0;
}

ConditionDaily::ConditionDaily (int s)
    : step (s)
{ }

bool
ConditionWeekly::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    // BUG:  Behave strangely around new year.
    return t.hour () == 0 && (t.yday () % step) == 0;
}

ConditionWeekly::ConditionWeekly (int s)
    : step (7 * s)
{ }

bool
ConditionMonthly::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    // BUG:  Behave strangely around new year.
    return t.hour () == 0 && (t.yday () % step) == 0;
}

ConditionMonthly::ConditionMonthly (int s)
    : step (30 * s)
{ }

bool
ConditionYearly::match (ColumnList&, const Bioclimate&, const Time& t) const
{
    // BUG:  Behave strangely around new year.
    return t.hour () == 0 && (t.yday () % step) == 0;
}

ConditionYearly::ConditionYearly (int s)
    : step (365 * s)
{ }
