// condition.C -- Logic expressions

#include "condition.h"

bool
Condition::match (ColumnList&, const Wheather&, int /* d */, int /* h */) const
{
    return true;
}

Condition Condition::null;

Condition::Condition ()
{ }

Condition::~Condition ()
{ }

bool
ConditionAt::match (ColumnList&, const Wheather&, int d, int h) const
{
    return day == d && hour == h;
}

ConditionAt::ConditionAt (int d, int h) : day (d), hour (h)
{ }

bool
ConditionBefore::match (ColumnList&, const Wheather&, int d, int h) const
{
    return day > d || (day == d && hour > h);
}

ConditionBefore::ConditionBefore (int d, int h) : day (d), hour (h)
{ }

bool
ConditionAfter::match (ColumnList&, const Wheather&, int d, int h) const
{
    return day < d || (day == d && hour < h);
}

ConditionAfter::ConditionAfter (int d, int h) : day (d), hour (h)
{ }

bool
ConditionHourly::match (ColumnList&, const Wheather&, int d, int h) const
{
    return ((24 * d + h) % step) == 0;
}

ConditionHourly::ConditionHourly (int s) : step (s)
{ }

bool
ConditionDaily::match (ColumnList&, const Wheather&, int d, int h) const
{
    return h == 0 && (d % step) == 0;
}

ConditionDaily::ConditionDaily (int s) : step (s)
{ }

bool
ConditionWeekly::match (ColumnList&, const Wheather&, int d, int h) const
{
    return h == 0 && (d % step) == 0;
}

ConditionWeekly::ConditionWeekly (int s) : step (7 * s)
{ }

bool
ConditionMonthly::match (ColumnList&, const Wheather&, int d, int h) const
{
    return h == 0 && (d % step) == 0;
}

ConditionMonthly::ConditionMonthly (int s) : step (30 * s)
{ }

bool
ConditionYearly::match (ColumnList&, const Wheather&, int d, int h) const
{
    return h == 0 && (d % step) == 0;
}

ConditionYearly::ConditionYearly (int s) : step (365 * s)
{ }
