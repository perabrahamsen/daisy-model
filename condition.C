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
    return day == d && hour == h;
}

ConditionBefore::ConditionBefore (int d, int h) : day (d), hour (h)
{ }

bool
ConditionAfter::match (ColumnList&, const Wheather&, int d, int h) const
{
    return day == d && hour == h;
}

ConditionAfter::ConditionAfter (int d, int h) : day (d), hour (h)
{ }
