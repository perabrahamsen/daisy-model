// condition.h -- Logic expressions

#include "time.h"

struct ColumnList;
struct Weather;
struct Time;

class Condition
{  
public:
    virtual bool match (ColumnList&, const Weather&, const Time&) const;
    static Condition null;
protected:
    Condition ();
public:
    virtual ~Condition ();
};

class ConditionAt : public Condition
{
    const Time time;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionAt (const Time&);
};

class ConditionBefore : public Condition
{
    const Time time;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionBefore (const Time&);
};

class ConditionAfter : public Condition
{
    const Time time;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionAfter (const Time&);
};

class ConditionHourly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionHourly (int);
};

class ConditionDaily : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionDaily (int);
};

class ConditionWeekly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionWeekly (int);
};

class ConditionMonthly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionMonthly (int);
};

class ConditionYearly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Weather&, const Time&) const;
    ConditionYearly (int);
};

