// condition.h -- Logic expressions

#include "daisy.h"
#include "time.h"

class Condition
{  
public:
    virtual bool match (ColumnList&, const Bioclimate&, const Time&) const;
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
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionAt (const Time&);
};

class ConditionBefore : public Condition
{
    const Time time;
public:
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionBefore (const Time&);
};

class ConditionAfter : public Condition
{
    const Time time;
public:
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionAfter (const Time&);
};

class ConditionHourly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionHourly (int);
};

class ConditionDaily : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionDaily (int);
};

class ConditionWeekly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionWeekly (int);
};

class ConditionMonthly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionMonthly (int);
};

class ConditionYearly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Bioclimate&, const Time&) const;
    ConditionYearly (int);
};

