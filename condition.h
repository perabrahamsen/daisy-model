// condition.h -- Logic expressions

#include "daisy.h"

class Condition
{  
public:
    virtual bool match (ColumnList&, const Wheather&, 
			int day, int hour) const;
    static Condition null;
protected:
    Condition ();
public:
    virtual ~Condition ();
};

class ConditionAt : public Condition
{
    const int day;
    const int hour;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionAt (int d, int h);
};

class ConditionBefore : public Condition
{
    const int day;
    const int hour;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionBefore (int d, int h);
};

class ConditionAfter : public Condition
{
    const int day;
    const int hour;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionAfter (int d, int h);
};

class ConditionHourly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionHourly (int);
};

class ConditionDaily : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionDaily (int);
};

class ConditionWeekly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionWeekly (int);
};

class ConditionMonthly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionMonthly (int);
};

class ConditionYearly : public Condition
{
    const int step;
public:
    bool match (ColumnList&, const Wheather&, int d, int h) const;
    ConditionYearly (int);
};

