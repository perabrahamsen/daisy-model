// action.h -- Manager actions

#ifndef ACTION_H
#define ACTION_H

#include <std/string.h>

struct ColumnList;
struct Weather;
struct Log;
struct AttributeList;

class Action
{
public:
    virtual void doIt(ColumnList&, const Weather&, Log&) const;
    virtual bool stop () const;
    static Action null;
protected:
    Action ();
public:
    virtual ~Action ();
};

class ActionSow : public Action
{
    const AttributeList& crop;
public:
    void doIt (ColumnList&, const Weather&, Log&) const;
    ActionSow (const AttributeList&);
};

class ActionStop : public Action
{
public:
    void doIt (ColumnList&, const Weather&, Log&) const;
    bool stop () const;
};

#endif ACTION_H
