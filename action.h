// action.h -- Manager actions

#ifndef ACTION_H
#define ACTION_H

#include "daisy.h"

class Action
{
public:
    virtual void doIt(ColumnList&, const Bioclimate&, const Library&,
		      Log&) const;
    virtual bool stop () const;
    static Action null;
protected:
    Action ();
public:
    virtual ~Action ();
};

class ActionSow : public Action
{
    const string crop;
public:
    void doIt (ColumnList&, const Bioclimate&, const Library&, Log&) const;
    ActionSow (string);
};

class ActionStop : public Action
{
public:
    void doIt (ColumnList&, const Bioclimate&, const Library&, Log&) const;
    bool stop () const;
};

#endif ACTION_H
