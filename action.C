// action.C -- Manager actions

#include "action.h"
#include "column.h"

void 
Action::doIt (ColumnList&, const Wheather&, const Library&) const
{
    cout << "resting...\n";
}

bool
Action::stop () const
{ 
    return false;
}

Action Action::null;

Action::Action ()
{ }

Action::~Action ()
{ }

void 
ActionSow::doIt (ColumnList& cl, const Wheather&, const Library& crops) const
{
    cout << "Sowing " << crop << "\n";

    for (ColumnList::iterator i = cl.begin ();
	 i != cl.end ();
	 i++)
	{
	    (*i)->sow (crops, crop);
	}
}

ActionSow::ActionSow (string c) : crop (c)
{ }

void 
ActionStop::doIt (ColumnList&, const Wheather&, const Library&) const
{
    assert (0);
}

bool
ActionStop::stop () const
{ 
    return true;
}
