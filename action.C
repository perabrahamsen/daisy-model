// action.C -- Manager actions

#include "action.h"
#include "column.h"

void 
Action::doIt (ColumnList&, const Weather&, Log&) const
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
ActionSow::doIt (ColumnList& cl, const Weather&, Log& log) const
{
  cout << "Sowing " << crop << "\n";

  for (ColumnList::iterator i = cl.begin ();
       i != cl.end ();
       i++)
    {
      (*i)->sow (crop, log);
    }
}

ActionSow::ActionSow (string c) : crop (c)
{ }

void 
ActionStop::doIt (ColumnList&, const Weather&, Log&) const
{
  assert (false);
}

bool
ActionStop::stop () const
{ 
  return true;
}
