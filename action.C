// action.C -- Manager actions

#include "action.h"
#include "common.h"

Librarian<Action>::Content* Librarian<Action>::content = NULL;

const char *const Action::description = "\
The `action' component represents management on different abstraction \
levels, from a single tillage operation to strategies of how to manage \
a farm.  Typically, but not necessarily, the high level management \
strategies are build by combining low level management operations.";

bool
Action::check (Daisy&) const
{ return true; }

Action::Action (const string& n)
  : name (n)
{ }

Action::~Action ()
{ }
