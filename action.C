// action.C -- Manager actions

#include "action.h"
#include "common.h"

Librarian<Action>::Content* Librarian<Action>::content = NULL;

const char *const Action::description = "\
The `action' component represents management on different abstraction\n\
levels, from a single tillage operation to strategies of how to manage\n\
a farm.  Typically, but not necessarily, the high level management\n\
strategies are build by combining low level management operations.";

void 
Action::output (Log&) const
{ }

bool
Action::done (const Daisy&) const
{ return true; }

bool
Action::check (const Daisy&, Treelog&) const
{ return true; }

Action::Action (const AttributeList& al)
  : name (al.name ("type")),
    alist (al)
{ }

Action::~Action ()
{ }
