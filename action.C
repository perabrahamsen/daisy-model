// action.C -- Manager actions

#include "action.h"
#include "common.h"

Librarian<Action>::Content* Librarian<Action>::content = NULL;

bool
Action::check (Daisy&) const
{ return true; }

Action::Action (const string& n)
  : name (n)
{ }

Action::~Action ()
{ }
