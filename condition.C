// condition.C -- Logic expressions

#include "condition.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"
#include <map>

Librarian<Condition>::Content* Librarian<Condition>::content = NULL;

const char *const Condition::description = "\
A `condition' component tests the state of the simulation, like\n\
whether the water pressure in a specific depth is above a given\n\
threshold.  Logic conditions like `and' and `or' can be used for\n\
testing whether multiple conditions are fulfilled simultaneously.";

Condition::Condition (const AttributeList& al)
  : name (al.name ("type"))
{ }

Condition::~Condition ()
{ }

