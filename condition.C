// condition.C -- Logic expressions

#include "condition.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"
#include <map>

Librarian<Condition>::Content* Librarian<Condition>::content = NULL;

Condition::Condition ()
{ }

Condition::~Condition ()
{ }

