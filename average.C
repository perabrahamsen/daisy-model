// average.C --- Find the average of two numbers.

#include "average.h"

EMPTY_TEMPLATE
Librarian<Average>::Content* Librarian<Average>::content = NULL;

const char *const Average::description = "\
Find the average of two numbers..";

Average::Average (const AttributeList& al)
  : name (al.name ("type"))
{ }

Average::~Average ()
{ }
