// chemical.C

#include "chemical.h"

Librarian<Chemical>::Content* Librarian<Chemical>::content = NULL;

Chemical::Chemical (const AttributeList& al)
  : name (al.name ("type"))
{ }

Chemical::~Chemical ()
{ }

