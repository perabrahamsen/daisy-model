// chemical.C

#include "chemical.h"

template<>
Librarian<Chemical>::Content* Librarian<Chemical>::content = NULL;

const char *const Chemical::description = "\
This component should, for a specific chemical (typically a pesticide),\n\
provide a description of the properties of interest to Daisy.";

Chemical::Chemical (const AttributeList& al)
  : name (al.name ("type"))
{ }

Chemical::~Chemical ()
{ }

