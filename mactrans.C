// mactrans.C --- Macropore transportation of solutes.

#include "mactrans.h"

EMPTY_TEMPLATE
Librarian<Mactrans>::Content* Librarian<Mactrans>::content = NULL;

const char *const Mactrans::description = "\
Macropore transportation of solutes.";

Mactrans::Mactrans (const AttributeList& al)
  : name (al.name ("type"))
{ }

Mactrans::~Mactrans ()
{ }
