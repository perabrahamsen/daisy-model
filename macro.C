// macro.C --- Preferention flow in soil macro pores.

#include "macro.h"

Librarian<Macro>::Content* Librarian<Macro>::content = NULL;

const char *const Macro::description = "\
Preferention flow in soil macro pores.";

Macro::Macro (const AttributeList& al)
  : name (al.name ("type"))
{ }

Macro::~Macro ()
{ }

