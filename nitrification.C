// nitrification.C

#include "nitrification.h"

Librarian<Nitrification>::Content* Librarian<Nitrification>::content = NULL;

const char *const Nitrification::description = "\
The nitrification process, transforming ammonium into nitrate.";

Nitrification::Nitrification (const string& n)
  : name (n)
{ }

Nitrification::~Nitrification ()
{ }

