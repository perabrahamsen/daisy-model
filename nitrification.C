// nitrification.C

#include "nitrification.h"
#include "common.h"

Librarian<Nitrification>::Content* Librarian<Nitrification>::content = NULL;

Nitrification::Nitrification (const string n)
  : name (n)
{ }

Nitrification::~Nitrification ()
{ }

