// adsorption.C

#include "adsorption.h"

Librarian<Adsorption>::Content* Librarian<Adsorption>::content = NULL;

void
Adsorption::output (Log&, Filter&) const
{ }

Adsorption::Adsorption (const string& n)
  : name (n)
{ }

Adsorption::~Adsorption ()
{ }
