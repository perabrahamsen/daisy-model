// adsorption.C

#include "adsorption.h"

Librarian<Adsorption>::Content* Librarian<Adsorption>::content = NULL;

const char *const Adsorption::description = "\
This component describes the adsorption of a chemical to the soil,\n\
which among other things affects how large a fraction can be\n\
transported with the water.";

void
Adsorption::output (Log&) const
{ }

Adsorption::Adsorption (const string& n)
  : name (n)
{ }

Adsorption::~Adsorption ()
{ }
