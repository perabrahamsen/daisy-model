// adsorption.C

#include "adsorption.h"

Librarian<Adsorption>::Content* Librarian<Adsorption>::content = NULL;

Adsorption::Adsorption (const string& n)
  : name (n)
{ }

Adsorption::~Adsorption ()
{ }
