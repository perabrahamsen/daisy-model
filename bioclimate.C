// bioclimate.C

#include "bioclimate.h"

Librarian<Bioclimate>::Content* Librarian<Bioclimate>::content = NULL;

Bioclimate::Bioclimate (const string& n)
  : name (n)
{ }

Bioclimate::~Bioclimate ()
{ }
