// adsorbtion.C

#include "adsorbtion.h"

Librarian<Adsorbtion>::Content* Librarian<Adsorbtion>::content = NULL;

Adsorbtion::Adsorbtion (const string& n)
  : name (n)
{ }

Adsorbtion::~Adsorbtion ()
{ }
