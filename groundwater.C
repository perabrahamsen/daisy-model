// groundwater.C

#include "groundwater.h"

Librarian<Groundwater>::Content* Librarian<Groundwater>::content = NULL;

void
Groundwater::output (Log&, Filter&) const
{ }

Groundwater::Groundwater (const string n)
  : name (n)
{ }

Groundwater::~Groundwater ()
{ }

