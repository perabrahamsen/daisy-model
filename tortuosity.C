// tortuosity.C

#include "tortuosity.h"

Librarian<Tortuosity>::Content* Librarian<Tortuosity>::content = NULL;

Tortuosity::Tortuosity (const string n)
  : name (n)
{ }

Tortuosity::~Tortuosity ()
{ }
