// tortuosity.C

#include "tortuosity.h"

Librarian<Tortuosity>::Content* Librarian<Tortuosity>::content = NULL;

const char *const Tortuosity::description = "\
Solutes in the soil can't move the shortest way between two points.\n\
The tortuosity factor indicates how far the average solute have\n\
moved in absolute coordinates, when it has moved a given distance\n\
along the curved line.  This component is responsible for calculating\n\
the soils tortuosity factor.";

Tortuosity::Tortuosity (const string& n)
  : name (n)
{ }

Tortuosity::~Tortuosity ()
{ }
