// transport.C

#include "transport.h"

template<>
Librarian<Transport>::Content* Librarian<Transport>::content = NULL;

const char *const Transport::description = "\
This component handles the transportation of solute in the soil with\n\
the water.";

Transport::Transport (const string& n)
  : name (n)
{ }

Transport::~Transport ()
{ }

