// transport.C

#include "transport.h"

Librarian<Transport>::Content* Librarian<Transport>::content = NULL;

Transport::Transport (const string& n)
  : name (n)
{ }

Transport::~Transport ()
{ }

