// parser.C

#include "parser.h"

Librarian<Parser>::Content* Librarian<Parser>::content = NULL;

const char *const Parser::description = "\
To start the simulation, many parameters must be specified and state\n\
variables must be given an initial value.  It is the responsibility of\n\
the `parser' component to read these data from an external source\n\
(typically a setup file), and convert them into the internal format\n\
used by Daisy.";

Parser::Parser (const string& n)
  : name (n)
{ }

Parser::~Parser ()
{ }
