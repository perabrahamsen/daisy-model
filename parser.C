// parser.C

#include "parser.h"

Librarian<Parser>::Content* Librarian<Parser>::content = NULL;

Parser::Parser (const string& n)
  : name (n)
{ }

Parser::~Parser ()
{ }
