// parser.C

#include "parser.h"

Librarian<Parser>::Content* Librarian<Parser>::content = NULL;

int 
Parser::error_count () const
{ return 0; }

Parser::Parser (const string& n)
  : name (n)
{ }

Parser::~Parser ()
{ }
