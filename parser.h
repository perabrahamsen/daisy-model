// parser.h

#ifndef PARSER_H
#define PARSER_H

#include "librarian.h"

class Treelog;

class Parser
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Interface.
public:
  virtual void load_nested (AttributeList&) = 0;
  virtual void load (AttributeList&) = 0;
  virtual int error_count () const = 0;
 
  // Create and Destroy.
public:
  virtual void initialize (const Syntax&, Treelog&) = 0;
protected:
  Parser (const string& name);
public:
  virtual ~Parser ();
};

static Librarian<Parser> Parser_init ("parser");

#endif // PARSER_H
