// parser.h

#ifndef PARSER_H
#define PARSER_H

#include <std/string.h>

class AttributeList;
class Library;
class Syntax;

class Parser
{
public:
  virtual void load (AttributeList&) = 0;

  // Library.
public:
  static const Library& library ();
  static Parser& create (const Syntax& syntax, const AttributeList&);
  typedef Parser& (*constructor) (const Syntax& syntax, const AttributeList&);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);
 
  // Create and Destroy.
protected:
  Parser ();
public:
  virtual ~Parser ();
};

// Ensure the Parser library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Parser_init
{
  static int count;
public:
  Parser_init ();
  ~Parser_init ();
} Parser_init;

#endif PARSER_H
