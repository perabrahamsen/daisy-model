// parser.C

#include "parser.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"
#include <map>

static Library* Parser_library = NULL;
typedef map<string, Parser::constructor, less<string> > Parser_map_type;
static Parser_map_type* Parser_constructors;

const Library&
Parser::library ()
{
  assert (Parser_library);
  return *Parser_library;
}

void
Parser::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (Parser_library);
  Parser_library->add (name, al, syntax);
  Parser_constructors->insert(Parser_map_type::value_type (name, cons));
}

void 
Parser::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), 
	    (*Parser_constructors)[super]);
}

Parser&
Parser::create (const Syntax& s, const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Parser_constructors)[name] (s, al);
}

Parser::Parser ()
{ }

Parser::~Parser ()
{ }

int Parser_init::count;

Parser_init::Parser_init ()
{ 
  if (count++ == 0)
    {
      Parser_library = new Library ("parser");
      Parser_constructors = new Parser_map_type ();
    }
  assert (count > 0);
}

Parser_init::~Parser_init ()
{ 
  if (--count == 0)
    {
      delete Parser_library;
      Parser_library = NULL;
      delete Parser_constructors;
      Parser_constructors = NULL;
    }
  assert (count >= 0);
}
