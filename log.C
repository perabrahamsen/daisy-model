// log.C

#include "log.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"
#include <map>

Syntax* Log::global_syntax_table = NULL;

static Library* Log_library = NULL;
typedef map<string, Log::constructor, less<string> > Log_map_type;
static Log_map_type* Log_constructors;

const Library&
Log::library ()
{
  assert (Log_library);
  return *Log_library;
}

void
Log::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (Log_library);
  Log_library->add (name, al, syntax);
  Log_constructors->insert(Log_map_type::value_type (name, cons));
}

void 
Log::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), 
	    (*Log_constructors)[super]);
}

Log&
Log::create (const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Log_constructors)[name] (al);
}

Log::Log ()
{ }

Log::~Log ()
{ }

int Log_init::count;

Log_init::Log_init ()
{ 
  if (count++ == 0)
    {
      Log::global_syntax_table = new Syntax ();
      Log_library = new Library ();
      Log_constructors = new Log_map_type ();
    }
  assert (count > 0);
}

Log_init::~Log_init ()
{ 
  if (--count == 0)
    {
      delete Log::global_syntax_table;
      Log::global_syntax_table = NULL;
      delete Log_library;
      Log_library = NULL;
      delete Log_constructors;
      Log_constructors = NULL;
    }
  assert (count >= 0);
}
