// condition.C -- Logic expressions

#include "condition.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"
#include <map>

static Library* Condition_library = NULL;
typedef map<string, Condition::constructor, less<string> > Condition_map_type;
static Condition_map_type* Condition_constructors;

const Library&
Condition::library ()
{
  assert (Condition_library);
  return *Condition_library;
}

void
Condition::add_type (const string name, 
		     const AttributeList& al, 
		     const Syntax& syntax,
		     constructor cons)
{
  assert (Condition_library);
  Condition_library->add (name, al, syntax);
  Condition_constructors->insert(Condition_map_type::value_type (name, cons));
}

void 
Condition::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), 
	    (*Condition_constructors)[super]);
}

Condition&
Condition::create (const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Condition_constructors)[name] (al);
}

Condition::Condition ()
{ }

Condition::~Condition ()
{ }

int Condition_init::count;

Condition_init::Condition_init ()
{ 
  if (count++ == 0)
    {
      Condition_library = new Library ("condition");
      Condition_constructors = new Condition_map_type ();
    }
  assert (count > 0);
}

Condition_init::~Condition_init ()
{ 
  if (--count == 0)
    {
      delete Condition_library;
      Condition_library = NULL;
      delete Condition_constructors;
      Condition_constructors = NULL;
    }
  assert (count >= 0);
}
