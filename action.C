// action.C -- Manager actions

#include "action.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"
#include <map.h>

static Library* Action_library = NULL;
typedef map<string, Action::constructor, less<string> > Action_map_type;
static Action_map_type* Action_constructors;

const Library&
Action::library ()
{
  assert (Action_library);
  return *Action_library;
}

void
Action::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (Action_library);
  Action_library->add (name, al, syntax);
  Action_constructors->insert(Action_map_type::value_type (name, cons));
}

void 
Action::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), 
	    (*Action_constructors)[super]);
}

Action&
Action::create (const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Action_constructors)[name] (al);
}

void 
Action::doIt (Daisy&) const
{ }

Action Action::null;

Action::Action ()
{ }

Action::~Action ()
{ }

int Action_init::count;

Action_init::Action_init ()
{ 
  if (count++ == 0)
    {
      Action_library = new Library ();
      Action_constructors = new Action_map_type ();
    }
  assert (count > 0);
}

Action_init::~Action_init ()
{ 
  if (--count == 0)
    {
      delete Action_library;
      Action_library = NULL;
      delete Action_constructors;
      Action_constructors = NULL;
    }
  assert (count >= 0);
}
