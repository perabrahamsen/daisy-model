// manager.C

#include "manager.h"
#include "library.h"
#include "alist.h"
#include <map.h>

static Library* Manager_library = NULL;
typedef map<string, Manager::constructor, less<string> > Manager_map_type;
static Manager_map_type* Manager_constructors;

const Library&
Manager::library ()
{
  assert (Manager_library);
  return *Manager_library;
}

void
Manager::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (Manager_library);
  Manager_library->add (name, al, syntax);
  Manager_constructors->insert(Manager_map_type::value_type (name, cons));
}

void 
Manager::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), 
	    (*Manager_constructors)[super]);
}

Manager&
Manager::create (const AttributeList& al)
{
  assert (al.check ("type"));
  return *(*Manager_constructors)[al.name ("type")] (al);
}

Manager::Manager ()
{ }

Manager::~Manager () 
{ }

int Manager_init::count;

Manager_init::Manager_init ()
{ 
  if (count++ == 0)
    {
      Manager_library = new Library ();
      Manager_constructors = new Manager_map_type ();
    }
  assert (count > 0);
}

Manager_init::~Manager_init ()
{ 
  if (--count == 0)
    {
      delete Manager_library;
      Manager_library = NULL;
      delete Manager_constructors;
      Manager_constructors = NULL;
    }
  assert (count >= 0);
}
