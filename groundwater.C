// groundwater.C

#include "groundwater.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

static Library* Groundwater_library = NULL;
typedef map<string, Groundwater::constructor, less<string> > Groundwater_map_type;
static Groundwater_map_type* Groundwater_constructors;

const Library&
Groundwater::library ()
{
  assert (Groundwater_library);
  return *Groundwater_library;
}

void
Groundwater::add_type (const string name, 
		       const AttributeList& al, 
		       const Syntax& syntax,
		       constructor cons)
{
  assert (Groundwater_library);
  Groundwater_library->add (name, al, syntax);
  Groundwater_constructors->insert(Groundwater_map_type::value_type (name, cons));
}

void 
Groundwater::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), 
	    (*Groundwater_constructors)[super]);
}

Groundwater&
Groundwater::create (const Time& t, const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Groundwater_constructors)[name] (t, al);
}

Groundwater::Groundwater (const Time& t)
  : time (t)
{ }

Groundwater::~Groundwater ()
{ }

int Groundwater_init::count;

Groundwater_init::Groundwater_init ()
{ 
  if (count++ == 0)
    {
      Groundwater_library = new Library ();
      Groundwater_constructors = new Groundwater_map_type ();
    }
  assert (count > 0);
}

Groundwater_init::~Groundwater_init ()
{ 
  if (--count == 0)
    {
      delete Groundwater_library;
      Groundwater_library = NULL;
      delete Groundwater_constructors;
      Groundwater_constructors = NULL;
    }
  assert (count >= 0);
}
