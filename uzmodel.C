// uzmodel.C

#include "uzmodel.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map.h>


UZtop::~UZtop ()
{ }

UZbottom::~UZbottom ()
{ }

void
UZmodel::output (const string, Log&, const Filter*) const
{ }

static Library* UZ_library = NULL;
typedef map<string, UZmodel::constructor, less<string> > UZ_map_type;
static UZ_map_type* UZ_constructors;

const Library&
UZmodel::library ()
{
  assert (UZ_library);
  return *UZ_library;
}

void
UZmodel::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (UZ_library);
  UZ_library->add (name, al, syntax);
  UZ_constructors->insert(UZ_map_type::value_type (name, cons));
}

UZmodel* 
UZmodel::create (const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*UZ_constructors)[name] (al);
}

UZmodel::~UZmodel ()
{ }

int UZ_init::count;

UZ_init::UZ_init ()
{ 
  if (count++ == 0)
    {
      UZ_library = new Library ();
      UZ_constructors = new UZ_map_type ();
    }
  assert (count > 0);
}

UZ_init::~UZ_init ()
{ 
  if (--count == 0)
    {
      delete UZ_library;
      UZ_library = NULL;
      delete UZ_constructors;
      UZ_constructors = NULL;
    }
  assert (count >= 0);
}
