// uzmodel.C

#include "uzmodel.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>


UZtop::~UZtop ()
{ }

UZbottom::~UZbottom ()
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

void 
UZmodel::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), (*UZ_constructors)[super]);
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

UZmodel::UZmodel (string n)
  : name (n)
{ }

UZmodel::~UZmodel ()
{ }

int UZ_init::count;

UZ_init::UZ_init ()
{ 
  if (count++ == 0)
    {
      UZ_library = new Library ("uzmodel");
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
