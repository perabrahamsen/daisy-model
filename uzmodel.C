// uzmodel.C

#include "uzmodel.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include <map.h>

UZtop::~UZtop ()
{ }

UZbottom::~UZbottom ()
{ }

static Library* UZ_library = NULL;
static bool UZ_library_used = false;
typedef map<string, UZmodel::constructor, less<string> > UZmap_type;
static UZmap_type* UZ_constructors;

const Library&
UZmodel::library ()
{
  assert (UZ_library);
  UZ_library_used = true;
  return *UZ_library;
}

void
UZmodel::add_model (const string name, 
		    const AttributeList& al, 
		    const Syntax* syntax,
		    constructor cons)
{
  assert (UZ_library);
  assert (!UZ_library_used);
  UZ_library->add (name, al, syntax);
  UZ_constructors->insert(UZmap_type::value_type (name, cons));
}

UZmodel* 
UZmodel::create (const AttributeList& al)
{
  assert (al.check ("type"));
  return (*UZ_constructors)[al.name ("type")] (al);
}

UZmodel::~UZmodel ()
{ }

int UZ_init::count;

UZ_init::UZ_init ()
{ 
  if (count++ == 0)
    {
      UZ_library = new Library ();
      UZ_constructors = new UZmap_type ();
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
