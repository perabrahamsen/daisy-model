// crop.C

#include "crop.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

static Library* Crop_library = NULL;
typedef map<string, Crop::constructor, less<string> > Crop_map_type;
static Crop_map_type* Crop_constructors;

const Library&
Crop::library ()
{
  assert (Crop_library);
  return *Crop_library;
}

void
Crop::add_type (const string name, 
		const AttributeList& alist, 
		const Syntax& syntax,
		constructor cons)
{
  assert (Crop_library);
  Crop_library->add (name, alist, syntax);
  Crop_constructors->insert(Crop_map_type::value_type (name, cons));
}

void 
Crop::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, 
	    al, library ().syntax (super),
	    (*Crop_constructors)[super]);
}

Crop*
Crop::create (const AttributeList& al, int layers)
{
  assert (al.check ("type"));
  string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Crop_constructors)[name] (al, layers);
}

Crop::Crop (const string n)
  : name (n)
{ }

Crop::~Crop ()
{ }

CropList::CropList (const vector<const AttributeList*>& sequence)
{
  for (vector<const AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    push_back (Crop::create (**i, -1));
}

CropList::~CropList ()
{
  for (const_iterator i = begin (); i != end (); i++)
    delete *i;
}

int Crop_init::count;

Crop_init::Crop_init ()
{ 
  if (count++ == 0)
    {
      Crop_library = new Library ();
      Crop_constructors = new Crop_map_type ();
    }
  assert (count > 0);
}

Crop_init::~Crop_init ()
{ 
  if (--count == 0)
    {
      delete Crop_library;
      Crop_library = NULL;
      delete Crop_constructors;
      Crop_constructors = NULL;
    }
  assert (count >= 0);
}
