 // crop.C

#include "crop.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"
#include <map>

const double Crop::DSremove = -5001.0;

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
Crop::add_type (const string& name, 
		AttributeList& alist, 
		const Syntax& syntax,
		constructor cons)
{
  assert (Crop_library);
  Crop_library->add (name, alist, syntax);
  Crop_constructors->insert(Crop_map_type::value_type (name, cons));
}

void 
Crop::derive_type (const string& name, AttributeList& al, const string& super)
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

void
Crop::kill (const string name, const Time& time, const Geometry& geometry,
	    OrganicMatter& organic_matter)
{
  harvest (name, time, geometry, organic_matter, 0.0, 0.0, 0.0, 0.0, true);
}

Crop::Crop (const string n)
  : name (n)
{ }

Crop::~Crop ()
{ }

CropList::CropList (const vector<AttributeList*>& sequence)
{
  for (vector<AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    push_back (Crop::create (**i, -1));
}

CropList::~CropList ()
{
  // Borland C++ don't want a const iterator here.
  for (iterator i = begin (); i != end (); i++)
    delete *i;
}

int Crop_init::count;

Crop_init::Crop_init ()
{ 
  if (count++ == 0)
    {
      Crop_library = new Library ("crop");
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

bool
Crop::ds_remove (const Crop* crop)
{ return crop->DS () == Crop::DSremove; }

