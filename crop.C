// crop.C

#include "crop.h"
#include "library.h"
#include <map.h>

static Library* Crop_par_library = NULL;
static Library* Crop_var_library = NULL;
typedef map<string, Crop::constructor, less<string> > Crop_map_type;
static Crop_map_type* Crop_constructors;

const Library&
Crop::par_library ()
{
  assert (Crop_par_library);
  return *Crop_par_library;
}

const Library&
Crop::var_library ()
{
  assert (Crop_var_library);
  return *Crop_var_library;
}

void
Crop::add_type (const string name, 
		const AttributeList& parList, 
		const Syntax& parSyntax,
		const AttributeList& varList, 
		const Syntax& varSyntax,
		constructor cons)
{
  assert (Crop_par_library);
  assert (Crop_var_library);
  Crop_par_library->add (name, parList, parSyntax);
  Crop_var_library->add (name, varList, varSyntax);
  Crop_constructors->insert(Crop_map_type::value_type (name, cons));
}

void 
Crop::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, 
	    al, par_library ().syntax (super),
	    var_library ().lookup (super), var_library ().syntax (super),
	    (*Crop_constructors)[super]);
}

Crop*
Crop::create (const string name, const AttributeList& var)
{
  assert (par_library ().check (name));
  return (*Crop_constructors)[name]
    (name, par_library ().lookup (name), var);
}

Crop::Crop (const string n)
  : name (n)
{ }

Crop::~Crop ()
{ }

int Crop_init::count;

Crop_init::Crop_init ()
{ 
  if (count++ == 0)
    {
      Crop_par_library = new Library ();
      Crop_var_library = new Library ();
      Crop_constructors = new Crop_map_type ();
    }
  assert (count > 0);
}

Crop_init::~Crop_init ()
{ 
  if (--count == 0)
    {
      delete Crop_par_library;
      Crop_par_library = NULL;
      delete Crop_var_library;
      Crop_var_library = NULL;
      delete Crop_constructors;
      Crop_constructors = NULL;
    }
  assert (count >= 0);
}
