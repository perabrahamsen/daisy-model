// horizon.C

#include "horizon.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"
#include <vector.h>
#include <map.h>

static Library* Horizon_library = NULL;
typedef map<string, Horizon::constructor, less<string> > Horizon_map_type;
static Horizon_map_type* Horizon_constructors;

struct Horizon::Implementation
{
  Implementation (const AttributeList&);
};

Horizon::Implementation::Implementation (const AttributeList&)
{ }

bool 
Horizon::compact () const
{
  return false;
}

const Library&
Horizon::library ()
{
  assert (Horizon_library);
  return *Horizon_library;
}

void
Horizon::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax* syntax,
		   constructor cons)
{
  assert (Horizon_library);
  Horizon_library->add (name, al, syntax);
  Horizon_constructors->insert(Horizon_map_type::value_type (name, cons));
}

void 
Horizon::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super),
	    (*Horizon_constructors)[super]);
}

Horizon&
Horizon::create (const string name)
{
  assert (library ().check (name));
  return *(*Horizon_constructors)[name] (library ().lookup (name));
}

Horizon::Horizon (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Horizon::~Horizon ()
{ }

struct HorizonList::Implementation
{
  vector<double> zplus;
  vector<const Horizon*> horizons;
  Implementation ();
};

HorizonList::Implementation::Implementation ()
{ }

const Horizon& 
HorizonList::horizon (double z) const
{
  unsigned i;
  for (i= 1; i < impl.zplus.size (); i++)
    if (z > impl.zplus[i])
      return *impl.horizons[i];
  return *impl.horizons[i - 1];
}

void 
HorizonList::add (double zplus, const Horizon& horizon)
{
  impl.zplus.push_back (zplus);
  impl.horizons.push_back (&horizon);
}

HorizonList::HorizonList ()
  : impl (*new Implementation)
{ }

HorizonList::~HorizonList ()
{ }

int Horizon_init::count;

Horizon_init::Horizon_init ()
{ 
  if (count++ == 0)
    {
      Horizon_library = new Library ();
      Horizon_constructors = new Horizon_map_type ();
    }
  assert (count > 0);
}

Horizon_init::~Horizon_init ()
{ 
  if (--count == 0)
    {
      delete Horizon_library;
      Horizon_library = NULL;
      delete Horizon_constructors;
      Horizon_constructors = NULL;
    }
  assert (count >= 0);
}
