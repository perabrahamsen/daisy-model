// horizon.C

#include "horizon.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "csmp.h"
#include <vector.h>
#include <map.h>

static Library* Horizon_library = NULL;
typedef map<string, Horizon::constructor, less<string> > Horizon_map_type;
static Horizon_map_type* Horizon_constructors;

bool 
Horizon::compact () const
{
  return false;
}

void
Horizon::K_to_M (CSMP& csmp, const int intervals) const
{
  static const double h0 = -20000;
  const double Ksat = K (0);
  const double max_change = pow (Ksat / K (h0), 1 / intervals);
  double step = (0 - h0) / 4;

  double h = h0;
  while (h < 0)
    {
      csmp.add (h, K (h0));
      step *= 2;
      while (K (h) / K (h + step) > max_change)
	step /= 2;
      h += step;
    }
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
		   const Syntax& syntax,
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
Horizon::create (const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Horizon_constructors)[name] (library ().lookup (name));
}

Horizon::Horizon ()
{ }

Horizon::~Horizon ()
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
