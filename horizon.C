// horizon.C

#include "horizon.h"
#include "syntax.h"
#include "alist.h"
#include <vector.h>

#define exception BUG_exception
#include <math.h>
#undef exception

struct Horizon::Implementation
{
  typedef list<const Horizon*> hList;
  const string name;
  const bool compact;
  static hList all;
  Implementation (string, const AttributeList&);
};

Horizon::Implementation::hList Horizon::Implementation::all;

Horizon::Implementation::Implementation (string n, const AttributeList& al)
  : name (n),
    compact (al.flag ("compact"))
{ }

double 
Horizon::Theta (double h) const
{
  if (h < -1.0)
    return 0.124 + 274.2 / (739.0 + pow (log (-h), 4));
  else
    return 0.495;
}

double 
Horizon::K (double h) const
{
  if (h < -1.0)
    return 3600 * 1.53e-3 / (124.6 + pow (-h, 1.77));
  else
    return 3600 * 1.23e-5;
}

double
Horizon::Cw1 (double h) const
{
  return Theta (h) - Cw2 (h) * h;
}

double 
Horizon::Cw2 (double h) const
{
  if (h < -1.0)
    return - (  (-4 * 274.2 * pow (log (-h), 3))
	      / (-h * pow (739.0 + pow (log (-h), 4), 2)));
  else
    return 0.0;
}

double 
Horizon::h (double /* Theta */) const
{
  THROW (Unimplemented ("Calculate h from Theta"));
  return -1;
}

bool 
Horizon::compact () const
{
  return impl.compact;
}

const Horizon&
Horizon::get (string name, const AttributeList& al)
{ 
    for (Implementation::hList::iterator i = Implementation::all.begin ();
	 i != Implementation::all.end (); 
	 i++)
	{
	    if ((*i)->impl.name == name)
		return *(*i);
	}
    const Horizon* h = new Horizon (name, al);
    Implementation::all.push_back (h);
    return *h;
}

Horizon::Horizon (string name, const AttributeList& al)
  : impl (*new Implementation (name, al))
{ }

Horizon::~Horizon ()
{ }

// Add the Horizon syntax to the syntax table.
static struct HorizonSyntax
{
  HorizonSyntax ();
} horizon_syntax;

HorizonSyntax::HorizonSyntax ()
{ 
  Syntax* syntax = new Syntax ();
  syntax->add ("compact", Syntax::Boolean);
  syntax_table->add ("horizon", syntax);
}

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
