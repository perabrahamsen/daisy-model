// hydraulic.C

#include "hydraulic.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "csmp.h"
#include <vector>
#include <map>

static Library* Hydraulic_library = NULL;
typedef map<string, Hydraulic::constructor, less<string> > Hydraulic_map_type;
static Hydraulic_map_type* Hydraulic_constructors;

bool 
Hydraulic::compact () const
{
  return false;
}

void
Hydraulic::K_to_M (CSMP& csmp, const int intervals) const
{
  static const double h0 = -20000.0;
  const double Ksat = K (0.0);
  const double max_change = pow (Ksat / K (h0), 1.0 / intervals);
  double step = (0 - h0) / 4.0;

  double h = h0;
  double sum = 0.0;
  while (h < 0)
    {
      csmp.add (h, sum);
      step *= 2;
      while (K (h + step) / K (h) > max_change)
	step /= 2;
      sum += step * (K (h) + K (h + step)) / 2;
      h += step;
    }
  csmp.add (h, sum);
}

const Library&
Hydraulic::library ()
{
  assert (Hydraulic_library);
  return *Hydraulic_library;
}

void
Hydraulic::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (Hydraulic_library);
  Hydraulic_library->add (name, al, syntax);
  Hydraulic_constructors->insert(Hydraulic_map_type::value_type (name, cons));
}

void 
Hydraulic::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super),
	    (*Hydraulic_constructors)[super]);
}

Hydraulic&
Hydraulic::create (const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Hydraulic_constructors)[name] (al);
}

void
Hydraulic::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("Theta_sat", Syntax::Number, Syntax::Const);
  syntax.add ("Theta_res", Syntax::Number, Syntax::Const);
  alist.add ("Theta_res", 0.0);
  syntax.add ("lambda", Syntax::Number, Syntax::Const);
}

Hydraulic::Hydraulic (const AttributeList& al)
  : Theta_sat (al.number ("Theta_sat")),
    Theta_res (al.number ("Theta_res")),
    lambda (al.number ("lambda"))
{ }

Hydraulic::~Hydraulic ()
{ }

int Hydraulic_init::count;

Hydraulic_init::Hydraulic_init ()
{ 
  if (count++ == 0)
    {
      Hydraulic_library = new Library ();
      Hydraulic_constructors = new Hydraulic_map_type ();
    }
  assert (count > 0);
}

Hydraulic_init::~Hydraulic_init ()
{ 
  if (--count == 0)
    {
      delete Hydraulic_library;
      Hydraulic_library = NULL;
      delete Hydraulic_constructors;
      Hydraulic_constructors = NULL;
    }
  assert (count >= 0);
}
