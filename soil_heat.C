// soil_heat.C

#include "soil_heat.h"
#include "alist.h"
#include "bioclimate.h"
#include "syntax.h"

struct SoilHeat::Implementation
{
  vector<double> T;
  void tick (const Surface&, const Bioclimate& bioclimate)
  { fill (T.begin (), T.end (), bioclimate.AirTemperature ()); }
  bool check (unsigned n) const;
  Implementation (const AttributeList& al)
    : T (al.number_sequence ("T"))
  { }
};

bool
SoilHeat::Implementation::check (unsigned n) const
{
  bool ok = true;
  if (T.size () != n)
    {
      cerr << "You have " << n << " intervals but " 
	   << T.size () << " T values\n";
      ok = false;
    }
  return ok;
}

void 
SoilHeat::tick (const Surface& surface, const Bioclimate& bioclimate)
{
  impl.tick (surface, bioclimate);
}

double
SoilHeat::temperature (int i) const
{
  return impl.T[i]; 
}

bool
SoilHeat::check (unsigned n) const
{
  return impl.check (n);
}

void
SoilHeat::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("T", Syntax::Number, Syntax::State, Syntax::Sequence);
}

SoilHeat::SoilHeat (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

SoilHeat::~SoilHeat ()
{
  delete &impl;
}
