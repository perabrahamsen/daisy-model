// soil_heat.C

#include "soil_heat.h"
#include "alist.h"
#include "bioclimate.h"
#include "syntax.h"

void 
SoilHeat::tick (const Surface&, const Bioclimate& bioclimate)
{
  fill (T.begin (), T.end (), bioclimate.AirTemperature ());
}

bool
SoilHeat::check (unsigned n) const
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
SoilHeat::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("T", Syntax::Number, Syntax::State, Syntax::Sequence);
}

SoilHeat::SoilHeat (const AttributeList& al)
  : T (al.number_sequence ("T"))
{ }
