// soil_heat.C

#include "soil_heat.h"
#include "alist.h"
#include "bioclimate.h"
#include "syntax.h"

void 
SoilHeat::tick (const Surface&, const Bioclimate& bioclimate)
{
  for (vector<double>::iterator i = T.begin (); i != T.end (); i++)
    *i = bioclimate.AirTemperature ();
}

bool
SoilHeat::check (Log& /* log */, unsigned n) const
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
  syntax.add ("T", Syntax::Array);
}

SoilHeat::SoilHeat (const AttributeList& al)
  : T (al.array ("T"))
{ }
