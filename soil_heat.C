// soil_heat.C

#include "soil_heat.h"
#include "alist.h"
#include "bioclimate.h"

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

SoilHeat::SoilHeat (const AttributeList& /* par */, 
		  const AttributeList& var)
  : T (var.array ("T"))
{ }
