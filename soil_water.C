// soil_water.C

#include "soil_water.h"

#if 0
  if (var.Theta.size () != zplus_.size ())
    {
      cerr << "You have " << zplus_.size () 
	   << " intervals but " << var.Theta.size () << " Theta values\n";
      ok = false;
    }
  if (var.h.size () != zplus_.size ())
    {
      cerr << "You have " << zplus_.size () 
	   << " intervals but " << var.h.size () << " h values\n";
      ok = false;
    }
  if (var.Xi.size () != zplus_.size ())
    {
      cerr << "You have " << zplus_.size () 
	   << " intervals but " << var.Xi.size () << " Xi values\n";
      ok = false;
    }
#endif

void
SoilWater::tick (Surface&, const Groundwater&)
{ }

SoilWater::SoilWater (const AttributeList& /* par */, 
		      const AttributeList& /* var */)
{ }
