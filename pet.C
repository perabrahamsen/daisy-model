// pet.C  -- Potential evopotranspiration

#include "pet.h"
#include "log.h"
#include "vegetation.h"
#include "surface.h"

Librarian<Pet>::Content* Librarian<Pet>::content = NULL;

const char *const Pet::description = "\
The 'pet' component should calculate the potential evapotranspiration\n\
from meteorological data, as well as the crop and soil state.";

double
Pet::reference_to_potential (const Vegetation& crops, 
			     const Surface& surface,
			     double ref)
{
  const double LAI = crops.LAI ();
  double EpFactor;
  if (LAI < 0.01)
    EpFactor = surface.EpFactor ();
  else if (LAI > 0.99)
    EpFactor = crops.EpFactor ();
  else
    EpFactor = LAI * crops.EpFactor () + (1.0 - LAI) * surface.EpFactor ();

  return EpFactor * max (0.0, ref);
}

double
Pet::dry () const 
{ return wet (); }

void
Pet::output (Log& log) const
{
  log.output ("wet", wet ());
  log.output ("dry", dry ());
}

void 
Pet::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("wet", "mm/h", Syntax::LogOnly, 
	      "Potential evapotranspiration for a wet system.");
  syntax.add ("dry", "mm/h", Syntax::LogOnly, 
	      "Potential evapotranspiration for a dry system.");
  syntax.add ("reference_evapotranspiration", "mm/h", Syntax::LogOnly, 
	      "Reference evapotranspiration for a dry system.");
}

Pet::Pet (const AttributeList& al)
  : name (al.name ("type"))
{ }

Pet::~Pet ()
{ }

