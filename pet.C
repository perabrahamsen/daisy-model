// pet.C  -- Potential evopotranspiration

#include "pet.h"
#include "log.h"
#include "crop.h"
#include "surface.h"

Librarian<Pet>::Content* Librarian<Pet>::content = NULL;

double
Pet::reference_to_potential (const CropList& crops, 
			     const Surface& surface,
			     double ref)
{
  const double LAI = crops.LAI ();
  double EpFactor = crops.CanopySum (&Crop::EpFac);
  if (LAI > 1.0)
    EpFactor /= LAI;
  else
    EpFactor += (1.0 - LAI) * surface.EpFactor ();

  return EpFactor * max (0.0, ref);
}

double
Pet::dry () const 
{ return wet (); }

void
Pet::output (Log& log, Filter& filter) const
{
  log.output ("wet", filter, wet (), true);
  log.output ("dry", filter, dry (), true);
}

void 
Pet::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("wet", Syntax::Number, Syntax::LogOnly);
  syntax.add ("dry", Syntax::Number, Syntax::LogOnly);
}

Pet::Pet (const AttributeList& al)
  : name (al.name ("type"))
{ }

Pet::~Pet ()
{ }

