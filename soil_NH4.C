// soil_NH4.C

#include "soil_NH4.h"
#include "soil_water.h"
#include "soil.h"
#include "math.h"
#include "mathlib.h"

double 
SoilNH4::diffusion_coefficient () const
{
  return 3600 * 1.8e-5; // [cm²/h]
}

void
SoilNH4::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Solute::load_syntax (syntax, alist); 
  // Use van Schouwenburg - Schuffelen adsorbtion by default.
  AttributeList& vS_S = *new AttributeList ();
  vS_S.add ("type", "vS_S");
  alist.add ("adsorbtion", vS_S);
}

SoilNH4::SoilNH4 (const AttributeList& al)
  : Solute (al)
{ }
