// soil_NO3.C

#include "soil_NO3.h"

double 
SoilNO3::diffusion_coefficient () const
{
  return 3600 * 2.0e-5; // [cm²/h]
}

void
SoilNO3::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Solute::load_syntax (syntax, alist);
  // Use "none" adsorption by default.
  AttributeList& none = *new AttributeList ();
  none.add ("type", "none");
  alist.add ("adsorption", none);
}

SoilNO3::SoilNO3 (const AttributeList& al)
  : Solute (al)
{ }
