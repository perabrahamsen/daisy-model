// soil_NO3.C

#include "soil_NO3.h"
#include "submodel.h"

double 
SoilNO3::diffusion_coefficient () const
{
  return 3600 * 2.0e-5; // [cm²/h]
}

void
SoilNO3::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilNO3");
  alist.add ("description", "Nitrate in soil.");
  Solute::load_syntax (syntax, alist);
  // Use "none" adsorption by default.
  AttributeList& none = *new AttributeList ();
  none.add ("type", "none");
  alist.add ("adsorption", none);
}

SoilNO3::SoilNO3 (const AttributeList& al)
  : Solute (al)
{ }

static Submodel::Register 
soil_NO3_submodel ("SoilNO3", SoilNO3::load_syntax);
