// soil_NH4.C

#include "soil_NH4.h"
#include "submodel.h"

double 
SoilNH4::diffusion_coefficient () const
{
  return 3600 * 1.8e-5; // [cm²/h]
}

void
SoilNH4::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "SoilNH4");
  alist.add ("description", "Ammonium in soil.");
  Solute::load_syntax (syntax, alist); 
  // Use linear adsorption by default.
  AttributeList& linear = *new AttributeList ();
  linear.add ("type", "linear");
  linear.add ("K_clay", 117.116);
  alist.add ("adsorption", linear);
}

SoilNH4::SoilNH4 (const AttributeList& al)
  : Solute (al)
{ }

static Submodel::Register 
soil_NH4_submodel ("SoilNH4", SoilNH4::load_syntax);

