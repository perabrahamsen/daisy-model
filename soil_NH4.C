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
  // Use linear adsorption by default.
  AttributeList& linear = *new AttributeList ();
  linear.add ("type", "linear");
  linear.add ("K_clay", 117.116);
  alist.add ("adsorption", linear);
}

SoilNH4::SoilNH4 (const AttributeList& al)
  : Solute (al)
{ }
