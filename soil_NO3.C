// soil_NO3.C

#include "soil_NO3.h"
#include "soil.h"
#include "submodel.h"

double 
SoilNO3::diffusion_coefficient () const
{
  return 3600 * 2.0e-5; // [cm²/h]
}

void 
SoilNO3::default_initialize (const Soil& soil, const SoilWater&)
{
  assert (C_.size () == 0);
  assert (M_.size () == 0);
  // We initialize to approximatey half the allowed content in
  // drinking water [ 0.5 * 100 mg NO3/l ~= 5.0e-6 g NO3-N/cm^3 ]
  C_.insert (C_.begin (), soil.size (), 5.0e-6); 
}

void
SoilNO3::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "SoilNO3");
  alist.add ("description", "Nitrate in soil.\n\
If unspecified, initial value will be 50 mg NO3/l.");
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
