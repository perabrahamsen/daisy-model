// soil_NH4.C

#include "soil_NH4.h"
#include "soil.h"
#include "soil_water.h"
#include "submodel.h"

double 
SoilNH4::diffusion_coefficient () const
{
  return 3600 * 1.8e-5; // [cm²/h]
}

void 
SoilNH4::default_initialize (const Soil& soil, const SoilWater& soil_water)
{
  assert (C_.size () == 0);
  assert (M_.size () == 0);
  // We initialize to approximatey 5% of the N corresponding to the
  // allowed content of NO3 in drinking water.
  // [ 0.05 * 100 mg/l = 0.5e-6 g/cm^3 ]
  for (unsigned int i = 0; i < soil.size (); i++)
    M_.push_back (0.5e-6 * soil_water.Theta (i));
}

void
SoilNH4::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "SoilNH4");
  alist.add ("description", "Ammonium in soil.\n\
If unspecified, initial value will be 5 mg NH4/l (including adsorbed).");
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

