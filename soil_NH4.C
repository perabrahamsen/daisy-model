// soil_NH4.C

#include "soil_NH4.h"
#include "soil_water.h"

double 
SoilNH4::beta (const Soil&, const SoilWater&, int, double) const
{
  // BUG! From NO3.
  return 0.0;
}

double 
SoilNH4::diffusion_coefficient () const
{
  // BUG! From NO3.
  return 3600 * 2.0e-5; // [cm^2/h]
}

double 
SoilNH4::C_to_M (const Soil&, const SoilWater& soil_water,
		 int i, double C) const
{
  // BUG! From NO3.
  return C * soil_water.Theta (i);
}

double 
SoilNH4::M_to_C (const Soil&, const SoilWater& soil_water,
		 int i, double M) const
{
  // BUG! From NO3.
  return M / soil_water.Theta (i);
}

void
SoilNH4::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Solute::load_syntax (syntax, alist); 
}

SoilNH4::SoilNH4 (const Soil& soil, const SoilWater& soil_water,
		  const AttributeList& al)
{ 
  Solute::initialize (soil, soil_water, al);
}
