// soil_NO3.C

#include "soil_NO3.h"
#include "soil_water.h"

double 
SoilNO3::beta (const Soil&, const SoilWater&, int, double) const
{
  return 0.0;
}

double 
SoilNO3::diffusion_coefficient () const
{
  return 3600 * 2.0e-5; // [cm^2/h]
}

double 
SoilNO3::C_to_M (const Soil&, const SoilWater& soil_water,
		 int i, double C) const
{
  return C * soil_water.Theta (i);
}

double 
SoilNO3::M_to_C (const Soil&, const SoilWater& soil_water,
		 int i, double M) const
{
  return M / soil_water.Theta (i);
}

void
SoilNO3::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Solute::load_syntax (syntax, alist);
}

SoilNO3::SoilNO3 (const Soil& soil, const SoilWater& soil_water,
		  const AttributeList& al)
{ 
  Solute::initialize (soil, soil_water, al);
}
