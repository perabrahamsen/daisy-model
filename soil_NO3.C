// soil_NO3.C

#include "soil_NO3.h"
#include "soil_water.h"
#include "soil.h"

#ifdef MIKE_SHE
#include "mike_she.h"
 
void
SoilNO3::clear (const Soil& soil, const SoilWater& soil_water)
{
  mike_she->get_no3_m (M_);
  for (int i = 0; i < soil.size (); i++)
    C_[i] = M_[i] / soil_water.Theta (i);
  Solute::clear ();
}
#else
void
SoilNO3::clear (const Soil& /*soil */, const SoilWater& /* soil_water */)
{
  Solute::clear ();
}
#endif

void 
SoilNO3::tick (const Soil& soil, const SoilWater& soil_water, double J_in)
{
  Solute::tick (soil, soil_water, J_in);
#ifdef MIKE_SHE
  mike_she->put_no3_m (M_);
#endif
}

double 
SoilNO3::diffusion_coefficient () const
{
  return 3600 * 2.0e-5; // [cm²/h]
}

double 
SoilNO3::C_to_M (const Soil&, double Theta, int, double C) const
{
  return C * Theta;
}

double 
SoilNO3::M_to_C (const Soil&, double Theta, int, double M) const
{
  return M / Theta;
}

void
SoilNO3::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Solute::load_syntax (syntax, alist);
}

SoilNO3::SoilNO3 (const Soil& soil, const SoilWater& soil_water,
		  const AttributeList& al)
  : Solute (al)
{ 
  Solute::initialize (soil, soil_water, al);
}
