// soil_NH4.C

#include "soil_NH4.h"
#include "soil_water.h"
#include "soil.h"
#include "math.h"
#include "mathlib.h"

double 
SoilNH4::beta (const Soil& soil, const SoilWater&, int i, double C) const
{
  return soil.v_planar (i) * soil.K_planar (i) / pow (soil.K_planar (i) + C, 2)
       + soil.v_edge (i)   * soil.K_edge (i)   / pow (soil.K_edge (i) + C, 2);
}

double 
SoilNH4::diffusion_coefficient () const
{
  return 3600 * 1.8e-5; // [cm^2/h]
}

double
SoilNH4::C_to_A (const Soil& soil, int i, double C) const
{
  return (soil.v_planar (i) * C) / (soil.K_planar (i) + C)
    + (soil.v_edge (i) * C) / (soil.K_edge (i) + C);
}

double 
SoilNH4::C_to_M (const Soil& soil, double Theta, int i, double C) const
{
  return C_to_A (soil, i, C) + Theta * C;
}

double 
SoilNH4::M_to_C (const Soil& soil, double Theta, int i, double M) const
{
  const double ve = soil.v_edge (i); 
  const double Ke = soil.K_edge (i);
  const double vp = soil.v_planar (i);
  const double Kp = soil.K_planar (i);

  if (M < 1e-6 * min (Ke, Kp))
    // There are numerical problems in the general solution for small M. 
    return M / (Theta + ve  / Ke + vp / Kp);

  const double a = Theta;
  const double b = Theta * (Kp + Ke) + vp + ve - M;
  const double c = vp * Ke + ve * Kp - M * (Kp + Ke) + Kp * Ke * Theta;
  const double d = - M * Kp * Ke;
    
  return single_positive_root_of_cubic_equation (a, b, c, d);
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
