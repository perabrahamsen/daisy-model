// nitrification.C

#include "nitrification.h"
#include "alist.h"
#include "syntax.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "csmp.h"
#include "common.h"

static double f_h (double h)
{
  if (h > -10e-2)
    return 0.0;
  if (h > -pow (10.0, -0.05))
    return log10 (-100.0 * h);
  if (h > -pow (10.0, 0.5))
    return 1;
  if (h > -10e2)
    return 1.0 - log10 (-100.0 * h);
  
  return 0.0;
}
      
static double f_T (double T)
{
  if (T < 2.0)
    return 0.0;
  if (T < 6.0)
    return 0.15 * (T - 2.0);
  if (T < 20.0)
    return 0.10 * T;
  if (T < 40)
    return exp (0.47 - 0.027 * T + 0.00193 * T * T);

  assert (0);
}

void Nitrification::tick (Soil& soil, SoilWater& soil_water,
			  SoilHeat& soil_heat,
			  SoilNO3& soil_NO3, SoilNH4& soil_NH4)
{
  vector<double> converted;
  
  for (int i = 0; i < soil.size (); i++)
    {
      const double C = soil_NH4.C (i);
      const double h = soil_water.h (i);
      const double T = soil_heat.T (i);
      const double rate = k_10 * f_h (h) * f_T (T) * C / (k + C);
      const double M = min (soil_NH4.M (i) * rate, soil_NH4.M_left (i) / dt);
      converted.push_back (M);
    }
  soil_NH4.add_to_sink (converted);
  soil_NO3.add_to_source (converted);
}

void
Nitrification::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("k", Syntax::Number, Syntax::Const);
  syntax.add ("k_10", Syntax::Number, Syntax::Const);
}

Nitrification::Nitrification (const AttributeList& al)
  : k (al.number ("k")),
    k_10 (al.number ("k_10"))
{ }
