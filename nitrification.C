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
#include "log.h"
#include "filter.h"

static double f_h (double h)
{
  const double pF = log10 (-h);

  if (pF <= 0.0)
    return 0.0;
  if (pF <= 1.5)
    return pF / 1.5;
  if (pF <= 2.5)
    return 1.0;
  if (pF <= 5.0)
    return 1.0 - (pF - 2.5) / (5.0 - 2.5);
  
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

void
Nitrification::output (Log& log, const Filter& filter) const
{
  log.output ("converted", filter, converted, true);
}

void Nitrification::tick (Soil& soil, SoilWater& soil_water,
			  SoilHeat& soil_heat,
			  SoilNO3& soil_NO3, SoilNH4& soil_NH4)
{
  converted.erase (converted.begin (), converted.end ());

  for (int i = 0; i < soil.size (); i++)
    {
      const double C = soil_NH4.C (i);
      const double h = soil_water.h (i);
      const double T = soil_heat.T (i);
      const double rate = k_10 * f_h (h) * f_T (T) * C / (k + C);
      assert (rate >= 0.0);
      const double M = min (soil_NH4.M (i) * rate, soil_NH4.M_left (i) / dt);
      converted.push_back (M);
    }
  soil_NH4.add_to_sink (converted);
  soil_NO3.add_to_source (converted);
}

void
Nitrification::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("converted", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("k", Syntax::Number, Syntax::Const);
  syntax.add ("k_10", Syntax::Number, Syntax::Const);
}

Nitrification::Nitrification (const AttributeList& al)
  : k (al.number ("k")),
    k_10 (al.number ("k_10"))
{ }
