// denitrification.C

#include "denitrification.h"
#include "alist.h"
#include "syntax.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic_matter.h"
#include "soil_NO3.h"
#include "csmp.h"
#include "log.h"

static double f_Theta (double x)
{
  if (x < 0.8)
    return 0.0;
  if (x < 0.9)
    return 2.0 * (x - 0.8);
  
  return 0.2 + 8.0 * (x - 0.9);
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
Denitrification::output (Log& log, Filter& filter) const
{
  log.output ("converted", filter, converted, true);
}

void Denitrification::tick (Soil& soil, SoilWater& soil_water,
			  SoilHeat& soil_heat,
			  SoilNO3& soil_NO3, OrganicMatter& organic_matter)
{
  converted.erase (converted.begin (), converted.end ());

  for (int i = 0; i < soil.size (); i++)
    {
      const double CO2 = organic_matter.CO2 (i);
      const double Theta = soil_water.Theta (i);
      const double T = soil_heat.T (i);
      const double rate = f_Theta (Theta / soil.Theta (i, 0.0)) 
	* f_T (T) * alpha * CO2 ;
      const double M = min (rate, K * soil_NO3.M_left (i) / dt);
      converted.push_back (M);
    }
  soil_NO3.add_to_sink (converted);
}

void
Denitrification::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("converted", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("K", Syntax::Number, Syntax::Const);
  syntax.add ("alpha", Syntax::Number, Syntax::Const);
}

Denitrification::Denitrification (const AttributeList& al)
  : K (al.number ("K")),
    alpha (al.number ("alpha"))
{ }
