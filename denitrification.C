// denitrification.C

#include "denitrification.h"
#include "alist.h"
#include "syntax.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "organic_matter.h"
#include "soil_NO3.h"
#include "groundwater.h"
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

void Denitrification::tick (const Soil& soil, const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    SoilNO3& soil_NO3, 
			    const OrganicMatter& organic_matter,
			    const Groundwater& groundwater)
{
  converted.erase (converted.begin (), converted.end ());

  unsigned int size = soil.size ();
  if (!active_underground)
    size = min (size, soil.interval_plus (soil.MaxRootingDepth ()));
  if (!active_groundwater && !groundwater.flux_bottom ())
    size = min (size, soil.interval_plus (groundwater.table ()));

  for (unsigned int i = 0; i < size; i++)
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
Denitrification::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("active_underground", Syntax::Boolean, Syntax::Const);
  alist.add ("active_underground", false);
  syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const);
  alist.add ("active_groundwater", true);
  syntax.add ("converted", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("K", Syntax::Number, Syntax::Const);
  syntax.add ("alpha", Syntax::Number, Syntax::Const);
}

Denitrification::Denitrification (const AttributeList& al)
  : active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    K (al.number ("K")),
    alpha (al.number ("alpha"))
{ }
