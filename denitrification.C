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
#include "submodel.h"

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

  assert (false);
  return -42.42e42;
}

void
Denitrification::output (Log& log) const
{
  log.output ("converted", converted);
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
      const double rate = f_Theta (Theta / soil_water.Theta (soil, i, 0.0)) 
	* f_T (T) * alpha * CO2 ;
      const double M = min (rate, K * soil_NO3.M_left (i) / dt);
      converted.push_back (M);
    }
  soil_NO3.add_to_sink (converted);
}

void
Denitrification::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Denitrification");
  alist.add ("description", "Denitrification in soil (conversion\n\
of nitrate to atmospheric nitrogen).  In this model, it is made\n\
proportional to the CO2 development, as specified by the parameter\n\
alpha, with a maximum rate specified by the parameter `K'.  The\n\
denitrification is also affected by temperature and water pressure.");
  syntax.add ("active_underground", Syntax::Boolean, Syntax::Const, "\
Set this flag to turn on denitrification below the root zone.");
  alist.add ("active_underground", false);
  syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Clear this flag to turn off denitrification in groundwater.");
  alist.add ("active_groundwater", true);
  syntax.add ("converted", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
	      "Amount of denitrification.");
  syntax.add ("K", "h^-1", Syntax::Const, "\
Maximum fraction of nitrate converted at each time step.");
  alist.add ("K", 0.020833);
  syntax.add ("alpha", "(g NO3-N/h)/(g CO2-C/h)", Syntax::Const, 
	      "Anaerobic denitrification constant.");
  alist.add ("alpha", 0.1);
}

Denitrification::Denitrification (const AttributeList& al)
  : active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    K (al.number ("K")),
    alpha (al.number ("alpha"))
{ }

static Submodel::Register 
denitrification_submodel ("Denitrification", Denitrification::load_syntax);
