// nitrification_solute.C

#include "nitrification.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "log.h"
#include "mathlib.h"
#include "csmp.h"

class NitrificationSolute : public Nitrification
{
  // Parameters.
private: 
  const bool active_underground; // True, iff turnover happens below rootzone.
  const bool active_groundwater; // True, iff turnover happens in groundwater.
  const double k;
  const double k_10;
  const CSMP heat_factor;
  const CSMP water_factor;

  // Log variable.
private:
  vector<double> converted;
  
  // Simulation.
public:
  void output (Log&) const;
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&);

  // Create.
public:
  NitrificationSolute (const AttributeList&);
};

static double f_h (double h)
{
  if (h >= 0.0)
    return 0.6;

  const double pF = h2pF (h);

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
#if 1
  if (T < 2.0)
    return 0.0;
  if (T < 6.0)
    return 0.15 * (T - 2.0);
  if (T < 20.0)
    return 0.10 * T;
  if (T < 40)
    return exp (0.47 - 0.027 * T + 0.00193 * T * T);
#else
  if (T < 1.0)
    return 0.0;
  if (T < 5.0)
    return 0.125 * (T - 1.0);
  if (T < 20.0)
    return 0.10 * T;
  if (T < 40)
    return exp (0.47 - 0.027 * T + 0.00193 * T * T);
#endif
  assert (false);
  return -42.42e42;
}

void
NitrificationSolute::output (Log& log) const
{
  log.output ("converted", converted);
}

void 
NitrificationSolute::tick (const Soil& soil, const SoilWater& soil_water,
			   const SoilHeat& soil_heat,
			   SoilNO3& soil_NO3, SoilNH4& soil_NH4)
{
  converted.erase (converted.begin (), converted.end ());

  unsigned int size = soil.size ();
  if (!active_underground)
    size = min (size, soil.interval_plus (soil.MaxRootingDepth ()));
  if (!active_groundwater)
    size = soil_water.first_groundwater_node ();

  for (unsigned int i = 0; i < size; i++)
    {
      const double C = soil_NH4.C (i);
      const double h = soil_water.h (i);
      const double T = soil_heat.T (i);
      const double T_factor = (heat_factor.size () < 1)
	? f_T (T)
	: heat_factor (T);
      const double w_factor = (water_factor.size () < 1)
	? f_h (h)
	: water_factor (h);
      const double rate = k_10 * w_factor * T_factor * C / (k + C);
      assert (rate >= 0.0);
      assert (soil_NH4.M_left (i) >= 0.0);
      const double M_new = min (rate, soil_NH4.M_left (i) / dt - 1e-8);
      if (M_new >= 0.0)
	converted.push_back (M_new);
      else
	converted.push_back (0.0);
    }
  soil_NH4.add_to_sink (converted);
  soil_NO3.add_to_source (converted);
}

NitrificationSolute::NitrificationSolute (const AttributeList& al)
  : Nitrification (al.name ("type")),
    active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    k (al.number ("k")),
    k_10 (al.number ("k_10")),
    heat_factor (al.csmp ("heat_factor")),
    water_factor (al.csmp ("water_factor"))
{ }

static struct NitrificationSoluteSyntax
{
  static Nitrification& make (const AttributeList& al)
  {
    return *new NitrificationSolute (al);
  }

  NitrificationSoluteSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "k_10 * C / (k + C).  Michaelis-Menten kinetics,\n\
with nitrification based on ammonium solute.");
    syntax.add ("active_underground", Syntax::Boolean, Syntax::Const, "\
Set this to true to enable nitrification below the root zone.");
    alist.add ("active_underground", false);
    syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Set this to true to enable nitrification in the groundwater.");
    alist.add ("active_groundwater", false);
    syntax.add ("converted", 
		"g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
		"Amount of ammonium converted this hour.");
    syntax.add ("k", "g/cm^3", Syntax::Const, 
		"Half saturation constant.");
    syntax.add ("k_10", "h^-1", Syntax::Const,
		"Max rate.");
    CSMP empty;
    syntax.add ("heat_factor", Syntax::CSMP, Syntax::Const,
		"Heat factor [dg C ->].");
    alist.add ("heat_factor", empty);
    syntax.add ("water_factor", Syntax::CSMP, Syntax::Const,
		"Water potential factor [cm ->].");
    alist.add ("water_factor", empty);
    Librarian<Nitrification>::add_type ("solute", alist, syntax, &make);
  }
} NitrificationSolute_syntax;
