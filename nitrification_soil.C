// nitrification_soil.C

#include "nitrification.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "csmp.h"
#include "mathlib.h"
#include "log.h"
#include "groundwater.h"

class NitrificationSoil : public Nitrification
{
  // Parameters.
private: 
  const bool active_underground; // True, iff turnover happens below rootzone.
  const bool active_groundwater; // True, iff turnover happens in groundwater.
  const double k;
  const double k_10;

  // Log variable.
private:
  vector<double> converted;
  
  // Simulation.
public:
  void output (Log&, Filter&) const;
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&, const Groundwater&);

  // Create.
public:
  NitrificationSoil (const AttributeList&);
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
NitrificationSoil::output (Log& log, Filter& filter) const
{
  log.output ("converted", filter, converted, true);
}

void 
NitrificationSoil::tick (const Soil& soil, const SoilWater& soil_water,
			 const SoilHeat& soil_heat,
			 SoilNO3& soil_NO3, SoilNH4& soil_NH4,
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
      const double M = soil_NH4.M (i);
      const double h = soil_water.h (i);
      const double T = soil_heat.T (i);
      const double rate = k_10 * f_h (h) * f_T (T) * M / (k + M);
      assert (rate >= 0.0);
      const double M_new = min (rate, soil_NH4.M_left (i) / dt);
      converted.push_back (M_new);
    }
  soil_NH4.add_to_sink (converted);
  soil_NO3.add_to_source (converted);
}

NitrificationSoil::NitrificationSoil (const AttributeList& al)
  : Nitrification (al.name ("type")),
    active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    k (al.number ("k")),
    k_10 (al.number ("k_10"))
{ }

static struct NitrificationSoilSyntax
{
  static Nitrification&
  make (const AttributeList& al)
    { return *new NitrificationSoil (al); }
  NitrificationSoilSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("active_underground", Syntax::Boolean, Syntax::Const);
      alist.add ("active_underground", false);
      syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const);
      alist.add ("active_groundwater", false);
      syntax.add ("converted", Syntax::Number, Syntax::LogOnly, 
		  Syntax::Sequence);
      syntax.add ("k", Syntax::Number, Syntax::Const);
      syntax.add ("k_10", Syntax::Number, Syntax::Const);
      Librarian<Nitrification>::add_type ("soil", alist, syntax, &make);
    }
} NitrificationSoil_syntax;
