// nitrification_solute.C

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

class NitrificationSolute : public Nitrification
{
  // Parameters.
private: 
  const double k;
  const double k_10;

  // Log variable.
private:
  vector<double> converted;
  
  // Simulation.
public:
  void output (Log&, const Filter&) const;
  void tick (Soil&, SoilWater&, SoilHeat&, SoilNO3&, SoilNH4&);

  // Create.
public:
  friend class NitrificationSoluteSyntax;
  static Nitrification& make (const AttributeList&);
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
  assert (0);
}

void
NitrificationSolute::output (Log& log, const Filter& filter) const
{
  log.output ("converted", filter, converted, true);
}

void 
NitrificationSolute::tick (Soil& soil, SoilWater& soil_water,
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
      const double M_new = min (rate, soil_NH4.M_left (i) / dt);
      converted.push_back (M_new);
    }
  soil_NH4.add_to_sink (converted);
  soil_NO3.add_to_source (converted);
}

NitrificationSolute::NitrificationSolute (const AttributeList& al)
  : Nitrification (al.name ("type")),
    k (al.number ("k")),
    k_10 (al.number ("k_10"))
{ }

Nitrification&
NitrificationSolute::make (const AttributeList& al)
{
  return *new NitrificationSolute (al);
}

static struct NitrificationSoluteSyntax
{
  NitrificationSoluteSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("converted", Syntax::Number, Syntax::LogOnly, 
		Syntax::Sequence);
    syntax.add ("k", Syntax::Number, Syntax::Const);
    syntax.add ("k_10", Syntax::Number, Syntax::Const);
    Librarian<Nitrification>::add_type ("solute", alist, syntax,
					&NitrificationSolute::make);
  }
} NitrificationSolute_syntax;
