// nitrification_soil.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "nitrification.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "mathlib.h"
#include "log.h"
#include "plf.h"
#include "check.h"

class NitrificationSoil : public Nitrification
{
  // Parameters.
private: 
  const bool active_underground; // True, iff turnover happens below rootzone.
  const bool active_groundwater; // True, iff turnover happens in groundwater.
  const double k;
  const double k_10;
  const PLF heat_factor;
  const PLF water_factor;

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
  daisy_assert (false);
}

void
NitrificationSoil::output (Log& log) const
{
  output_variable (converted, log);
}

void 
NitrificationSoil::tick (const Soil& soil, const SoilWater& soil_water,
			 const SoilHeat& soil_heat,
			 SoilNO3& soil_NO3, SoilNH4& soil_NH4)
{
  for (int i = 0; i < soil.size (); i++)
    {
      daisy_assert (soil_NO3.M_left (i) >= 0.0);
      daisy_assert (soil_NH4.M_left (i) >= 0.0);
    }

  converted.erase (converted.begin (), converted.end ());

  unsigned int size = soil.size ();
  if (!active_underground)
    size = min (size, soil.interval_plus (soil.MaxRootingDepth ()));
  if (!active_groundwater)
    size = soil_water.first_groundwater_node ();

  for (unsigned int i = 0; i < size; i++)
    {
      const double M = soil_NH4.M (i);
      const double h = soil_water.h (i);
      const double T = soil_heat.T (i);
      const double T_factor = (heat_factor.size () < 1)
	? f_T (T)
	: heat_factor (T);
      const double w_factor = (water_factor.size () < 1)
	? f_h (h)
	: water_factor (h);

      const double rate = k_10 * w_factor * T_factor * M / (k + M);
      daisy_assert (rate >= 0.0);
      daisy_assert (soil_NH4.M_left (i) >= 0.0);
      const double M_new = min (rate, soil_NH4.M_left (i) / dt  - 1e-8);
      if (M_new > 0.0)
	converted.push_back (M_new);
      else 
	converted.push_back (0.0);
    }
  soil_NH4.add_to_sink (converted);
  soil_NO3.add_to_source (converted);

  for (int i = 0; i < soil.size (); i++)
    {
      daisy_assert (soil_NO3.M_left (i) >= 0.0);
      daisy_assert (soil_NH4.M_left (i) >= 0.0);
    }
}

NitrificationSoil::NitrificationSoil (const AttributeList& al)
  : Nitrification (al),
    active_underground (al.flag ("active_underground")),
    active_groundwater (al.flag ("active_groundwater")),
    k (al.number ("k")),
    k_10 (al.number ("k_10")),
    heat_factor (al.plf ("heat_factor")),
    water_factor (al.plf ("water_factor"))
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
      alist.add ("description", 
		 "k_10 * M / (k + M).  Michaelis-Menten kinetics,\n\
with nitrification based on total ammonium content.");
      syntax.add ("active_underground", Syntax::Boolean, Syntax::Const, "\
Set this to true to enable nitrification below the root zone.");
      alist.add ("active_underground", false);
      syntax.add ("active_groundwater", Syntax::Boolean, Syntax::Const, "\
Set this to true to enable nitrification in the groundwater.");
      alist.add ("active_groundwater", false);
      syntax.add ("converted", 
		  "g N/cm^3/h", Syntax::LogOnly, Syntax::Sequence, 
		  "Amount of ammonium converted this hour.");
      syntax.add ("k", "g N/cm^3", Check::positive (), Syntax::Const, 
		  "Half saturation constant.");
      syntax.add ("k_10", "g N/cm^3/h", Check::non_negative (), Syntax::Const,
		  "Max rate.");
      syntax.add ("heat_factor", "dg C", Syntax::None (), Syntax::Const,
		  "Heat factor.");
      alist.add ("heat_factor", PLF::empty ());
      syntax.add ("water_factor", "cm", Syntax::None (), Syntax::Const,
		  "Water potential factor.");
      alist.add ("water_factor", PLF::empty ());
      Librarian<Nitrification>::add_type ("soil", alist, syntax, &make);
    }
} NitrificationSoil_syntax;
