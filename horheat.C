// horheat.C --- Heat capicity and conductivity for horizons.
// 
// Copyright 1996-2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2004 KVL.
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


#include "horheat.h"
#include "texture.h"
#include "hydraulic.h"
#include "alist.h"
#include "syntax.h"
#include "check.h"
#include "tmpstream.h"
#include "mathlib.h"
#include "assertion.h"
#include "submodel.h"
#include <numeric>

double
HorHeat::heat_conductivity (double Theta, double Ice) const
{
  const int entry = int ((Theta + Ice) * intervals);
  daisy_assert (entry >= 0);
  daisy_assert (entry < intervals);
  return ((K_ice[entry] * Ice + K_water[entry] * Theta) 
	  / (Theta + Ice))
    * 3600;			// erg/s / cm / K -> erg/h / cm / K
}

double
HorHeat::heat_capacity (double pTheta, double pIce) const
{ return C_soil 
    + heat_capacity_table[Water] * pTheta 
    + heat_capacity_table[Ice] * pIce; }

void
HorHeat::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "HorHeat");
  alist.add ("description", "Heat capacity and conductivity per horizon.");

  syntax.add ("quarts_form_factor", Syntax::None (), Syntax::Const,
	      "Gemetry factor used for conductivity calculation.");
  alist.add ("quarts_form_factor", 2.0);
  syntax.add ("mineral_form_factor", Syntax::None (), Syntax::Const,
	      "Gemetry factor used for conductivity calculation.");
  alist.add ("mineral_form_factor", 4.0);
  syntax.add ("intervals", Syntax::Integer, Syntax::Const, "\
Number of numeric intervals to use in the heat coductivity table.");
  alist.add ("intervals", 100);
  syntax.add ("C_soil", "erg/cm^3/dg C", Check::positive (), 
	      Syntax::OptionalConst,
	      "The soils heat capacity.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("K_water",
	      "erg/s/cm/dg C", Check::positive (),
	      Syntax::OptionalConst, Syntax::Sequence,
	      "Heat conductivity table for water in soil.\n\
By default, this is calculated from the soil constituents.");
  syntax.add ("K_ice",
	      "erg/s/cm/dg C", Check::positive (),
	      Syntax::OptionalConst, Syntax::Sequence,
	      "Heat conductivity table for solid frozen soil.\n\
By default, this is calculated from the soil constituents.");
}

void 
HorHeat::initialize (const Hydraulic& hydraulic, const Texture& texture,
                     const double quarts, Treelog& msg)
{
  // Already initialized.
  if (K_water.size () != 0)
    return;

  // The particles are not in a real continuous medium.  Try to correct.
  const double continuum_correction_factor = 1.25;
      
  // Above this pF heat is mostly tranfered by Air.
  Theta_pF_high = hydraulic.Theta (pF2h (4.2));
      
  // Below this pf heat is mostly transfered by Water or Ice.
  const double Theta_pF_low
    = (hydraulic.Theta (pF2h (2.0)) + Theta_pF_high) / 2.0;

  // Water that won't freeze.
  const double LiquidWater = Theta_pF_high; 
  
  // Quarts content in soil.
  TmpStream tmp;
  tmp () << "Quartz = " << quarts << " []";
  msg.debug (tmp.str ());

  // Relative content of various constituents in soil.
  content[Quarts] = quarts * (1.0 - hydraulic.Theta_sat);
  content[Minerals] = (texture.mineral () - quarts) 
    * (1.0 - hydraulic.Theta_sat);
  content[Organic_Matter] = texture.humus * (1.0 - hydraulic.Theta_sat);

  // Find capasity of dry soil.
  content[Air] = hydraulic.porosity ();
  content[Water] = 0.0;
  content[Ice] = 0.0;
  C_soil = HeatCapacity ();

  // We calculate for water between Theta_res and Theta_sat.
  const int from = double2int (floor (intervals * hydraulic.Theta_res));
  const int to = double2int (ceil (intervals * hydraulic.Theta_sat));
  
  daisy_assert (0 <= from);
  daisy_assert (from < to);
  daisy_assert (to < intervals);

  // Make room, make room...
  K_water.insert (K_water.end (), intervals, 0.0);
  K_ice.insert (K_ice.end (), intervals, 0.0);

  for (int i = from; i < to; i++)
    {
      // Fill out water, ice, and air for pure water system.
      content[Water] = (i + 0.0) / (intervals + 0.0);
      content[Ice] = 0.0;
      content[Air] = hydraulic.porosity () - content[Water];

      // Calculate termal attributes for this combination.
      const double K_water_wet = ThermalConductivity (hydraulic, Water);
      const double K_water_dry = continuum_correction_factor
	* ThermalConductivity (hydraulic, Air);
      
      // Find actual conductivity in combined water and air system.
      if (content[Water] < Theta_pF_high)
	K_water[i] = K_water_dry;
      else if (content[Water] > Theta_pF_low)
	K_water[i] = K_water_wet;
      else
	K_water[i] = K_water_dry 
	  + (K_water_wet - K_water_dry)
	  * (content[Water] - Theta_pF_high)
	  / (Theta_pF_low - Theta_pF_high);
      
      // Fill out water, ice, and air for pure ice system.
      content[Water] = min (LiquidWater, (i + 0.0) / (intervals + 0.0));
      content[Ice] = max ((i + 0.0) / (intervals + 0.0) - LiquidWater, 0.0);
      content[Air] = hydraulic.Theta_sat - (content[Water] + content[Ice]);
      
      // Calculate termal attributes for this combination.
      const double K_ice_wet = ThermalConductivity (hydraulic, Ice);
      const double K_ice_dry = continuum_correction_factor 
	* ThermalConductivity (hydraulic, Air);
      
      // Find actual conductivity in combined ice and air system.
      if (content[Water] + content[Ice] < Theta_pF_high)
	K_ice[i] = K_ice_dry;
      else if (content[Water] + content[Ice] > Theta_pF_low)
	K_ice[i] = K_ice_wet;
      else
	K_ice[i] = K_ice_dry 
	  + (K_ice_wet - K_ice_dry)
	  * (content[Water] + content[Ice] - Theta_pF_high)
	  / (Theta_pF_low - Theta_pF_high);
    }
  for (int i = to; i < intervals; i++)
    {
      K_water[i] = K_water[to-1];
      K_ice[i] = K_ice[to-1];
    }
  for (int i = 0; i < from; i++)
    {
      K_water[i] = K_water[from];
      K_ice[i] = K_ice[from];
    }
}

double 
HorHeat::HeatCapacity ()
{
  daisy_assert (approximate (accumulate (&content[0], 
                                         &content[Constituents_End],
                                         0.0),
                             1.0));
  double C = 0.0;
  for (int i = 0; i < Constituents_End; i++)
    C += heat_capacity_table[i] * content[i];
  
  return C;
}

double 
HorHeat::DepolationsFactor (const Hydraulic& hydraulic,
					    const constituents medium, 
					    const double alfa)
{
  if (medium == Air)
    return 0.333 - (0.333 - 0.070) * content[Air] / (hydraulic.porosity()
						     - Theta_pF_high);

  const double a = 1 - alfa * alfa;
  
  if (alfa < 1)
    return 1.0 / (2.0 * a)
      + alfa * alfa / (4.0 * a * sqrt (a)) 
      * log ((1.0 - sqrt (a)) / (1.0 + sqrt (a)));
  if (alfa == 1.0)
    return 1.0 / 3.0;
  if (alfa > 1.0)
    return (alfa * alfa / sqrt (-a) * (M_PI_2 - atan (sqrt (-1.0 / a))) - 1.0)
      / (2.0 * -a);
  daisy_assert (false);
}

double 
HorHeat::ThermalConductivity (const Hydraulic& hydraulic,
					      constituents medium)
{
  // Thermal conductivity of each medium.
  double thermal_conductivity[Constituents_End] =
  { 0.57e5, 2.2e5, 0.025e5, 8.8e5, 2.9e5, 0.25e5 }; // [erg/s/cm/dg C]

  // Air conductivity is modified by water vapour.
  const double vapour_conductivity = 0.040e5;
  thermal_conductivity[Air] 
    += vapour_conductivity * min (1.0, (content[Water] / Theta_pF_high));
  
  double S1 = content[medium] * thermal_conductivity[medium];
  double S2 = content[medium];
  
  for (constituents i = Constituents_Start;
       i < Constituents_End;
       // C++ enums SUCKS!
       i = constituents (i + 1))
    {
      if (i != medium && content[i] > 0.0)
	{
	  const double a = thermal_conductivity[i] 
	    / thermal_conductivity[medium] - 1.0;
	  double k = -42.42e42;
	  switch (i)
	    {
	    case Water:
	    case Ice:
	      k = (1.0 / (1.0 + a)) / 3.0;
	      break;
	    case Quarts:
	    case Minerals:
	    case Air:
	      {
		const double g = DepolationsFactor (hydraulic, i, 
						    (i == Quarts)
						    ? quarts_form_factor
						    : mineral_form_factor);
		k = (2.0 / (1.0 + a * g)
		     + 1.0 / (1.0 + a * (1.0 - 2.0 * g)))
		  / 3.0;
	      }
	      break;
	    case Organic_Matter:
	      {
		const double Alfa = -3.0;
		k = (1.0 / (1.0 + a / (1.0 - Alfa))
		     + 1.0 / (1.0 - a * Alfa / (1.0 - Alfa))) / 3.0;
	      }
	    break;
	    case Constituents_End:
	      abort ();
	    }
	  S1 += k * content[i] * thermal_conductivity[i];
	  S2 += k * content[i];
	}
    }
  return S1 / S2;
}

const double 
HorHeat::heat_capacity_table[Constituents_End] = // [erg / cm³ / °C]
// Ice is given as equivalent amount of water.
{ 4.2e7, 1.9e7 * (1.0 / 0.92), 1.25e4, 2.0e7, 2.0e7, 2.5e7 }; 

HorHeat::HorHeat (const AttributeList& al)
  : quarts_form_factor (al.number ("quarts_form_factor")),
    mineral_form_factor (al.number ("mineral_form_factor")),
    C_soil (al.number ("C_soil", -42.42e42)),
    K_water (al.check ("C_soil")
             ? al.number_sequence ("K_water") 
             : vector<double> ()),
    K_ice (al.check ("K_ice")
           ? al.number_sequence ("K_ice") 
           : vector<double> ()),
    intervals (al.integer ("intervals"))
{ }

HorHeat::~HorHeat ()
{ }

static Submodel::Register horheat_submodel ("HorHeat", HorHeat::load_syntax);
