// root_system.C -- Root development and uptake.
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


#include "root_system.h"
#include "rootdens.h"
#include "submodel.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "soil_water.h"
#include "soil.h"
#include "log.h"
#include "check.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

double 
RootSystem::potential_water_uptake (const double h_x,
				    const Soil& soil,
				    const SoilWater& soil_water)
{
  const vector<double>& L = Density;
  vector<double>& S = H2OExtraction;

  const double area = M_PI * Rad * Rad;
  double total = 0.0;
  for (unsigned int i = 0; i < soil.size () && L[i] > 0.0; i++)
    {
      if (soil_water.h (i) >= 0.0)
	{
	  S[i] = 0.0;
	  continue;
	}
      const double h = h_x - (1 + Rxylem) * soil.z (i);
      daisy_assert (soil_water.Theta_left (i) >= 0.0);
      daisy_assert (soil_water.Theta (soil, i, h_wp) >= soil.Theta_res (i));
      const double max_uptake
	= max (0.0, (soil_water.Theta_left (i) 
		     - soil_water.Theta (soil, i, h_wp)) / dt);
      const double uptake
	= bound (0.0, 
		 (2 * M_PI * L[i]
		    * (soil_water.Theta (soil, i, h) 
		       / soil_water.Theta (soil, i, 0.0))
		    * (soil.M (i, soil_water.h (i)) - soil.M (i, h))
		  / (- 0.5 * log (area * L[i]))),
		 max_uptake);
      daisy_assert (soil_water.h (i) > h_wp || uptake == 0.0);
      daisy_assert (soil_water.Theta_left (i) - uptake > soil.Theta_res (i));
      daisy_assert (L[i] >= 0.0);
      daisy_assert (soil_water.Theta (soil, i, h) > 0.0);
      daisy_assert (soil_water.Theta (soil, i, 0.0) > 0.0);
      daisy_assert (soil.M (i, soil_water.h (i)) >= 0.0);
      daisy_assert (soil.M (i, h) >= 0.0);
      daisy_assert (area * L[i] > 0.0);
      daisy_assert ((- 0.5 * log (area * L[i])) != 0.0);
      daisy_assert (uptake >= 0.0);
      S[i] = uptake;
      total += uptake * soil.dz (i) * 10; // mm/cm.
    }
  return total;
}

double
RootSystem::water_uptake (double Ept_,
			  const Soil& soil,
			  SoilWater& soil_water,
			  const double EvapInterception,
			  const double day_fraction,
			  Treelog& msg)
{
  daisy_assert (EvapInterception >= 0);
  if (Ept_ < 0)
    {
      Treelog::Open nest (msg, "RootSystem water uptake");
      std::ostringstream tmp;
      tmp << "BUG: Negative EPT (" << Ept_ << ")";
      msg.error (tmp.str ());
      Ept_ = 0.0;
    }
  Ept = Ept_;

  static const double min_step = 1.0;
  double total = potential_water_uptake (h_x, soil, soil_water);
  double step = min_step;

  while (total < Ept && h_x > h_wp)
    {
      const double h_next = max (h_x - step, h_wp);
      const double next = potential_water_uptake (h_next, soil, soil_water);

      if (next < total)
	// We are past the top of the curve.
	if (step <= min_step)
	  // We cannot go any closer to the top, skip it.
	  {
	    h_x = h_wp;
	    total = potential_water_uptake (h_x, soil, soil_water);
	    break;
	  }
	else
	  // Try again a little close.
	  {
	    step /= 2;
	    continue;
	  }
      total = next;
      h_x = h_next;
      step *= 2;
    }
  if (h_x < h_wp)
    h_x = h_wp;

  step = min_step;
  daisy_assert (h_x < 0.001);
  while (total > Ept && h_x < 0.0)
    {
      daisy_assert (h_x < 0.001);
      const double h_next = min (h_x + step, 0.0);
      const double next = potential_water_uptake (h_next, soil, soil_water);

      if (next < Ept)
	// We went too far.
	if (step <= min_step)
	  {
	    // We can't get any closer.
	    daisy_assert (next <= total);
	    if (next >= Ept)
	      {
		// total = next;
		h_x = h_next;
	      }
	    else

	    break;
	  }
	else
	  // Try again a little closer.
	  {
	    step /= 2;
	    continue;
	  }

      total = next;
      h_x = h_next;
      step *= 2;
    }

  // We need this to make sure H2OExtraction corresponds to 'h_x'.
  const double total2 = potential_water_uptake (h_x, soil, soil_water);
  daisy_assert (total == total2);
  daisy_assert (h_x >= h_wp);

  if (total > Ept)
    {
      daisy_assert (h_x < 0.001);
      daisy_assert (total > 0);
      const double factor = Ept / total;
      for (unsigned int i = 0; i < soil.size (); i++)
	H2OExtraction[i] *= factor;
      total = Ept;
    }
  H2OUpt = total;

  // Update soil water sink term.
  soil_water.root_uptake (H2OExtraction);
  // Update water stress factor
  if (Ept < 0.010)
    water_stress = 0.0;
  else
    water_stress = 1.0 - (total + EvapInterception) / (Ept + EvapInterception);
  water_stress_days += water_stress * day_fraction;

  return H2OUpt;
}

double
RootSystem::solute_uptake (const Soil& soil,
			   const SoilWater& soil_water,
			   Solute& solute,
			   double PotNUpt,
			   vector<double>& uptake,
			   const double I_max,
			   const double C_root_min)
{
  if (PotNUpt <= 0.0)
    {
      fill (uptake.begin (), uptake.end (), 0.0);
      return 0.0;
    }

  daisy_assert (PotNUpt > 0.0);
  PotNUpt /= 1.0e4;		// gN/m²/h -> gN/cm²/h
  const int size = soil.size ();
  // I: Uptake per root length.
  // I = I_zero - B_zero * C_root
  vector<double> I_zero (size, 0.0);
  vector<double> B_zero (size, 0.0);
  double U_zero = 0.0;		// Total uptake a C_root_min
  double B = 0.0;

  for (int i = 0; i < size; i++)
    {
      const double C_l = solute.C (i);
      const double Theta = soil_water.Theta_old (i);
      const double L = Density[i];
      if (L > 0 && soil_water.h (i) <= 0.0)
	{
	  const double q_r = H2OExtraction[i] / L;
	  const double D = solute.diffusion_coefficient ()
	    * soil.tortuosity_factor (i, Theta)
	    * Theta;
	  const double alpha = q_r / ( 2 * M_PI * D);
	  const double beta = 1.0 / (Rad * sqrt (M_PI * L));
	  const double beta_squared = beta * beta;
	  if (alpha < 1e-10)
	    {
	      B_zero[i] = 4.0 * M_PI * D
		/ (beta_squared * log (beta_squared) / (beta_squared - 1.0) - 1.0);
	      I_zero[i] = B_zero[i] * C_l;
	    }
	  else if (alpha == 2.0)
	    {
	      B_zero[i] = q_r * log (beta_squared)
		/ ((beta_squared - 1.0) - log (beta_squared));
	      I_zero[i] = q_r * (beta_squared - 1.0) * C_l
		/ ((beta_squared - 1.0) - log (beta_squared));
	    }
	  else
	    {
	      B_zero[i] = q_r * (pow (beta, 2.0 - alpha) - 1.0)
		/ ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
		   - (pow (beta, 2.0 - alpha) - 1.0));
	      I_zero[i] = q_r * (beta_squared - 1.0) * (1.0 - 0.5 * alpha) * C_l
		/ ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
		   - (pow (beta, 2.0 - alpha) - 1.0));
	    }
	  daisy_assert (isfinite (I_zero[i]));
	  daisy_assert (isfinite (B_zero[i]));
	  B += L * soil.dz (i) * B_zero[i];
	  U_zero += L * soil.dz (i) 
	    * bound (0.0, I_zero[i] - B_zero[i] * C_root_min, I_max);
	}
    }
  double C_root = C_root_min;
  if (U_zero > PotNUpt)
    C_root = max ((U_zero - PotNUpt) / B, C_root_min);

  for (int i = 0; i < size; i++)
    {
      const double L = Density[i];
      if (solute.M_left (i) > 1e-8 && L > 0 && soil_water.h (i) <= 0.0)
	uptake[i] = bound (0.0,
			   L * (min (I_zero[i], I_max)
				- B_zero[i] * C_root),
			   max (solute.M_left (i) - 1e-8, 0.0));
      else
	uptake[i] = 0.0;
      daisy_assert (uptake[i] >= 0.0);
    }
  solute.add_to_root_sink (uptake);

  // gN/cm³/h -> gN/m²/h
  return soil.total (uptake) * 1.0e4;
}

double
RootSystem::nitrogen_uptake (const Soil& soil,
			     const SoilWater& soil_water,
			     SoilNH4& soil_NH4,
			     const double NH4_root_min,
			     SoilNO3& soil_NO3,
			     const double NO3_root_min,
			     const double PotNUpt)
{
  NH4Upt = solute_uptake (soil, soil_water, soil_NH4, 
			  PotNUpt, NH4Extraction, MxNH4Up, NH4_root_min);
  NO3Upt = solute_uptake (soil, soil_water, soil_NO3, 
			  PotNUpt - NH4Upt, NO3Extraction, 
			  MxNO3Up, NO3_root_min);

  daisy_assert (NH4Upt >= 0.0);
  daisy_assert (NO3Upt >= 0.0);

  return NH4Upt + NO3Upt;
}

void
RootSystem::tick_hourly (int hour, double T)
{
  partial_soil_temperature += T;
  if (hour == 0)
    {
      soil_temperature = partial_soil_temperature / 24.0;
      partial_soil_temperature = 0.0;
    }

  // Clear nitrogen.
  NH4Upt = NO3Upt =0.0;
}

void
RootSystem::tick_daily (Treelog& msg, const Soil& soil, 
			const double WRoot, const double IncWRoot,
			const double DS)
{
  // Penetration.
  if (IncWRoot > 0)
    {
      const int i = soil.interval_plus (-Depth);
      double clay_fac = PenClayFac (soil.clay (i));
      double dp = PenPar1 * clay_fac * max (0.0, soil_temperature - PenPar2);
      PotRtDpt = min (PotRtDpt + dp, MaxPen);
      /*max depth determined by crop*/
      Depth = min (Depth + dp, MaxPen);
      PotRtDpt = max (PotRtDpt, Depth);
      /*max depth determined by crop*/
      Depth = min (Depth, -soil.MaxRootingDepth ()); /*or by soil conditions*/
    }
  set_density (msg, soil, WRoot, DS);
}

void
RootSystem::set_density (Treelog& msg, const Geometry& geometry, 
			 const double WRoot, const double DS)
{ rootdens->set_density (msg, Density, geometry, Depth, PotRtDpt, WRoot, DS); }

void
RootSystem::full_grown (Treelog& msg, const Soil& soil, 
			const double WRoot)
{
  PotRtDpt = MaxPen;
  Depth = min (MaxPen, -soil.MaxRootingDepth ());
  set_density (msg, soil, WRoot, 1.0);
}

void
RootSystem::output (Log& log) const
{
  output_derived (rootdens, "rootdens", log);
  output_variable (PotRtDpt, log);
  output_variable (Depth, log);
  output_variable (Density, log);
  output_variable (H2OExtraction, log);
  output_variable (NH4Extraction, log);
  output_variable (NO3Extraction, log);
  output_variable (h_x, log);
  output_variable (partial_soil_temperature, log);
  output_variable (soil_temperature, log);
  output_variable (water_stress, log);
  output_variable (water_stress_days, log);
  output_variable (production_stress, log);
  output_variable (Ept, log);
  output_variable (H2OUpt, log);
  output_variable (NH4Upt, log);
  output_variable (NO3Upt, log);
}

void
RootSystem::initialize (unsigned int size)
{
  while (Density.size () < size)
    Density.push_back (0.0);
  while (H2OExtraction.size () < size)
    H2OExtraction.push_back (0.0);
  while (NH4Extraction.size () < size)
    NH4Extraction.push_back (0.0);
  while (NO3Extraction.size () < size)
    NO3Extraction.push_back (0.0);
}

void 
RootSystem::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "RootSystem");
  alist.add ("description", "Standard root system model.");

  syntax.add ("rootdens", Librarian<Rootdens>::library (),
	      "Root density model.");
  alist.add ("rootdens", Rootdens::default_model ());

  syntax.add ("DptEmr", "cm", Check::non_negative (), Syntax::Const,
	    "Penetration at emergence.");
  alist.add ("DptEmr", 10.0);
  syntax.add ("PenPar1", "cm/dg C/d", Check::non_negative (), Syntax::Const,
	    "Penetration rate parameter, coefficient.");
  alist.add ("PenPar1", 0.25);
  syntax.add ("PenPar2", "dg C", Check::none (), Syntax::Const,
	    "Penetration rate parameter, threshold.");
  alist.add ("PenPar2", 4.0);
  syntax.add ("PenClayFac", Syntax::Fraction (), Syntax::None (),
	      Check::non_negative (), Syntax::Const, 
	      "Clay dependent factor to multiply 'PenPar1' with.");
  PLF clay;
  clay.add (0.0, 1.0);
  clay.add (1.0, 1.0);
  alist.add ("PenClayFac", clay);
  syntax.add ("MaxPen", "cm", Check::positive (), Syntax::Const,
	    "Maximum penetration depth.");
  alist.add ("MaxPen", 100.0);
  syntax.add ("Rad", "cm", Check::positive (), Syntax::Const,
	    "Root radius.");
  alist.add ("Rad", 0.005);
  syntax.add ("h_wp", "cm", Check::none (), Syntax::Const,
	    "Matrix potential at wilting point.");
  alist.add ("h_wp",-15000.0);
  syntax.add ("MxNH4Up", "g/cm/h", Check::non_negative (), Syntax::Const,
	    "Maximum NH4 uptake per unit root length.");
  alist.add ("MxNH4Up", 2.5e-7);
  syntax.add ("MxNO3Up", "g/cm/h", Check::non_negative (), Syntax::Const,
	    "Maximum NO3 uptake per unit root length.");
  alist.add ("MxNO3Up", 2.5e-8);
  syntax.add ("Rxylem", Syntax::None (), Check::non_negative (), Syntax::Const,
	    "Transport resistence in xyleme.");
  alist.add ("Rxylem", 10.0);

  syntax.add ("PotRtDpt", "cm", Check::non_negative (), Syntax::OptionalState,
	      "Potential root penetration depth.");
  syntax.add ("Depth", "cm", Check::non_negative (), Syntax::OptionalState,
	      "Rooting Depth.");
  syntax.add ("Density", "cm/cm3", Check::non_negative (),
	      Syntax::LogOnly, Syntax::Sequence,
	       "Root density in soil layers.");
  syntax.add ("H2OExtraction", "cm^3/cm^3/h", Check::non_negative (), 
	      Syntax::LogOnly, Syntax::Sequence,
	       "Extraction of H2O in soil layers.");
  syntax.add ("NH4Extraction", "g N/cm^3/h", Check::non_negative (), 
	      Syntax::LogOnly, Syntax::Sequence,
	       "Extraction of NH4-N in soil layers.");
  syntax.add ("NO3Extraction", "g N/cm^3/h", Check::non_negative (), 
	      Syntax::LogOnly, Syntax::Sequence,
	       "Extraction of NO3-N in soil layers.");
  syntax.add ("h_x", "cm", Check::none (), Syntax::State,
	       "Root extraction at surface.");
  alist.add ("h_x", 0.0);
  syntax.add ("partial_soil_temperature", "dg C h", Syntax::State,
	      "Soil temperature hours this day, so far.");
  alist.add ("partial_soil_temperature", 0.0);
  syntax.add ("soil_temperature", "dg C", Syntax::State,
	      "Average soil temperature yesterday.");
  alist.add ("soil_temperature", 0.0);
  syntax.add ("water_stress", Syntax::None (), Check::fraction (),
	      Syntax::LogOnly,
	       "Fraction of requested water we didn't get.");
  syntax.add ("water_stress_days", "d", Check::non_negative (),
	      Syntax::State,
	       "Number of days production has halted due to water stress.\n\
This is the sum of water stress for each hour, multiplied with the\n\
fraction of the radition of that day that was received that hour.");
  alist.add ("water_stress_days", 0.0);
  syntax.add ("production_stress", Syntax::None (), Check::fraction (), 
	      Syntax::LogOnly,
	       "SVAT induced stress, or -1 if not applicable.");
  syntax.add ("Ept", "mm/h", Check::none (), Syntax::LogOnly,
	       "Potential transpiration.");
  syntax.add ("H2OUpt", "mm/h", Check::non_negative (), Syntax::LogOnly,
	      "H2O uptake.");
  syntax.add ("NH4Upt", "g N/m^2/h", Check::non_negative (), Syntax::LogOnly,
	      "NH4-N uptake.");
  syntax.add ("NO3Upt", "g N/m^2/h", Check::non_negative (), Syntax::LogOnly,
	      "NO3-N uptake.");
}

static double
get_PotRtDpt (const AttributeList& al)
{
  if (al.check ("PotRtDpt"))
    return al.number ("PotRtDpt");
  if (al.check ("Depth"))
    return al.number ("Depth");
  return al.number ("DptEmr");
}

RootSystem::RootSystem (const AttributeList& al)
  : rootdens (Librarian<Rootdens>::create (al.alist ("rootdens"))),
    PenPar1 (al.number ("PenPar1")),
    PenPar2 (al.number ("PenPar2")),
    PenClayFac (al.plf ("PenClayFac")),
    MaxPen (al.number ("MaxPen")),
    Rad (al.number ("Rad")),
    h_wp (al.number ("h_wp")),
    MxNH4Up (al.number ("MxNH4Up")),
    MxNO3Up (al.number ("MxNO3Up")),
    Rxylem (al.number ("Rxylem")),
    PotRtDpt (get_PotRtDpt (al)),
    Depth (al.check ("Depth") ? al.number ("Depth") : al.number ("DptEmr")),
    h_x (al.number ("h_x")),
    partial_soil_temperature (al.number ("partial_soil_temperature")),
    soil_temperature (al.number ("soil_temperature")),
    water_stress (0.0),
    water_stress_days (al.number ("water_stress_days")),
    production_stress (-1.0),
    Ept (0.0),
    H2OUpt (0.0),
    NH4Upt (0.0),
    NO3Upt (0.0)
{ }

RootSystem::~RootSystem ()
{ }

static Submodel::Register 
root_system_submodel ("RootSystem", RootSystem::load_syntax);
