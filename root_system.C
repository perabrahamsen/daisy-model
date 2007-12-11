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

#define BUILD_DLL

#include "root_system.h"
#include "rootdens.h"
#include "submodel.h"
#include "geometry.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "soil.h"
#include "chemical.h"
#include "chemistry.h"
#include "log.h"
#include "check.h"
#include "block.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

double 
RootSystem::potential_water_uptake (const double h_x,
                                    const Geometry& geo,
				    const Soil& soil,
				    const SoilWater& soil_water,
                                    const double dt)
{
  const std::vector<double>& L = Density;
  std::vector<double>& S = H2OExtraction;
  const size_t size = std::min (S.size (), soil.size ());
  daisy_assert (L.size () >= size);
  const double area = M_PI * Rad * Rad;

  for (size_t i = 0; i < size; i++)
    {
      if (L[i] <= 0.0 || soil_water.h (i) >= 0.0)
	{
	  S[i] = 0.0;
	  continue;
	}
      const double h = h_x - (1 + Rxylem) * geo.z (i);
      daisy_assert (soil_water.Theta_left (i, dt) >= 0.0);
      daisy_assert (soil_water.Theta_ice (soil, i, h_wp) 
                    >= soil.Theta_res (i));
      const double max_uptake
	= std::max (0.0, (soil_water.Theta_left (i, dt) 
		     - soil_water.Theta_ice (soil, i, h_wp)) / dt);
      const double uptake
	= bound (0.0, 
		 (2 * M_PI * L[i]
		    * (soil_water.Theta_ice (soil, i, h) 
		       / soil_water.Theta_ice (soil, i, 0.0))
		    * (soil.M (i, soil_water.h (i)) - soil.M (i, h))
		  / (- 0.5 * log (area * L[i]))),
		 max_uptake);
      daisy_assert (soil_water.h (i) > h_wp || iszero (uptake));
      daisy_assert (soil_water.Theta_left (i, dt) - uptake * dt
                    > soil.Theta_res (i));
      daisy_assert (L[i] >= 0.0);
      daisy_assert (soil_water.Theta_ice (soil, i, h) > 0.0);
      daisy_assert (soil_water.Theta_ice (soil, i, 0.0) > 0.0);
      daisy_assert (soil.M (i, soil_water.h (i)) >= 0.0);
      daisy_assert (soil.M (i, h) >= 0.0);
      daisy_assert (area * L[i] > 0.0);
      daisy_assert (std::isnormal (- 0.5 * log (area * L[i])));
      daisy_assert (uptake >= 0.0);
      S[i] = uptake;
    }
  return geo.total_surface (S) * 10.0 /* [mm/cm] */;
}

double
RootSystem::water_uptake (double Ept_,
                          const Geometry& geo,
			  const Soil& soil,
			  SoilWater& soil_water,
			  const double EvapInterception,
			  const double day_fraction,
                          const double dt,
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
  double total = potential_water_uptake (h_x, geo, soil, soil_water, dt);
  double step = min_step;

  while (total < Ept && h_x > h_wp)
    {
      const double h_next = std::max (h_x - step, h_wp);
      const double next = potential_water_uptake (h_next, geo, 
                                                  soil, soil_water, dt);

      if (next < total)
	// We are past the top of the curve.
	if (step <= min_step)
	  // We cannot go any closer to the top, skip it.
	  {
	    h_x = h_wp;
	    total = potential_water_uptake (h_x, geo, soil, soil_water, dt);
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
      const double h_next = std::min (h_x + step, 0.0);
      const double next = potential_water_uptake (h_next, geo, 
                                                  soil, soil_water, dt);

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
  const double total2 
    = potential_water_uptake (h_x, geo, soil, soil_water, dt);
  daisy_assert (approximate (total, total2));
  daisy_assert (h_x >= h_wp);

  if (total > Ept)
    {
      daisy_assert (h_x < 0.001);
      daisy_assert (total > 0);
      const double factor = Ept / total;
      for (size_t i = 0; i < soil.size (); i++)
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
RootSystem::solute_uptake (const Geometry& geo, const Soil& soil,
			   const SoilWater& soil_water,
			   Chemical& solute,
			   double PotNUpt,
			   std::vector<double>& uptake,
			   const double I_max,
			   const double C_root_min,
                           const double dt)
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
  std::vector<double> I_zero (size, 0.0);
  std::vector<double> B_zero (size, 0.0);
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
		/ (beta_squared * log (beta_squared) / (beta_squared - 1.0)
                   - 1.0);
	      I_zero[i] = B_zero[i] * C_l;
	    }
	  else
            { 
              const double divisor 
                = ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
                   - (pow (beta, 2.0 - alpha) - 1.0));
              
              if (std::isnormal (divisor))
                {
                  B_zero[i] = q_r * (pow (beta, 2.0 - alpha) - 1.0)
                    / divisor;
                  I_zero[i] = q_r * (beta_squared - 1.0) * (1.0 - 0.5 * alpha)
                    * C_l / divisor;
                }
              else              
                {
                  daisy_assert (approximate (alpha, 2.0));
                  const double div2 
                    = ((beta_squared - 1.0) - log (beta_squared));
                  daisy_assert (std::isnormal (div2));
                  B_zero[i] = q_r * log (beta_squared) / div2;
                  I_zero[i] = q_r * (beta_squared - 1.0) * C_l / div2;
                }
            }
	  daisy_assert (std::isfinite (I_zero[i]));
	  daisy_assert (std::isfinite (B_zero[i]));
	  B += L * geo.cell_volume (i) * B_zero[i];
	  U_zero += L * geo.cell_volume (i) 
	    * bound (0.0, I_zero[i] - B_zero[i] * C_root_min, I_max);
	}
    }
  double C_root = C_root_min;
  if (U_zero > PotNUpt)
    C_root = std::max ((U_zero - PotNUpt) / B, C_root_min);

  for (int i = 0; i < size; i++)
    {
      const double L = Density[i];
      if (solute.M_left (i, dt) > 1e-8 && L > 0 && soil_water.h (i) <= 0.0)
	uptake[i] = bound (0.0,
			   L * (std::min (I_zero[i], I_max)
				- B_zero[i] * C_root),
			   std::max (solute.M_left (i, dt) - 1e-8, 0.0));
      else
	uptake[i] = 0.0;
      daisy_assert (uptake[i] >= 0.0);
    }
  solute.add_to_root_sink (uptake, dt);

  // gN/cm³/h -> gN/m²/h
  return geo.total_surface (uptake) * 1.0e4;
}

double
RootSystem::nitrogen_uptake (const Geometry& geo, const Soil& soil,
			     const SoilWater& soil_water,
			     Chemistry& chemistry,
			     const double NH4_root_min,
			     const double NO3_root_min,
			     const double PotNUpt, double dt)
{
  // If we don't track inorganic N, assume we have enough.
  if (!chemistry.know (Chemical::NH4_solute ())
      || !chemistry.know (Chemical::NO3 ()))
    return PotNUpt;
    
  Chemical& soil_NH4 = chemistry.find (Chemical::NH4_solute ());
  Chemical& soil_NO3 = chemistry.find (Chemical::NO3 ());

  NH4Upt = solute_uptake (geo, soil, soil_water, soil_NH4, 
			  PotNUpt, NH4Extraction, MxNH4Up, NH4_root_min, dt);
  NO3Upt = solute_uptake (geo, soil, soil_water, soil_NO3, 
			  PotNUpt - NH4Upt, NO3Extraction, 
			  MxNO3Up, NO3_root_min, dt);

  daisy_assert (NH4Upt >= 0.0);
  daisy_assert (NO3Upt >= 0.0);

  return NH4Upt + NO3Upt;
}

void
RootSystem::tick (const double T, const double dt)
{
  partial_soil_temperature += T * dt;
  partial_day += dt;
  if (partial_day >= 24.0)
    {
      soil_temperature = partial_soil_temperature / partial_day;
      partial_soil_temperature = 0.0;
      partial_day = 0.0;
    }

  // Clear nitrogen.
  NH4Upt = NO3Upt =0.0;
}

void
RootSystem::tick_daily (const Geometry& geo, const Soil& soil, 
			const double WRoot, const bool root_growth,
			const double DS, Treelog& msg)
{
  // Penetration.
  if (root_growth)
    {
      const double clay = geo.content_at (soil, &Soil::clay, -Depth);
      double clay_fac = PenClayFac (clay);
      double dp = PenPar1 * clay_fac * std::max (0.0, soil_temperature - PenPar2);
      PotRtDpt = std::min (PotRtDpt + dp, MaxPen);
      /*max depth determined by crop*/
      Depth = std::min (Depth + dp, MaxPen);
      PotRtDpt = std::max (PotRtDpt, Depth);
      /*max depth determined by crop*/
      Depth = std::min (Depth, -soil.MaxRootingHeight ()); /*or by soil conditions*/
    }
  set_density (geo, WRoot, DS, msg);
}

void
RootSystem::set_density (const Geometry& geo, 
			 const double WRoot, const double DS, Treelog& msg)
{ rootdens->set_density (geo, MaxPen, PotRtDpt, PotRtDpt,
			 WRoot, DS, Density, msg); }

void
RootSystem::full_grown (const Geometry& geo, 
                        const double max_rooting_depth,
			const double WRoot, Treelog& msg)
{
  PotRtDpt = MaxPen;
  Depth = std::min (MaxPen, -max_rooting_depth);
  set_density (geo, WRoot, 1.0, msg);
}

void
RootSystem::output (Log& log) const
{
  output_derived (rootdens, "rootdens", log);
  output_derived (ABAprod, "ABAprod", log);
  output_variable (PotRtDpt, log);
  output_variable (Depth, log);
  output_variable (Density, log);
  output_variable (H2OExtraction, log);
  output_variable (NH4Extraction, log);
  output_variable (NO3Extraction, log);
  output_variable (h_x, log);
  output_variable (partial_soil_temperature, log);
  output_variable (partial_day, log);
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
RootSystem::initialize (size_t size)
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

  syntax.add_object ("rootdens", Rootdens::component,
                     "Root density model.");
  alist.add ("rootdens", Rootdens::default_model ());

  syntax.add_object ("ABAprod", ABAProd::component,
                     "ABA production model.");
  alist.add ("ABAprod", ABAProd::default_model ());

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
  alist.add ("MxNO3Up", 2.5e-7);
  syntax.add ("Rxylem", Syntax::None (), Check::non_negative (), Syntax::Const,
	    "Transport resistence in xyleme.");
  alist.add ("Rxylem", 10.0);

  syntax.add ("PotRtDpt", "cm", Check::non_negative (), Syntax::OptionalState,
	      "Potential root penetration depth.");
  syntax.add ("Depth", "cm", Check::non_negative (), Syntax::OptionalState,
	      "Rooting Depth.");
  syntax.add ("Density", "cm/cm^3", Check::non_negative (),
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
  syntax.add ("ABAExtraction", "g/cm^3/h", Check::non_negative (), 
	      Syntax::LogOnly, Syntax::Sequence,
	      "Extraction of ABA in soil layers.");
  syntax.add ("ABAConc", "g/cm^3/h", Check::non_negative (), 
	      Syntax::State, "ABA concentration in water uptake.");
  alist.add ("ABAConc", 0.0);
  syntax.add ("h_x", "cm", Check::none (), Syntax::State,
	       "Root extraction at surface.");
  alist.add ("h_x", 0.0);
  syntax.add ("partial_soil_temperature", "dg C h", Syntax::State,
	      "Soil temperature hours this day, so far.");
  alist.add ("partial_soil_temperature", 0.0);
  syntax.add ("partial_day", "h", Syntax::State,
	      "Hours we have accumulated soil temperature this day.");
  alist.add ("partial_day", 0.0);
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
get_PotRtDpt (Block& al)
{
  if (al.check ("PotRtDpt"))
    return al.number ("PotRtDpt");
  if (al.check ("Depth"))
    return al.number ("Depth");
  return al.number ("DptEmr");
}

RootSystem::RootSystem (Block& al)
  : rootdens (Librarian::build_item<Rootdens> (al, "rootdens")),
    ABAprod (Librarian::build_item<ABAProd> (al, "ABAprod")),
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
    ABAConc (al.number ("ABAConc")),
    h_x (al.number ("h_x")),
    partial_soil_temperature (al.number ("partial_soil_temperature")),
    partial_day (al.number ("partial_day")),
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
