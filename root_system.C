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
#include "ABAprod.h"
#include "solupt.h"
#include "librarian.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "soil_water.h"
#include "soil.h"
#include "chemical.h"
#include "chemistry.h"
#include "log.h"
#include "check.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include "metalib.h"
#include <sstream>

double 
RootSystem::crown_potential () const
{ return h_x; }

double 
RootSystem::potential_water_uptake (const double h_x,
                                    const Geometry& geo,
                                    const Soil& soil,
                                    const SoilWater& soil_water,
                                    const double dt)
{
  const std::vector<double>& L = EffectiveDensity;
  std::vector<double>& S = H2OExtraction;
  const size_t size = std::min (S.size (), soil.size ());
  daisy_assert (L.size () >= size);
  const double area = M_PI * Rad * Rad; // [cm^2 R]

  for (size_t i = 0; i < size; i++)
    {
      if (L[i] <= 0.0 || soil_water.h (i) >= 0.0)
        {
          S[i] = 0.0;
          continue;
        }
      const double h = h_x - (1 + Rxylem) * geo.cell_z (i);
      daisy_assert (soil_water.Theta_ice (soil, i, h_wp) > 0.0);
      daisy_assert (soil.Theta_res (i) >= 0.0);
#ifdef THETA_RES
      daisy_assert (soil_water.Theta_ice (soil, i, h_wp) 
                    >= soil.Theta_res (i));
#endif
      const double max_uptake
        = std::max (0.0, (soil_water.Theta (i) 
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
                          const SoilWater& soil_water,
                          const double EvapInterception,
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
        {
          // We are past the top of the curve. 
          // NOTE: potential_water_uptake (h) is not monotonic. 
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
        {
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

  // Update water stress factor
  if (Ept < 0.010)
    water_stress = 0.0;
  else
    water_stress = 1.0 - (total + EvapInterception) / (Ept + EvapInterception);

  // ABA production.
  ABAprod->production (geo, soil_water, H2OExtraction, EffectiveDensity,
                       ABAExtraction, msg);
  const double mm_per_cm = 10.0; // [mm/cm]
  if (H2OUpt > 0.0)
    // [g/cm^3 W] = [g/cm^2 A] * [mm/cm] / [mm/h]
    ABAConc = geo.total_surface (ABAExtraction) * mm_per_cm / H2OUpt;
  else
    { /* Use old value */ }
  daisy_assert (std::isfinite (ABAConc));

  // Result.
  return H2OUpt;
}

double
RootSystem::nitrogen_uptake (const Geometry& geo, const Soil& soil,
                             const SoilWater& soil_water,
                             Chemistry& chemistry,
                             const double NH4_root_min,
                             const double NO3_root_min,
                             const double PotNUpt)
{
  if (PotNUpt <= 0.0)           
    // No uptake needed.
    {
      NH4Upt = NO3Upt = 0.0;
      fill (NH4Extraction.begin (), NH4Extraction.end (), 0.0);
      fill (NO3Extraction.begin (), NO3Extraction.end (), 0.0);
    }
  else if (chemistry.know (Chemical::NH4 ()) 
           && chemistry.know (Chemical::NO3 ()))
    // Normlal uptake.
    {
      Chemical& soil_NH4 = chemistry.find (Chemical::NH4 ());
      Chemical& soil_NO3 = chemistry.find (Chemical::NO3 ());

      NH4Upt = NH4_uptake->value (geo, soil, soil_water,
                                  EffectiveDensity, H2OExtraction, Rad,
                                  soil_NH4, PotNUpt, NH4Extraction,
                                  MxNH4Up, NH4_root_min);
      NO3Upt = NO3_uptake->value (geo, soil, soil_water,
                                  EffectiveDensity, H2OExtraction, Rad,
                                  soil_NO3, PotNUpt - NH4Upt, NO3Extraction, 
                                  MxNO3Up, NO3_root_min);
    }
  else 
    // If we don't track inorganic N, assume we have enough.
    {
      NH4Upt = PotNUpt;
      NO3Upt = 0.0;
    }
  daisy_assert (NH4Upt >= 0.0);
  daisy_assert (NO3Upt >= 0.0);

  return NH4Upt + NO3Upt;
}

void
RootSystem::tick_dynamic (const Geometry& geo, const SoilHeat& soil_heat,
			  SoilWater& soil_water, const double day_fraction,
                          const double dt, Treelog& msg)
{
  // Root death.
  rootdens->tick (geo, soil_heat, soil_water, Density, dt, msg);

  // Update soil water sink term.
  soil_water.root_uptake (H2OExtraction);

  // Accumulated water stress.
  water_stress_days += water_stress * day_fraction;

  // Keep track of daily soil temperature.
  const double T
    = geo.content_height (soil_heat, &SoilHeat::T, -Depth);
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
                        const SoilWater& soil_water,
                        const double WRoot, const bool root_growth,
                        const double DS, Treelog& msg)
{
  const double SoilLimit = -soil.MaxRootingHeight (); // [cm], negative
  const double z = -Depth;	// [cm], negative

  // Penetration.
  if (root_growth)
    {
      // Limit by pressure (pF).
      class pFAccess : public Geometry::Access
      { 
	const SoilWater& soil_water;
	const double pF_min;
	
	double operator()(size_t c) const
	{
	  const double h = soil_water.h (c);
	  if (h < 0.0)
	    return std::max (h2pF (h), pF_min);
	  return pF_min;
	}
      public:
	pFAccess (const SoilWater& sw, const double pF_min_)
	  : soil_water (sw),
	    pF_min (pF_min_)
	{ }
      } pF_accessor (soil_water, 0.0);
      const double pF = geo.access_content_height (pF_accessor, z);
      const double pF_fac = PenpFFac (pF);

      // Limit by clat content.
      const double clay = geo.content_height (soil, &Soil::clay, z);
      const double clay_fac = PenClayFac (clay);

      // Limit by relative water content.
      const double Theta 
        = geo.content_height (soil_water, &SoilWater::Theta, z);
      daisy_assert (Theta >= 0.0);
      const double Theta_sat 
        = geo.content_height (soil, &Soil::Theta_sat, z);
      daisy_assert (Theta_sat > 0.0);

      daisy_assert (Theta_sat <= 1.0);
      const double water = Theta/Theta_sat;
      daisy_assert (water >= 0.0);
      daisy_assert (water <= 1.01);
      double water_fac = PenWaterFac (water);

      // Limit by development stage (DS).
      const double DS_fac = PenDSFac (DS);

      // Limit by horizon.
      const double soil_fac
	= geo.content_height (soil, &Soil::root_retardation, z);
      
      double dp = PenPar1 * pF_fac * clay_fac * water_fac * DS_fac * soil_fac
        * std::max (0.0, soil_temperature - PenPar2);
      PotRtDpt = std::min (PotRtDpt + dp, MaxPen);
      /*max depth determined by crop*/
      Depth = std::min (Depth + dp, MaxPen);
      PotRtDpt = std::max (PotRtDpt, Depth);
      /*max depth determined by crop*/
      Depth = std::min (Depth, SoilLimit); /*or by soil conditions*/
    }
  set_density (geo, soil, WRoot, DS, msg);
}

void
RootSystem::set_density (const Geometry& geo, const Soil& soil,
                         const double WRoot, const double DS, Treelog& msg)
{
  const double SoilLimit = -soil.MaxRootingHeight ();
  rootdens->set_density (geo, SoilLimit, PotRtDpt, 
                         PotRtDpt * (MaxWidth / MaxPen),
                         WRoot, DS, Density, msg);

  const double DS_fac = DensityDSFac (DS);
  daisy_assert (EffectiveDensity.size () == Density.size ());
  daisy_assert (EffectiveDensity.size () == geo.cell_size ());
  for (size_t c = 0; c < geo.cell_size (); c++)
    EffectiveDensity[c] = Density[c] * soil.root_homogeneity (c) * DS_fac;
}

void
RootSystem::full_grown (const Geometry& geo, const Soil& soil,
                        const double WRoot, Treelog& msg)
{
  const double SoilLimit = -soil.MaxRootingHeight ();
  PotRtDpt = MaxPen;
  Depth = std::min (MaxPen, SoilLimit);
  set_density (geo, soil, WRoot, 1.0, msg);
}

void
RootSystem::output (Log& log) const
{
  output_object (rootdens, "rootdens", log);
  output_derived (ABAprod, "ABAprod", log);
  output_derived (NH4_uptake, "NH4_uptake", log);
  output_derived (NO3_uptake, "NO3_uptake", log);
  output_variable (PotRtDpt, log);
  output_variable (Depth, log);
  output_variable (Density, log);
  output_variable (EffectiveDensity, log);
  output_variable (H2OExtraction, log);
  output_variable (NH4Extraction, log);
  output_variable (NO3Extraction, log);
  output_variable (ABAExtraction, log);
  output_variable (ABAConc, log);
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
RootSystem::initialize (const Geometry& geo, const Soil& soil,
			const double row_width, 
                        const double row_pos, const double DS, Treelog& msg)
{
  msg.message ("init 2d");
  const bool is_row_crop = row_width > 0.0;
  if (rootdens.get ())
    /* We already have a root density model. */;
  else if (is_row_crop)
    rootdens = Rootdens::create_row (metalib, msg, row_width, row_pos);
  else
    rootdens = Rootdens::create_uniform (metalib, msg);

  rootdens->initialize (geo, row_width, row_pos, msg);
  initialize (geo, soil, DS, msg);
}

void
RootSystem::initialize (const Geometry& geo, const Soil& soil, const double DS,
			Treelog& msg)
{
  msg.message ("init 1d");
  const size_t cell_size = geo.cell_size ();
  ABAprod->initialize (msg);
  NH4_uptake->initialize (geo, msg);
  NO3_uptake->initialize (geo, msg);
  while (Density.size () < cell_size)
    Density.push_back (0.0);
  const double DS_fac = DensityDSFac (DS);
  EffectiveDensity = Density;
  daisy_assert (EffectiveDensity.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    EffectiveDensity[c] = Density[c] * soil.root_homogeneity (c) * DS_fac;
  while (H2OExtraction.size () < cell_size)
    H2OExtraction.push_back (0.0);
  while (NH4Extraction.size () < cell_size)
    NH4Extraction.push_back (0.0);
  while (NO3Extraction.size () < cell_size)
    NO3Extraction.push_back (0.0);
  while (ABAExtraction.size () < cell_size)
    ABAExtraction.push_back (0.0);
}

bool
RootSystem::check (const Geometry& geo, Treelog& msg) const
{
  const size_t cell_size = geo.cell_size ();
  bool ok = true;
  if (!ABAprod->check (msg))
    ok = false;
  if (Density.size () > 0 && Density.size () != cell_size)
    {
      std::ostringstream tmp;
      tmp << "Density has " << Density.size () 
          << " cells, soil has " << cell_size << " cells";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
RootSystem::load_syntax (Frame& frame)
{

  frame.declare_object ("rootdens", Rootdens::component, 
                        Attribute::OptionalConst, Attribute::Singleton,
                        "Root density model.");

  frame.declare_object ("ABAprod", ABAProd::component, "ABA production model.");
  frame.set ("ABAprod", "none");
  frame.declare_object ("NH4_uptake", Solupt::component,
                        Attribute::OptionalState, Attribute::Singleton, "\
NH4 uptake model.\n\
By default, this will be identical to the NO3 uptake model.");
  frame.declare_object ("NO3_uptake", Solupt::component, "NO3 uptake model.");
  frame.set ("NO3_uptake", "fixed_sink");
  frame.declare ("DptEmr", "cm", Check::non_negative (), Attribute::Const,
                 "Penetration at emergence.");
  frame.set ("DptEmr", 10.0);
  frame.declare ("PenPar1", "cm/dg C/d", Check::non_negative (), Attribute::Const,
                 "Penetration rate parameter, coefficient.");
  frame.set ("PenPar1", 0.25);
  frame.declare ("PenPar2", "dg C", Check::none (), Attribute::Const,
                 "Penetration rate parameter, threshold.");
  frame.set ("PenPar2", 4.0);
  frame.declare ("PenpFFac", "pF", Attribute::None (),
                 Check::non_negative (), Attribute::Const, 
                 "Moisture dependent factor to multiply 'PenPar1' with.\n\
If pressure is less than -1 cm, pF will be assumed to be 0.");
  frame.set ("PenpFFac", PLF::always_1 ());
  frame.declare ("PenClayFac", Attribute::Fraction (), Attribute::None (),
                 Check::non_negative (), Attribute::Const, 
                 "Clay dependent factor to multiply 'PenPar1' with.");
  frame.set ("PenClayFac", PLF::always_1 ());
  frame.declare ("PenWaterFac", Attribute::Fraction (), Attribute::None (),
                 Check::non_negative (), Attribute::Const, 
                 "Water dependent factor to multiply 'PenPar1' with.\n\
The factor is a function of relative water content (Theta/Theta_sat).");
  frame.set ("PenWaterFac", PLF::always_1 ());
  frame.declare ("PenDSFac", "DS", Attribute::None (),
                 Check::non_negative (), Attribute::Const, "\
Development stage dependent factor to multiply 'PenPar1' with.");
  frame.set ("PenDSFac", PLF::always_1 ());
  frame.declare ("DensityDSFac", "DS", Attribute::None (),
                 Check::non_negative (), Attribute::Const, "\
DS based factor for effective root density.");
  frame.set ("DensityDSFac", PLF::always_1 ());
  frame.declare ("MaxPen", "cm", Check::positive (), Attribute::Const,
                 "Maximum penetration depth.");
  frame.set ("MaxPen", 100.0);
  frame.declare ("MaxWidth", "cm", Check::positive (), Attribute::OptionalConst,
                 "Maximum horizontal distance of roots from plant.");
  frame.declare ("Rad", "cm", Check::positive (), Attribute::Const,
                 "Root radius.");
  frame.set ("Rad", 0.005);
  frame.declare ("h_wp", "cm", Check::none (), Attribute::Const,
                 "Matrix potential at wilting point.");
  frame.set ("h_wp",-15000.0);
  frame.declare ("MxNH4Up", "g/cm/h", Check::non_negative (), Attribute::Const,
                 "Maximum NH4 uptake per unit root length.");
  frame.set ("MxNH4Up", 2.5e-7);
  frame.declare ("MxNO3Up", "g/cm/h", Check::non_negative (), Attribute::Const,
                 "Maximum NO3 uptake per unit root length.");
  frame.set ("MxNO3Up", 2.5e-7);
  frame.declare ("Rxylem", Attribute::None (), Check::non_negative (), Attribute::Const,
                 "Transport resistence in xyleme.");
  frame.set ("Rxylem", 10.0);

  frame.declare ("PotRtDpt", "cm", Check::non_negative (), Attribute::OptionalState,
                 "Potential root penetration depth.");
  frame.declare ("Depth", "cm", Check::non_negative (), Attribute::OptionalState,
                 "Rooting Depth.");
  frame.declare ("Density", "cm/cm^3", Check::non_negative (),
                 Attribute::OptionalState, Attribute::SoilCells,
                 "Root density in soil layers.");
  frame.declare ("EffectiveDensity", "cm/cm^3", Check::non_negative (),
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Effective root density in soil layers.\n\
This takes heterogeneous distribution of roots in soil into account.");
  frame.declare ("H2OExtraction", "cm^3/cm^3/h", Check::non_negative (), 
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Extraction of H2O in soil layers.");
  frame.declare ("NH4Extraction", "g N/cm^3/h", Check::non_negative (), 
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Extraction of NH4-N in soil layers.");
  frame.declare ("NO3Extraction", "g N/cm^3/h", Check::non_negative (), 
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Extraction of NO3-N in soil layers.");
  frame.declare ("ABAExtraction", "g/cm^3/h", Check::non_negative (), 
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Extraction of ABA in soil layers.");
  frame.declare ("ABAConc", "g/cm^3", Check::non_negative (), 
                 Attribute::State, "ABA concentration in water uptake.");
  frame.set ("ABAConc", 0.0);
  frame.declare ("h_x", "cm", Check::none (), Attribute::State,
                 "Root extraction at surface.");
  frame.set ("h_x", 0.0);
  frame.declare ("partial_soil_temperature", "dg C h", Attribute::State,
                 "Soil temperature hours this day, so far.");
  frame.set ("partial_soil_temperature", 0.0);
  frame.declare ("partial_day", "h", Attribute::State,
                 "Hours we have accumulated soil temperature this day.");
  frame.set ("partial_day", 0.0);
  frame.declare ("soil_temperature", "dg C", Attribute::State,
                 "Average soil temperature yesterday.");
  frame.set ("soil_temperature", 0.0);
  frame.declare ("water_stress", Attribute::None (), Check::fraction (),
                 Attribute::LogOnly,
                 "Fraction of requested water we didn't get.");
  frame.declare ("water_stress_days", "d", Check::non_negative (),
                 Attribute::State,
                 "Number of days production has halted due to water stress.\n\
This is the sum of water stress for each hour, multiplied with the\n\
fraction of the radition of that day that was received that hour.");
  frame.set ("water_stress_days", 0.0);
  frame.declare ("production_stress", Attribute::None (), Check::fraction (), 
                 Attribute::LogOnly,
                 "SVAT induced stress, or -1 if not applicable.");
  frame.declare ("Ept", "mm/h", Check::none (), Attribute::LogOnly,
                 "Potential transpiration.");
  frame.declare ("H2OUpt", "mm/h", Check::non_negative (), Attribute::LogOnly,
                 "H2O uptake.");
  frame.declare ("NH4Upt", "g N/m^2/h", Check::non_negative (), Attribute::LogOnly,
                 "NH4-N uptake.");
  frame.declare ("NO3Upt", "g N/m^2/h", Check::non_negative (), Attribute::LogOnly,
                 "NO3-N uptake.");
}

static double
get_PotRtDpt (const Block& al)
{
  if (al.check ("PotRtDpt"))
    return al.number ("PotRtDpt");
  if (al.check ("Depth"))
    return al.number ("Depth");
  return al.number ("DptEmr");
}

RootSystem::RootSystem (const Block& al)
  : metalib (al.metalib ()),
    rootdens (al.check ("rootdens") 
              ? Librarian::build_item<Rootdens> (al, "rootdens")
              : NULL),
    ABAprod (Librarian::build_item<ABAProd> (al, "ABAprod")),
    NH4_uptake (al.check ("NH4_uptake")
                ? Librarian::build_item<Solupt> (al, "NH4_uptake")
                : Librarian::build_item<Solupt> (al, "NO3_uptake")),
    NO3_uptake (Librarian::build_item<Solupt> (al, "NO3_uptake")),
    PenPar1 (al.number ("PenPar1")),
    PenPar2 (al.number ("PenPar2")),
    PenpFFac (al.plf ("PenpFFac")),
    PenClayFac (al.plf ("PenClayFac")),
    PenWaterFac (al.plf ("PenWaterFac")),
    PenDSFac (al.plf ("PenDSFac")),
    DensityDSFac (al.plf ("DensityDSFac")),
    MaxPen (al.number ("MaxPen")),
    MaxWidth (al.number ("MaxWidth", MaxPen)),
    Rad (al.number ("Rad")),
    h_wp (al.number ("h_wp")),
    MxNH4Up (al.number ("MxNH4Up")),
    MxNO3Up (al.number ("MxNO3Up")),
    Rxylem (al.number ("Rxylem")),
    PotRtDpt (get_PotRtDpt (al)),
    Depth (al.check ("Depth") ? al.number ("Depth") : al.number ("DptEmr")),
    Density (al.check ("Density")
             ? al.number_sequence ("Density")
             : std::vector<double> ()),
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

static DeclareSubmodel 
root_system_submodel (RootSystem::load_syntax, "RootSystem", "\
Standard root system model.");

// root_system.C ends here.
