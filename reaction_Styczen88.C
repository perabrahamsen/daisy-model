// reaction_Styczen88.C -- Colloids generation based in rainfall momentum.
// 
// Copyright 2009 Per Abrahamsen and KU
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "reaction_colgen.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block_model.h"
#include "librarian.h"
#include "chemistry.h"
#include "chemical.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "treelog.h"
#include "frame.h"
#include "plf.h"
#include "vegetation.h"
#include "bioclimate.h"
#include <memory>

struct ReactionStyczen88 : public ReactionColgen
{
  // Constants.
  /* const */ double surface_soil; // Soil in mixing layer. [g/cm^2]

  // Parameters.
  const double Ae;              // Soil resitance factor [h^2/g C/cm^2 S]
  const double fixed_MA;      // Mulch factor (protective coverage) []
  const double droplet_diameter; // Size of vegetation droplets. [mm]

  // Log variable.
  double DH;                    // Squaret droplet momentum [g^2 C/cm/h]
  double CM;                    // Vegetation factor []
  double MR;                    // Squared direct rainfall momentum [g^2 C/h^3]

  // Simulation.
  static double find_DH_2_below (const double h_veg, 
                                 const double droplet_diameter);
  static double find_DH_2_13 (const double h_veg, 
                              const double droplet_diameter);
  static double find_DH_13_above (const double /* h_veg */, 
                                  const double droplet_diameter);
  static double find_DH (const double h_veg, const double droplet_diameter);
  void colloid_generation (const double P /* [mm/h] */,
                           const double LD /* [mm/h] */,
                           const double f_cov /* [] */,
                           const double litter_cover /* [] */,
                           const double h_veg /* [m] */,
                           const double h_pond /* [mm] */,
                           const double dt /* [h] */);
  void  tick_top (const Vegetation&, const Bioclimate&,
		  const double tillage_age /* [d] */,
                  const double total_rain, 
                  const double h_pond,
                  OrganicMatter&, Chemistry& chemistry,
		  const double dt, Treelog&);
                           
  void output (Log& log) const;

  // Create and Destroy.
  void initialize (const Geometry& geo,
                   const Soil& soil, const SoilWater&, const SoilHeat&, 
                   const OrganicMatter&, const Surface&, Treelog&);
  ReactionStyczen88 (const BlockModel& al);
};

double
ReactionStyczen88::find_DH_2_below (const double h_veg, 
                                    const double droplet_diameter)
{
  static const struct B : public PLF
  { 
    B ()
    {
      add (4.5, 0.7954);
      add (5.0, 1.1058);
      add (5.5, 1.4916);
      add (6.0, 1.9601);
    };
  } b;

  return b (droplet_diameter) * h_veg;
}

double
ReactionStyczen88::find_DH_2_13 (const double h_veg, 
                                 const double droplet_diameter)
{
  static const struct A : public PLF
  { 
    A ()
    {
      add (4.5, -0.5);
      add (5.0, -0.5);
      add (5.5, -0.5);
      add (6.0, -0.5);
    };
  } a_;

  static const struct B : public PLF
  { 
    B ()
    {
      add (4.5, 1.2031);
      add (5.0, 1.5930);
      add (5.5, 2.0692);
      add (6.0, 2.5496);
    };
  } b_;

  static const struct C : public PLF
  { 
    C ()
    {
      add (4.5, -0.12416);
      add (5.0, -0.15954);
      add (5.5, -0.20184);
      add (6.0, -0.23976);
    };
  } c_;

  static const struct D : public PLF
  { 
    D ()
    {
      add (4.5, 4.33e-3);
      add (5.0, 5.44e-3);
      add (5.5, 6.70e-3);
      add (6.0, 7.68e-3);
    };
  } d_;

  const double a = a_ (droplet_diameter);
  const double b = b_ (droplet_diameter);
  const double c = c_ (droplet_diameter);
  const double d = d_ (droplet_diameter);
  const double h = h_veg;
    
  return a + b * h + c * h * h + d * h * h * h;
}

double
ReactionStyczen88::find_DH_13_above (const double /* h_veg */, 
                                     const double droplet_diameter)
{
  static const struct A : public PLF
  { 
    A ()
    {
      add (4.5, 3.8647);
      add (5.0, 5.4080);
      add (5.5, 7.2934);
      add (6.0, 9.5310);
    }
  } a;

  return a (droplet_diameter);
}

double
ReactionStyczen88::find_DH (const double h_veg, const double droplet_diameter)
{
  if (h_veg < 2.0)
    return find_DH_2_below (h_veg, droplet_diameter);
  if (h_veg < 13.0)
    return find_DH_2_13 (h_veg, droplet_diameter);
  
  return find_DH_13_above (h_veg, droplet_diameter);
}

void
ReactionStyczen88::colloid_generation (const double P /* [mm/h] */,
                                       const double canopy_leak /* [mm/h] */,
                                       const double f_cov /* [] */,
				       const double litter_cover /* [] */,
                                       const double h_veg /* [m] */,
                                       const double h_pond /* [mm] */,
                                       const double dt /* [h] */)
{
  // Direct rainfall momentum
  if (P <= 75)
    MR = 2.04e-8 * std::pow (P, 1.63);
  else
    MR = 4.83e-8 * std::pow (P, 1.43);

  // Droplet momentum.
  DH = find_DH (h_veg, droplet_diameter);
  daisy_assert (std::isfinite (DH));

  const double LD = canopy_leak / 3600.0 / 1000.0; // [mm/h] -> [m/s]
  
  // Vegetation modifier.
  CM = (MR > 0.0)
    ? ((1.0 - f_cov) * MR + LD * DH) / MR
    : 1.0;
  daisy_assert (std::isfinite (CM));

  const double MA = (fixed_MA < 0.0) ? litter_cover : fixed_MA;
  daisy_assert (MA >= 0.0);
  
  // Detachment of colloids at the surface. [g cm^-2 h^-1]
  D = Ae * (1.0 - MA) * KH * CM * MR; 

  daisy_assert (std::isfinite (D));
  daisy_assert (D >= 0.0);

  surface_release = D * dt / surface_soil;
}

void 
ReactionStyczen88::tick_top (const Vegetation& vegetation,
			     const Bioclimate& bioclimate,
			     const double /* tillage_age */,
                             const double total_rain, 
                             const double h_pond,
                             OrganicMatter&,
			     Chemistry& chemistry, const double dt, Treelog&)
{
  const double litter_cover = bioclimate.litter_cover (); // [];
  const double canopy_cover = vegetation.cover (); // [];
  const double canopy_drip = bioclimate.canopy_leak (); // [mm/h]
  const double h_veg = vegetation.height () * 0.01 ;	 // [m]

  ReactionColgen::tick_colgen (total_rain, h_pond);

  Chemical& colloid = chemistry.find (colloid_name);
  
  const double P = total_rain; // [mm/h]
  const double LD = canopy_drip; // [mm/h]
  const double f_cov = canopy_cover; // []

  colloid_generation (P, LD, f_cov, litter_cover, h_veg, h_pond, dt);

  colloid.add_to_surface_transform_source (D);
  colloid.release_surface_colloids (surface_release);
}

void 
ReactionStyczen88::output (Log& log) const 
{
  ReactionColgen::output_colgen (log);
  output_variable (DH, log); 
  output_variable (CM, log); 
  output_variable (MR, log); 
}


void 
ReactionStyczen88::initialize (const Geometry& geo, 
			       const Soil& soil,
			       const SoilWater&, const SoilHeat&, 
			       const OrganicMatter&,
			       const Surface& surface, Treelog&)
{ surface_soil = find_surface_soil (geo, soil, surface); }

ReactionStyczen88::ReactionStyczen88 (const BlockModel& al)
  : ReactionColgen (al),
    surface_soil (-42.42e42),
    Ae (al.number ("Ae")),
    fixed_MA (al.number ("MA", -42.42e42)),
    droplet_diameter (al.number ("droplet_diameter")),
    DH (-42.42e42),
    CM (-42.42e42),
    MR (-42.42e42)
{ }

static struct ReactionStyczen88Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionStyczen88 (al); }
  ReactionStyczen88Syntax ()
    : DeclareModel (Reaction::component, "colgen_Styczen88", "colgen", "\
Colloid generation using rainfall momentum.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "styczen88");

    frame.declare ("Ae", "h^2/g/cm^2", Check::positive (), Attribute::Const, 
               "Soil resistance factor.");
    frame.declare ("MA", Attribute::Fraction (), Attribute::OptionalConst, 
               "Protective cover (mulch factor).\n\
By default, use the cover predicted by the litter model.");
    frame.declare ("droplet_diameter", "mm", Check::positive (), Attribute::Const, 
               "Size of droplets from vegetation.");
    frame.declare ("DH", "kg^2/m/s^2", Attribute::LogOnly, 
               "Squared vegetation droplet momentum.");
    frame.declare ("CM", Attribute::Fraction (), Attribute::LogOnly, 
               "Vegetation factor.");
    frame.declare ("MR", "(N s)^2/m^2/s", Attribute::LogOnly, 
               "Squared direct rainfall momentum.");
  }
} ReactionStyczen88syntax;

// reaction_Styczen88.C ends here.
