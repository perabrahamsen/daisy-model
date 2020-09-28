// reaction_shoot.C -- Shoot.
// 
// Copyright 2018 KU.
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

#include "reaction.h"
#include "log.h"
#include "plf.h"
#include "vegetation.h"
#include "chemistry.h"
#include "chemical.h"
#include "assertion.h"
#include "mathlib.h"
#include "treelog.h"
#include "block_model.h"
#include "librarian.h"
#include "rate.h"
#include "vcheck.h"
#include "crop.h"
#include "check.h"
#include <sstream>

// The 'shoot' reaction model.

struct ReactionShoot : public Reaction
{
  const symbol chemical;
  const symbol crop;
  const double fill_rate;	// [h^-1]
  const PLF M_max;		// [g/cm^2] -> [g/cm^2]
  const PLF DS_factor;		// [DS] -> []

      // Simulation.
  void tick_top (const Vegetation& vegetation, const Bioclimate&,
		 const double, const double, const double,
		 OrganicMatter&, Chemistry& chemistry,
		 const double dt, Treelog&)
  {
    const double height = 0.0;	// [cm]
    const double DM		// [g/cm^2]
      = vegetation.DM_by_name (crop, height) // [kg/ha]
      * 1000.0 /* [kg/g] */
      / (10000.0 * 10000.0) /* [cm^2/ha] */;
    const double DS = vegetation.DS_by_name (crop);
    const double my_M_max = M_max (DM) * DS_factor (DS); // [g/cm^2]
    daisy_assert (std::isfinite (DM));
    daisy_assert (std::isfinite (DS));
    daisy_assert (std::isfinite (M_max (DM)));
    daisy_assert (std::isfinite ( DS_factor (DS)));
    if (my_M_max < 1e-99 || fill_rate < 1e-99)
      return;
    daisy_assert (std::isfinite (my_M_max));
    daisy_assert (my_M_max > 0.0);
    Chemical& my_chemical = chemistry.find (chemical);
    const double M0 = my_chemical.canopy_storage_amount ();
    daisy_assert (std::isfinite (M0));
    // Solution to 
    //   dM/dt = fill_rate * (M_max - M)
    // is
    //   M (t) = M_max + C1 * exp (- fill_rate * t)
    // insert M (t=0) = M0 to find C1
    //   M0 = M_max + C1 * exp (0) 
    //   => C1 = M0 - M_max
    const double C1 = M0 - my_M_max; // []
    daisy_assert (std::isfinite (C1));
    daisy_assert (std::isfinite (fill_rate));
    daisy_assert (std::isfinite (dt));
    const double M1		// [g/cm^2]
      = my_M_max + C1 * std::exp (- fill_rate * dt); 
    daisy_assert (std::isfinite (M1));
    const double dM = M1 - M0; // [g/cm^2]
    daisy_assert (std::isfinite (dM));
    my_chemical.add_to_canopy_transform_source (dM / dt); // [g/cm^2/h]
    
  }
    
  void output (Log&) const
  { }

  // Create.
  void initialize (const Geometry&, 
                   const Soil&, const SoilWater&,
                   const SoilHeat&, const OrganicMatter&,
		   const Surface&, Treelog&)
  { }

  bool check (const Geometry&, 
              const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const OrganicMatter&,
	      const Chemistry& chemistry, Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (chemical))
      {
	msg.error ("Shoot requires '" + chemical.name () + "' to be tracked");
	ok = false;
      }
    return ok;
  }    
  explicit ReactionShoot (const BlockModel& al)
    : Reaction (al),
      chemical (al.name ("chemical")),
      crop (al.name ("crop")),
      fill_rate (Rate::value (al, "fill")),
      M_max (al.plf ("M_max")),
      DS_factor (al.plf ("DS_factor"))
  {
    daisy_assert (fill_rate >= 0.0);
  }
};

static struct ReactionShootSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionShoot (al); }
  ReactionShootSyntax ()
    : DeclareModel (Reaction::component, "shoot", "Pool based on shoot DM.\n\
Generate chemical (M) on canopy up to maximum (M_max).\n\
M_max depends on the shoot DM and DS.\n\
dM/dt = fill_rate * (M_max - M).")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("chemical", Attribute::Const, "\
Chemical to generate.");
    static VCheck::InLibrary is_chemical (Chemical::component);
    frame.set_check ("chemical", is_chemical);
    frame.declare_string ("crop", Attribute::Const, "\
Crop whose shoot is used as basis.\n\
Special value 'all' means all crops on the field.");
    frame.set ("crop", Vegetation::all_crops ());
    frame.set_check ("crop", Crop::check_all ());
    Rate::declare (frame, "fill", "Pool fill rate.");
    frame.declare ("M_max", "g/cm^2", "g/cm^2", Check::non_negative (),
		   Attribute::Const, "\
Maximum amount of chemical per shoot DM.");
    frame.declare ("DS_factor", "DS", Attribute::None (),
		   Check::non_negative (), Attribute::Const, "\
Influence of DS on M_max.");
    frame.set ("DS_factor", PLF::always_1 ());
  }
} ReactionShoot_syntax;

// The 'shoot2' reaction model.

struct ReactionShoot2 : public Reaction
{
  // Parameters.
  const symbol chemical;
  const symbol crop;
  const double fill_rate;	// [h^-1]
  const PLF M_max;		// [g/cm^2] -> [g/cm^2]
  const PLF DS_factor;		// [DS] -> []

  // State.
  double M_max_0;		// Potential [g/cm^2]

  // Simulation.
  void tick_top (const Vegetation& vegetation, const Bioclimate&,
		 const double, const double, const double,
		 OrganicMatter&, Chemistry& chemistry,
		 const double dt, Treelog&)
  {
    const double height = 0.0;	// [cm]
    const double DM1		// [g/cm^2]
      = vegetation.DM_by_name (crop, height) // [kg/ha]
      * 1000.0 /* [kg/g] */
      / (10000.0 * 10000.0) /* [cm^2/ha] */;
    const double DS1 = vegetation.DS_by_name (crop);
    const double M_max_1 = M_max (DM1) * DS_factor (DS1); // [g/cm^2]
    if (M_max_1 < 1e-99 || fill_rate < 1e-99)
      return;


    daisy_assert (std::isfinite (M_max_1));
    daisy_assert (M_max_1 > 0.0);
    Chemical& my_chemical = chemistry.find (chemical);
    const double M0 = my_chemical.canopy_storage_amount ();
    daisy_assert (std::isfinite (M0));

    const double growth = std::max (M_max_1 - M_max_0, 0.0); // [g/cm^2]

    M_max_0 = M_max_1;		// Save for next timestep.

    if (M0 + growth > M_max_1)
      {
	// PTA limited by potential.
	const double S = (M_max_1 - M0) / dt; // [g/cm^2/h]
	my_chemical.add_to_canopy_transform_source (S); 
	return;
      }

    // PTA product of growth and fill rates.
    const double growth_rate = growth / dt; // [g/cm^2/h]

    // dM/dt = growth_rate + fill_rate * (M_max_1 - M (t))
    // Solution
    // M (t) = C1 * exp (- fill_rate * t) + growth_rate / fill_rate + M_max_1
    // insert M (t=0) = M0 to find C1
    // M0 = C1 + growth_rate / fill_rate + M_max_1
    // C1 = M0 - growth_rate / fill_rate - M_max_1
    const double C1 = M0 - growth_rate / fill_rate- M_max_1; // [g/cm^2]
    daisy_assert (std::isfinite (C1));
    daisy_assert (std::isfinite (fill_rate));
    daisy_assert (std::isfinite (dt));
    const double M1		// [g/cm^2]
      = C1 * std::exp (- fill_rate * dt) + growth_rate / fill_rate + M_max_1;  
    daisy_assert (std::isfinite (M1));
    const double dM = M1 - M0; // [g/cm^2]
    daisy_assert (std::isfinite (dM));
    const double S = dM / dt;	// [g/cm^2/h]
    my_chemical.add_to_canopy_transform_source (S); 
  }
    
  void output (Log& log) const
  {
    output_value (M_max_0, "M_pot", log);
  }

  // Create.
  void initialize (const Geometry&, 
                   const Soil&, const SoilWater&,
                   const SoilHeat&, const OrganicMatter&,
		   const Surface&, Treelog&)
  { }

  bool check (const Geometry&, 
              const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const OrganicMatter&, const Chemistry& chemistry,
	      Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (chemical))
      {
	msg.error ("Shoot2 requires '" + chemical.name () + "' to be tracked");
	ok = false;
      }
    return ok;
  }    
  explicit ReactionShoot2 (const BlockModel& al)
    : Reaction (al),
      chemical (al.name ("chemical")),
      crop (al.name ("crop")),
      fill_rate (Rate::value (al, "fill")),
      M_max (al.plf ("M_max")),
      DS_factor (al.plf ("DS_factor")),
      M_max_0 (al.number ("M_pot"))
  {
    daisy_assert (fill_rate >= 0.0);
  }
};

static struct ReactionShoot2Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionShoot2 (al); }
  ReactionShoot2Syntax ()
    : DeclareModel (Reaction::component, "shoot2", "Pool based on shoot DM.\n\
Generate chemical (M) on canopy up to maximum (M_max).\n\
M_max depends on the shoot DM and DS.\n\
\n\
dM/dt = growth_rate + fill_rate * (M_max - M).\n\
\n\
growth_rate = max (0.0, (M_max (t1) - M_max (t0)) / (t1 - t0))")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "jorgensen2020novel");
    frame.declare_string ("chemical", Attribute::Const, "\
Chemical to generate.");
    static VCheck::InLibrary is_chemical (Chemical::component);
    frame.set_check ("chemical", is_chemical);
    frame.declare_string ("crop", Attribute::Const, "\
Crop whose shoot is used as basis.\n\
Special value 'all' means all crops on the field.");
    frame.set ("crop", Vegetation::all_crops ());
    frame.set_check ("crop", Crop::check_all ());
    Rate::declare (frame, "fill", "Pool fill rate.");
    frame.declare ("M_max", "g/cm^2", "g/cm^2", Check::non_negative (),
		   Attribute::Const, "\
Maximum amount of chemical per shoot DM.");
    frame.declare ("DS_factor", "DS", Attribute::None (),
		   Check::non_negative (), Attribute::Const, "\
Influence of DS on M_max.");
    frame.set ("DS_factor", PLF::always_1 ());
    frame.declare ("M_pot", "g/cm^2", Check::non_negative (),
		   Attribute::State,
		   "Potential chemical content.");
    frame.set ("M_pot", 0.0);
  }
} ReactionShoot2_syntax;

// reaction_shoot.C ends here.
