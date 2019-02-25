// soilfac.C --- Find factor associated with soil cell.
// 
// Copyright 2017 KU.
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

#include "soilfac.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "check.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "vcheck.h"

#include <sstream>

// soilfac component.

const char *const Soilfac::component = "soilfac";

symbol 
Soilfac::library_id () const
{
  static const symbol id (component);
  return id;
}

Soilfac::Soilfac ()
{ }

Soilfac::~Soilfac ()
{ }

static struct SoilfacInit : public DeclareComponent 
{
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
  SoilfacInit ()
    : DeclareComponent (Soilfac::component, "\
Find factor associated with each soil cell.")
  { }
} Soilfac_init;

// The 'FOCUS' model.

struct SoilfacFOCUS : public Soilfac
{
  const double B;
  const double alpha;
  const double T_ref;

  // Simulation.
  double value(size_t c,
	       const Geometry& geo, const Soil& soil, 
	       const SoilWater& soil_water, const SoilHeat& soil_heat, 
	       const OrganicMatter&, const Chemical&) const
  { 
    // Depth.
    const double z = geo.cell_z (c);

    // From FOCUS:
    const double depth_factor 
      = (z > -30.0)
      ? 1.0
      : ((z > -60.0)
	 ? 0.5
	 : ((z > -100.0)
	    ? 0.3
	    : 0.0));
    daisy_assert (depth_factor >= 0.0);
    daisy_assert (depth_factor <= 1.0);

    // Water.
    const double Theta = soil_water.Theta (c);
    const double h_ice = soil_water.h_ice (c);
    const double h_wp = -15000.0;
    const double h_fc = -100.0;
    const double Theta_wp_05 = 0.5 * soil.Theta (c, h_wp, h_ice);
    const double Theta_fc = soil.Theta (c, h_fc, h_ice);

    // From MACRO 5.2 changes documents:
    const double water_factor 
      = (Theta < Theta_wp_05)
      ? 0.0 
      : ((Theta < Theta_fc)
	 ? std::pow ((Theta - Theta_wp_05) / (Theta_fc - Theta_wp_05), B)
	 : 1.0);
    daisy_assert (water_factor >= 0.0);
    daisy_assert (water_factor <= 1.0);

    // Heat.
    const double T = soil_heat.T (c);
    
    // From MACRO 5.0 technical documentation:
    const double heat_factor 
      = (T < 0.0)
      ? 0.0
      : ((T > 5.0)
	 ? std::exp (alpha * (T - T_ref))
	 : 0.2 * T * std::exp (alpha * (5.0 - T_ref)));
    daisy_assert (heat_factor >= 0.0);
    daisy_assert (heat_factor <= 50.0);

    return depth_factor * water_factor * heat_factor;
  }

  // Create and Destroy.
  SoilfacFOCUS (const BlockModel& al)
    : B (al.number ("B")),
      alpha (al.number ("alpha")),
      T_ref (al.number ("T_ref"))
  { }
  ~SoilfacFOCUS ()
  { }
};

static struct SoilfacFOCUSSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SoilfacFOCUS (al); }
  SoilfacFOCUSSyntax ()
    : DeclareModel (Soilfac::component, "FOCUS", "\
Depth, moisture, and heat effect according to the FOCUS Surface Water WG.\n\
\n\
Water factor is zero below 0.5 Theta_wp, one above field capacity, and \n\
((Theta - 0.5 Theta_wp) / (Theta_fc - 0.5 Theta_wp))^B\n\
otherwise. From MACRO 5.2.\n\
\n\
Heat factor is zero below zero degrees,\n\
0.2 T e^(alpha (5 - T_ref)) below 5 dg C, and\n\
and e^(alpha (T - T_ref)) above 5 dg C. From MACRO 5.0.\n\
\n\
Depth factor is 1.0 above 30 cm, 0.5 above 60 cm, 0.3 above 100 cm,\n\
and zero below.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "focussw2002");
    frame.declare ("B", Attribute::None (), Check::none (),
		   Attribute::Const, "Soil moisture effect parameter.");
    frame.set_cited ("B", 0.49, "Section 7.4.5", "focussw2002");
    frame.declare ("T_ref", "dg C", Check::none (),
		   Attribute::Const, "Reference temperature.");
    frame.set_cited ("T_ref", 20.0, "Section 7.4.2", "focussw2002");
    frame.declare ("alpha", "K^-1", Check::none (),
		   Attribute::Const, "temperature effect parameter.");
    frame.set_cited ("alpha", 0.0948, "Section 7.4.4", "focussw2002");
  }
} SoilfacFOCUS_syntax;

// The 'Maja' model.

struct SoilfacMaja : public Soilfac
{
  const double B;
  const double alpha;
  const double T_ref;
  const std::vector<double> z;
  const std::vector<double> z_factor;

  // Simulation.
  double value(size_t c,
	       const Geometry& geo, const Soil& soil, 
	       const SoilWater& soil_water, const SoilHeat& soil_heat, 
	       const OrganicMatter&, const Chemical&) const
  { 
    // Depth.
    const double depth = geo.cell_z (c);

    double depth_factor = 0.0;
    for (size_t i = 0; i < z.size (); i++)
      if (depth > z[i])
	{
	  depth_factor = z_factor[i];
	  break;
	}

    // Water.
    const double Theta = soil_water.Theta (c);
    const double h_ice = soil_water.h_ice (c);
    const double h_wp = -15000.0;
    const double h_fc = -100.0;
    const double Theta_wp_05 = 0.5 * soil.Theta (c, h_wp, h_ice);
    const double Theta_fc = soil.Theta (c, h_fc, h_ice);

    // From MACRO 5.2 changes documents:
    const double water_factor 
      = (Theta < Theta_wp_05)
      ? 0.0 
      : ((Theta < Theta_fc)
	 ? std::pow ((Theta - Theta_wp_05) / (Theta_fc - Theta_wp_05), B)
	 : 1.0);
    daisy_assert (water_factor >= 0.0);
    daisy_assert (water_factor <= 1.0);

    // Heat.
    const double T = soil_heat.T (c);
    
    // From MACRO 5.0 technical documentation:
    const double heat_factor 
      = (T < 0.0)
      ? 0.0
      : ((T > 5.0)
	 ? std::exp (alpha * (T - T_ref))
	 : 0.2 * T * std::exp (alpha * (5.0 - T_ref)));
    daisy_assert (heat_factor >= 0.0);
    daisy_assert (heat_factor <= 50.0);

    return depth_factor * water_factor * heat_factor;
  }

  // Create and Destroy.
  SoilfacMaja (const BlockModel& al)
    : B (al.number ("B")),
      alpha (al.number ("alpha")),
      T_ref (al.number ("T_ref")),
      z (al.number_sequence ("z")),
      z_factor (al.number_sequence ("z_factor"))
  {
    daisy_assert (z.size () == z_factor.size ());
  }
  ~SoilfacMaja ()
  { }
};

static struct SoilfacMajaSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SoilfacMaja (al); }
  SoilfacMajaSyntax ()
    : DeclareModel (Soilfac::component, "Maja", "\
Depth, moisture, and heat effect according to the Maja Surface Water WG.\n\
\n\
Water factor is zero below 0.5 Theta_wp, one above field capacity, and \n\
((Theta - 0.5 Theta_wp) / (Theta_fc - 0.5 Theta_wp))^B\n\
otherwise. From MACRO 5.2.\n\
\n\
Heat factor is zero below zero degrees,\n\
0.2 T e^(alpha (5 - T_ref)) below 5 dg C, and\n\
and e^(alpha (T - T_ref)) above 5 dg C. From MACRO 5.0.\n\
\n\
Depth factor is specified by 'z' and 'z_factor'.")
  { }
  static bool check_maja (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;
    if (al.value_size ("z") != al.value_size ("z_factor"))
      {
	msg.error ("'z' and 'z_factor' must have the same number of elements");
	ok = false;
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_maja);
    frame.set_strings ("cite", "focussw2002");
    frame.declare ("B", Attribute::None (), Check::none (),
		   Attribute::Const, "Soil moisture effect parameter.");
    frame.set_cited ("B", 0.49, "Section 7.4.5", "focussw2002");
    frame.declare ("T_ref", "dg C", Check::none (),
		   Attribute::Const, "Reference temperature.");
    frame.set_cited ("T_ref", 20.0, "Section 7.4.2", "focussw2002");
    frame.declare ("alpha", "K^-1", Check::none (),
		   Attribute::Const, "temperature effect parameter.");
    frame.set_cited ("alpha", 0.0948, "Section 7.4.4", "focussw2002");
    frame.declare ("z", "cm", Check::negative (), Attribute::Const,
		   Attribute::Variable, "\
End of each interval.");
    frame.set_check ("z", VCheck::decreasing ());
    frame.declare ("z_factor", Attribute::None (), Check::non_negative (),
		   Attribute::Const, Attribute::Variable, "\
Factor to apply to decompose rate at each interval.");
  }
} SoilfacMaja_syntax;

// soilfac.C ends here.
