// ghf.C  -- Ground heat flux.
// 
// Copyright 2021 KU
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

#include "ghf.h"
#include "block_model.h"
#include "librarian.h"
#include "weather.h"
#include "soil_heat.h"

// The 'ghf' component.

const char *const GHF::component = "ghf";

GHF::GHF (const BlockModel&)
{ }

GHF::~GHF ()
{ }

static struct GHFInit : public DeclareComponent 
{
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
  GHFInit ()
    : DeclareComponent (GHF::component, "\
Ground heat flux.")
  { }
} GHF_init;

// The 'const' model.

struct GHFConst : public GHF
{
  const double G;		// [W/m^2]
  
  // Simulation.
  double value (const Geometry&,
		const Soil&, const SoilWater&, const SoilHeat&,
		const Weather&, const double, Treelog&) const // [W/m^2]
  { return G; }

  // Create.
  GHFConst (const BlockModel& al)
    : GHF (al),
      G (al.number ("G"))
  { }
};

static struct GHFConstSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GHFConst (al); }
  GHFConstSyntax ()
    : DeclareModel (GHF::component, "const", 
		    "The ground heat flux never changes.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("G", "W/m^2", Attribute::Const, "\
The ground heat flux never changes.");
    frame.order ("G");
  }
} GHFConst_syntax;

// The 'none' parameterization.

static struct GHFNoneSyntax : public DeclareParam
{
  GHFNoneSyntax ()
    : DeclareParam (GHF::component, "none", "const", "No ground heat flux.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("G", 0.0);
  }
} GHFNone_syntax;


// The 'weather' model

struct GHFWeather : public GHF
{
  // Simulation.
  double value (const Geometry&,
		const Soil&, const SoilWater&,
		const SoilHeat&,
		const Weather& weather, const double, Treelog&) const // [W/m^2]
  { return weather.ground_heat_flux (); }
  
  // Create.
  GHFWeather (const BlockModel& al)
    : GHF (al)
  { }
};

static struct GHFWeatherSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GHFWeather (al); }
  GHFWeatherSyntax ()
    : DeclareModel (GHF::component, "weather", 
		    "Use value from weather file.")
  { }
  void load_frame (Frame&) const
  { }
} GHFWeather_syntax;

// The 'old' model

struct GHFOld : public GHF
{
  // Simulation.
  double value (const Geometry& geo,
		const Soil& soil, const SoilWater& soil_water, const SoilHeat& soil_heat,
		const Weather&, const double, Treelog&) const // [W/m^2]
  { return soil_heat.top_flux (geo, soil, soil_water); }
  
  // Create.
  GHFOld (const BlockModel& al)
    : GHF (al)
  { }
};

static struct GHFOldSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GHFOld (al); }
  GHFOldSyntax ()
    : DeclareModel (GHF::component, "old", 
		    "Based soil numeric cell 0 and 1.\n\
The model used in Daisy 6.08 and earlier, should not be used.")
  { }
  void load_frame (Frame&) const
  { }
} GHFOld_syntax;

// The 'surface' model

struct GHFSurface : public GHF
{
  // Simulation.
  double value (const Geometry& geo,
		const Soil& soil, const SoilWater& soil_water, const SoilHeat& soil_heat,
		const Weather&, const double, Treelog&) const // [W/m^2]
  { return soil_heat.top_flux (geo); }
  
  // Create.
  GHFSurface (const BlockModel& al)
    : GHF (al)
  { }
};

static struct GHFSurfaceSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GHFSurface (al); }
  GHFSurfaceSyntax ()
    : DeclareModel (GHF::component, "surface", 
		    "Based on flux through soil surface.\n\
Calculated from previous timestep.")
  { }
  void load_frame (Frame&) const
  { }
} GHFSurface_syntax;

// The 'FAO56' model

struct GHFFAO56 : public GHF
{
  // Simulation.
  double value (const Geometry&,
		const Soil&, const SoilWater&, const SoilHeat&,
		const Weather& weather, const double Rn_ref,
		Treelog&) const // [W/m^2]
  {
    const bool is_day = (weather.sin_solar_elevation_angle () > 0.0);
    return is_day
      ? (0.1 * Rn_ref)		// Eq 45.
      : (0.5 * Rn_ref);		// Eq 46.
  }  
  // Create.
  GHFFAO56 (const BlockModel& al)
    : GHF (al)
  { }
};

static struct GHFFAO56Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GHFFAO56 (al); }
  GHFFAO56Syntax ()
    : DeclareModel (GHF::component, "FAO56", "Equation 45 and 46.\n\
Ground heat flux is 10 % of Rn during day, and 50 % of Rn during night.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "FAO-PM");
  }
} GHFFAO56_syntax;


// ghf.C ends here.
