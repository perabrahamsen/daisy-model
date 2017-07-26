// svat_none.C -- No production stress.
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

#include "svat.h"
#include "bioclimate.h"
#include "librarian.h"
#include "frame.h"
#include "weather.h"

struct SVAT_none : public SVAT
{
  double crop_ea;
  double T_a;
  double e_c;

  // Simulation.
  void tick (const Weather& weather, const Vegetation&,
	     const Geometry&, const Soil&, const SoilHeat&, double,
	     const SoilWater&, const Bioclimate& bio, const Movement&,
	     double, double, double, Treelog&)
  { 
    crop_ea = bio.crop_ea();
    T_a = weather.air_temperature ();
    e_c = weather.vapor_pressure ();
  }
  double production_stress () const
  { return -1; }
  
  void solve(const double, const double, Treelog&)  
  { }

  double transpiration() const 
  { return crop_ea; }

  double CanopyTemperature () const
  { return T_a; }  // [dg C]

  double SunLeafTemperature () const
  { return T_a; }  // [dg C]

  double ShadowLeafTemperature () const
  { return T_a; }  // [dg C]

  double CanopyVapourPressure () const
  { return e_c; }               // [Pa]

  double SunBoundaryLayerWaterConductivity () const
  {
    // Leave it to the bioclimate module.
    return -1.0; 
  }

  double ShadowBoundaryLayerWaterConductivity () const
  { 
    // Leave it to the bioclimate module.
    return -1.0; 
  }

  double SoilSurfaceTemperature () const
  { return T_a; }  // [dg C]

  // Create.
  bool check (const Weather&, Treelog&) const
  { return true; }
  SVAT_none (const BlockModel& al)
    : SVAT (al)
  { }
  void summarize (Treelog&) const
  { }
};

static struct SVAT_NoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SVAT_none (al); }
  SVAT_NoneSyntax ()
    : DeclareModel (SVAT::component, "none", "No SVAT in effect.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} SVAT_none_syntax;
