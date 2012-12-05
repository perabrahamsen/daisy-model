// groundwater_flux.C --- Forced flux lower boundary.
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

#include "groundwater.h"
#include "block_model.h"
#include "check.h"
#include "librarian.h"
#include "frame.h"

class GroundwaterFlux : public Groundwater
{
  // Groundwater.
  const double flux;

public:
  bottom_t bottom_type() const
  { return forced_flux; }
  double q_bottom (size_t) const
  { return flux; }

  // Simulation.
public:
  void tick (const Geometry&,
             const Soil&, SoilWater&, double, 
	     const SoilHeat&, const Time&, const Scope&, Treelog&)
  { }
  double table () const
  { return 42.42e42; }

  // Create and Destroy.
public:
  void initialize (const Geometry&, const Time&, const Scope&, Treelog&)
  { }
  bool check (const Geometry&, const Scope&, Treelog&) const
  { return true; }
  GroundwaterFlux (const BlockModel& al)
    : Groundwater (al),
      flux (al.number ("flux"))
  { }
  ~GroundwaterFlux ()
  { }
};

static struct GroundwaterFluxSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GroundwaterFlux (al); }

  GroundwaterFluxSyntax ()
    : DeclareModel (Groundwater::component, "flux", "common", "\
Flux groundwater, free drainage.")
  { }
  void load_frame (Frame& frame) const
    { 
      frame.declare ("flux", "cm/h", Check::none (), Attribute::Const,
		  "Constant flux to groundwater.");
      frame.order ("flux");
    }
} GroundwaterFlux_syntax;
