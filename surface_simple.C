// surface_simple.C -- Simple surface models.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2022 UCPH
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

#include "surface_simple.h"
#include "librarian.h"
#include "assertion.h"
#include "block_model.h"

// The 'simple' base model.

static struct SurfaceSimpleSyntax : DeclareBase
{
  SurfaceSimpleSyntax () 
    : DeclareBase (Surface::component, "simple", "\
Don't keep track of surface stuff.")
  { }

  void load_frame (Frame&) const
  { }
} SurfaceSimple_syntax;

// The 'const_flux' model.

struct SurfaceConstFlux : public SurfaceSimple
{
  // Content.
  const double flux;

  top_t top_type (const Geometry&, size_t edge) const
  { return forced_flux; }
  double q_top (const Geometry&, size_t edge, const double dt) const // [cm/h]
  { return flux; }
  double h_top (const Geometry& geo, size_t edge) const // [cm]
  {
    const double dt = 1.0;       // [h]
    return -q_top (geo, edge, dt) * dt; 
  }

  // Create.
  SurfaceConstFlux (const BlockModel& al)
    : SurfaceSimple (al),
      flux (al.number ("flux"))
  { }
  ~SurfaceConstFlux ()
  { }
};

static struct SurfaceConstFluxSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SurfaceConstFlux (al); }

  SurfaceConstFluxSyntax () 
    : DeclareModel (Surface::component, "const_flux", "simple", "\
Constant flux upper boundary for soil.")
  { }

  void load_frame (Frame& frame) const
  {
    frame.declare ("flux", "cm/h", Attribute::Const, "\
Flux up from soil.");
    frame.order ("flux");
  }
} SurfaceConstFlux_syntax;

// The 'none' parameterization.

static struct SurfaceNoneSyntax : public DeclareParam
{ 
  SurfaceNoneSyntax ()
    : DeclareParam (Surface::component, "none", "const_flux", "\
Zero flux upper boundary for soil.")
  { }
  void load_frame (Frame& frame) const
  { frame.set ("flux", 0.0); }
} SurfaceNone_syntax;

// The 'const_pressure' model.

struct SurfaceConstPressure : public SurfaceSimple
{
  // Content.
  const double pressure;

  top_t top_type (const Geometry&, size_t edge) const
  { return forced_pressure; }
  double q_top (const Geometry& geo, size_t edge,
		const double dt) const // [cm/h]
  { return h_top (geo, edge) / dt; }
  double h_top (const Geometry&, size_t edge) const // [cm]
  { return pressure; }

  // Create.
  SurfaceConstPressure (const BlockModel& al)
    : SurfaceSimple (al),
      pressure (al.number ("pressure"))
  { }
  ~SurfaceConstPressure ()
  { }
};

static struct SurfaceConstPressureSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SurfaceConstPressure (al); }

  SurfaceConstPressureSyntax () 
    : DeclareModel (Surface::component, "const_pressure", "simple", "\
Constant pressure upper boundary for soil.")
  { }

  void load_frame (Frame& frame) const
  {
    frame.declare ("pressure", "cm", Attribute::Const, "\
Soil upper boundary pressure.");
    frame.order ("pressure");
  }
} SurfaceConstPressure_syntax;

// surface_simple.C ends here.
