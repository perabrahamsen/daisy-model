// uzrect_const.C --- Steady-state water flow
// 
// Copyright 2006, 2007 Mikkel Mollerup, Per Abrahamsen and KVL.
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
#include "uzrect.h"
#include "geometry_rect.h"
#include "soil_water.h"
#include "block_model.h"
#include "frame.h"
#include "mathlib.h"
#include "assertion.h"
#include "librarian.h"
 

struct UZRectConst : public UZRect
{
  const double q_x;             //[mm/h] horizontal flow
  const double q_z;             //[mm/h] vertical flow
 
  // Interface.
  bool obey_surface ()
  { return false; }
  void tick (const GeometryRect&, const std::vector<size_t>& drain_cell,
	     const double drain_water_level,
	     const Soil&, SoilWater&, const SoilHeat&, 
             const Surface&, const Groundwater&,
             double dt, Treelog&);
  void output (Log&) const;
  
 
  // Create and Destroy.
  void initialize (const Geometry& geo, const bool has_macropores);
  UZRectConst (const BlockModel& al);
  ~UZRectConst ();
};





void 
UZRectConst::tick (const GeometryRect& geo, const std::vector<size_t>&,
		   const double,
                   const Soil&, 
                   SoilWater& soil_water, const SoilHeat&,
                   const Surface&, const Groundwater&, 
                   const double, Treelog&)

{
  const size_t edge_size = geo.edge_size (); // number of edges 

  for (size_t edge = 0; edge != edge_size; ++edge) 
    {
      const double sin_angle = geo.edge_sin_angle (edge);
      const double cos_angle = geo.edge_cos_angle (edge);
      const double q = q_z * sin_angle + q_x * cos_angle;
      soil_water.set_flux (edge, q);
    }
}

void
UZRectConst::output (Log&) const 
{ }


void 
UZRectConst::initialize (const Geometry&, const bool)
{ }

UZRectConst::UZRectConst (const BlockModel& al)
  : UZRect (al),
    q_x (al.number ("q_x")),
    q_z (al.number ("q_z"))
{ }

UZRectConst::~UZRectConst ()
{ }

static struct UZRectConstSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UZRectConst (al); }
  UZRectConstSyntax ()
    : DeclareModel (UZRect::component, "const", "\
Steady-state water flow.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("q_x", "cm/h", Attribute::Const, "\
Horizontal flow.");
    frame.set ("q_x", 0.0);
    frame.declare ("q_z", "cm/h", Attribute::Const, "\
Vertical flow upwards.");
    frame.set ("q_z", 0.0); 
  }
} UZRectConst_syntax;

// uzrect_const.C ends here.
