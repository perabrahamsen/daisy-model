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

#include "uzrect.h"
#include "geometry_rect.h"
#include "soil_water.h"
#include "syntax.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "assertion.h"
#include "librarian.h"
 



struct UZRectConst : public UZRect
{
  const double q_x;             //[mm/h] horizontal flow
  const double q_z;             //[mm/h] verticla flow
 


  // Interface.
  void tick (const GeometryRect&, std::vector<size_t>& drain_cell,
	     const Soil&, SoilWater&, const SoilHeat&, 
             const Surface&, const Groundwater&, double dt, Treelog&);
  void output (Log&) const;
  
 
  // Create and Destroy.
  void has_macropores (Block&, bool);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  UZRectConst (Block& al);
  ~UZRectConst ();
};



void 
UZRectConst::tick (const GeometryRect& geo, std::vector<size_t>&,
		      const Soil&, 
                      SoilWater& soil_water, const SoilHeat&,
                      const Surface&, const Groundwater&,
                      const double,
                      Treelog&)

{
  const size_t edge_size = geo.edge_size (); // number of edges 

  for (size_t edge = 0; edge != edge_size; ++edge) 
    {
      const double sin_angle = geo.edge_sin_angle (edge);
      const double cos_angle = sqrt (1.0 - sqr (sin_angle));
      const double q = q_z * sin_angle + q_x * cos_angle;
      soil_water.set_flux (edge, q);
    }
}

void
UZRectConst::output (Log&) const 
{ }

void 
UZRectConst::has_macropores (Block&, const bool)
{ /* Ignore for now. */ }

void 
UZRectConst::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("q_x", "mm/h", Syntax::Const, "\
Horizontal flow.");
  alist.add ("q_x", 0.0);
  syntax.add ("q_z", "mm/h", Syntax::Const, "\
Vertical flow upwards.");
  alist.add ("q_z", 0.0); 
}


UZRectConst::UZRectConst (Block& al)
  : UZRect (al),
    q_x (al.number ("q_x")),
    q_z (al.number ("q_z"))
{ }

UZRectConst::~UZRectConst ()
{ }


static struct UZRectConstSyntax
{
  static Model& make (Block& al)
  { return *new UZRectConst (al); }
  UZRectConstSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Steady-state water flow.");
    UZRectConst::load_syntax (syntax, alist);
    Librarian::add_type (UZRect::component, "const", alist, syntax, &make);
  }
} UZRectConst_syntax;

// uzrect_const.C ends here.
