// uzrect_Mollerup.C --- A 2D solution to Richard's equation in a rect. grid.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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
#include "groundwater.h"
#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "mathlib.h"
#include "assertion.h"
#include <sstream>

struct UZRectMollerup : public UZRect
{
  // Parameters.

  // Interface.
  void tick (const GeometryRect&, const Soil&, SoilWater&, 
             const SoilHeat&, Surface& surface, Groundwater& groundwater, 
             Treelog&);

  // Internal functions.

  // Create and Destroy.
  void has_macropores (bool);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  UZRectMollerup (Block& al);
  ~UZRectMollerup ();
};

void 
UZRectMollerup::tick (const GeometryRect& geo, const Soil& soil, 
                 SoilWater& soil_water, const SoilHeat& soil_heat,
                 Surface& surface, Groundwater& groundwater, Treelog& msg)
{
  const size_t edge_size = geo.edge_size ();
  const size_t cell_rows = geo.cell_rows ();
  const size_t cell_columns = geo.cell_columns ();
  const size_t edge_rows = geo.edge_rows ();

  // Insert magic here.

  // Update surface and groundwater reservoirs.
  for (size_t edge = 0; edge < edge_size; edge++)
    {
      if (geo.edge_to (edge) == Geometry::cell_above)
        surface.accept_top (soil_water.q (edge) * dt, geo, edge, msg);
      if (geo.edge_from (edge) == Geometry::cell_below)
        groundwater.accept_bottom ((soil_water.q (edge)
                                    + soil_water.q_p (edge)) * dt,
                                   geo, edge);
    }
}

void 
UZRectMollerup::has_macropores (const bool)
{ /* Ignore for now. */ }

void 
UZRectMollerup::load_syntax (Syntax&, AttributeList&)
{ }

UZRectMollerup::UZRectMollerup (Block& al)
  : UZRect (al)
{ }

UZRectMollerup::~UZRectMollerup ()
{ }

const AttributeList& 
UZRect::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      UZRectMollerup::load_syntax (dummy, alist);
      alist.add ("type", "Mollerup");

    }
  return alist;
}

static struct UZRectMollerupSyntax
{
  static UZRect& make (Block& al)
  { return *new UZRectMollerup (al); }
  UZRectMollerupSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
An finite volume solution to matrix water transport.\n\
See Mollerup, 2007 for details.");
    UZRectMollerup::load_syntax (syntax, alist);
    Librarian<UZRect>::add_type ("Mollerup", alist, syntax, &make);
  }
} UZRectMollerup_syntax;

// uzrect_Mollerup.C ends here.
