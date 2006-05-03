// movement_1D.C --- Movement in a 1D system.
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

#include "movement.h"
#include "geometry1d.h"
#include "soil_water1d.h"
#include "soil_heat1d.h"
#include "soltrans1d.h"
#include "log.h"
#include "submodeler.h"

struct Movement1D : public Movement
{
  // Water.
  std::auto_ptr<Geometry1D> geometry;
  std::auto_ptr<SoilWater1D> water;
  std::auto_ptr<SoilHeat1D> heat;
  std::auto_ptr<Soltrans1D> solute;

  // Simulation.
  void output (Log& log) const
  { 
    output_submodule (*water, "SoilWater", log);
    output_submodule (*heat, "SoilHeat", log);
  }

  // Create.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  Movement1D (Block& al)
    : Movement (al),
      geometry (submodel<Geometry1D> (al, "Geometry")),
      water (submodel<SoilWater1D> (al, "Water")),
      heat (submodel<SoilHeat1D> (al, "Heat")),
      solute (submodel<Soltrans1D> (al, "Solute"))
  { }
};

void 
Movement1D::load_syntax (Syntax& syntax, AttributeList& alist)
{
   syntax.add_submodule ("Geometry", alist, Syntax::State,
                         "Discretization of the soil.",
                         Geometry1D::load_syntax);
   syntax.add_submodule ("Water", alist, Syntax::State,
                         "Soil water content and transportation.",
                         SoilWater1D::load_syntax);
   syntax.add_submodule ("Heat", alist, Syntax::State,
                         "Soil heat and flux.",
                         SoilHeat1D::load_syntax);
   syntax.add_submodule ("Solute", alist, Syntax::State,
                         "Solute transport in soil.",
                         Soltrans1D::load_syntax);
}

const AttributeList& 
Movement::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      Movement1D::load_syntax (dummy, alist);
      alist.add ("type", "1D");
    }
  return alist;
}

static struct Movement1DSyntax
{
  static Movement& make (Block& al)
  { return *new Movement1D (al); }

  Movement1DSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "One dimensional movement.");
    Movement1D::load_syntax (syntax, alist);
 
    Librarian<Movement>::add_type ("1D", alist, syntax, &make);
  }
} Movement1D_syntax;
