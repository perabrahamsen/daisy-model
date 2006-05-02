// groundwater_lysimeter.C
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


#include "groundwater.h"
#include "geometry1d.h"

class GroundwaterLysimeter : public Groundwater
{
  // Content.
  double location;                // Location of lysimeter.

  // Groundwater.
public:
  bottom_t bottom_type () const
  { return lysimeter; }
  bool accept_bottom (double)
  { return true; }
  double q_bottom () const
  { daisy_assert (false); }
  bool is_lysimeter () const
  { return true; }

  // Simulation.
public:
  void tick (const Geometry1D&, const Soil&, SoilWater&, double, 
	     const SoilHeat&, const Time&, Treelog&)
  { }
  double table () const
  { return location; }

  // Create and Destroy.
public:
  void initialize (const Geometry1D& geo, const Time&, Treelog&)
  { location = geo.zplus (geo.cell_size () - 1); }

  GroundwaterLysimeter (Block& al)
    : Groundwater (al),
      location (-42.42e42)
  { }
  ~GroundwaterLysimeter ()
  { }
};

static struct GroundwaterLysimeterSyntax
{
  static Groundwater& make (Block& al)
  { return *new GroundwaterLysimeter (al); }

  GroundwaterLysimeterSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("descriptions", "Lysimeter bottom.");
    Groundwater::load_syntax (syntax, alist);
    Librarian<Groundwater>::add_type ("lysimeter", alist, syntax, &make);
  }
} GroundwaterLysimeter_syntax;


