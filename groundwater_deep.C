// groundwater_deep.C --- Free drainage.
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
#include "syntax.h"
#include "alist.h"
#include "assertion.h"

class GroundwaterDeep : public Groundwater
{
  // Groundwater.
public:
  bottom_t bottom_type() const
  { return free_drainage; }
  double q_bottom () const
  { daisy_notreached (); }

  // Simulation.
public:
  void tick (const Geometry&,
             const Soil&, SoilWater&, double, 
	     const SoilHeat&, const Time&, Treelog&)
  { }
  double table () const
  { return 42.42e42; }

  // Create and Destroy.
public:
  void initialize (const Output&, const Geometry&, const Time&, Treelog&)
  { }
  GroundwaterDeep (Block& al)
    : Groundwater (al)
  { }
  ~GroundwaterDeep ()
  { }
};

static struct GroundwaterDeepSyntax
{
  static Model& make (Block& al)
  { return *new GroundwaterDeep (al); }

  GroundwaterDeepSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Deep groundwater, free drainage.");
      Groundwater::load_syntax (syntax, alist);
      BuildBase::add_type (Groundwater::component, "deep", alist, syntax, &make);
    }
} GroundwaterDeep_syntax;
