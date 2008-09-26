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
#include "syntax.h"
#include "alist.h"
#include "librarian.h"

struct SVAT_none : public SVAT
{
  double crop_ea;
  double T_a;

  // Simulation.
  void tick (const Weather&, const Vegetation&,
	     const Surface&, const Geometry&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&, const Bioclimate& bio, Treelog&)
  { 
    crop_ea = bio.crop_ea();
    T_a = bio.daily_air_temperature ();
  }
  double production_stress () const
  { return -1; }
  
  void solve(const double, Treelog&)  
  { }

  double transpiration() const 
  { return crop_ea; }

  double CanopyTemperature () const
  { return T_a; }  // [dg C]

  double SunLeafTemperature () const
  { return T_a; }  // [dg C]

  double ShadowLeafTemperature () const
  { return T_a; }  // [dg C]

  // Create.
  SVAT_none (Block& al)
    : SVAT (al)
  { }
};

static struct SVAT_NoneSyntax
{
  static Model& make (Block& al)
  { return *new SVAT_none (al); }
  SVAT_NoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SVAT::load_syntax (syntax, alist);
    Librarian::add_type (SVAT::component, "none", alist, syntax, &make);
  }
} SVAT_none_syntax;
