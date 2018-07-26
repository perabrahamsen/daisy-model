// ABAprod.C  -- Production of ABA in soil.
// 
// Copyright 2007 Per Abrahamsen and KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.5
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

#include "ABAprod.h"
#include "block_model.h"
#include "librarian.h"

const char *const ABAProd::component = "ABAproduction";

symbol 
ABAProd::library_id () const
{
  static const symbol id (component);
  return id;
}

ABAProd::ABAProd (const BlockModel& al)
  : ModelDerived (al.type_name ()),
    units (al.units ())
{ }

ABAProd::~ABAProd ()
{ }

static struct ABAProdInit : public DeclareComponent
{
  ABAProdInit () 
    : DeclareComponent (ABAProd::component, "\
The 'ABAproduction' component calculates the prod of ABA in soil.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} ABAProd_init;

struct ABAProdNone : public ABAProd
{
  // Solve.
  void production (const Geometry&, const SoilWater&,
		   const std::vector<double>& /* [cm^3/cm^3] */,
		   const std::vector<double>& /* [cm/cm^3] */,
		   std::vector<double>& ABA /* [g/cm/h] */,
		   Treelog&) const
  { fill (ABA.begin (), ABA.end (), 0.0); }
  void output (Log&) const
  { }

  // Create and Destroy.
  void initialize (Treelog&)
  { }
  bool check (Treelog&) const
  { return true; }
  ABAProdNone (const BlockModel& al)
    : ABAProd (al)
  { }
  ~ABAProdNone ()
  { }
};

static struct ABAProdNoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ABAProdNone (al); }
  ABAProdNoneSyntax ()
    : DeclareModel (ABAProd::component, "none", "No ABA production.")
  { }
  void load_frame (Frame& frame) const
  { }
} ABAProdNone_syntax;

// ABAprod.C ends here

