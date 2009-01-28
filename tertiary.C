// tertiary.C --- Transport of water and solute outside the matrix.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "tertiary.h"
#include "tertsmall.h"
#include "geometry.h"
#include "soil_water.h"
#include "block.h"
#include "librarian.h"
#include "frame.h"

// The 'tertiary' component.

const char *const Tertiary::component = "tertiary";

symbol
Tertiary::library_id () const
{
  static const symbol id (component);
  return id;
}

Tertiary::Tertiary (Block& al)
  : ModelFramed (al)
{ }

Tertiary::~Tertiary ()
{ }

static struct TertiaryInit : public DeclareComponent 
{
  TertiaryInit ()
    : DeclareComponent (Tertiary::component, "\
Transport of water and solute outside the matrix.")
  { }
} Tertiary_init;

// The 'none' model.

class TertiaryNone : public Tertiary
{
  // Identity.
  bool has_macropores ()
  { return false; }

  // Simulation.
  void deactivate (const int)
  { }
  void tick (const Units&, const Geometry&, const Soil&, const SoilHeat&,
             const double dt, SoilWater&, Surface&, Treelog&)
  { }
  Tertsmall& implicit ()
  { return Tertsmall::none (); }
  void solute (const Geometry&, const SoilWater&, 
               const std::map<size_t, double>& J_tertiary,
               const double /* dt */,
               Chemical&, Treelog&)
  { }
  void output (Log&) const
  { }

  // Create and Destroy.
  bool initialize (const Units&, const Geometry&, const Soil&, const Scope&,  
                   const Groundwater&, Treelog&)
  { return true; }
  bool check (const Geometry&, Treelog&) const
  { return true; }
public:
  TertiaryNone (Block& al)
    : Tertiary (al)
  { }
};

static struct TertiaryNoneSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new TertiaryNone (al); }
  TertiaryNoneSyntax ()
    : DeclareModel (Tertiary::component, "none", "No tertiary transport.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} TertiaryNone_syntax;

// tertiary.C ends here.
