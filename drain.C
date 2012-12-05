// drain.C --- Lateral transport of water.
// 
// Copyright 2008, 2010 Per Abrahamsen and KU.
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

#include "drain.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"

// The 'drain' component.

const char *const Drain::component = "drain";

symbol
Drain::library_id () const
{
  static const symbol id (component);
  return id;
}

Drain::Drain (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Drain::~Drain ()
{ }

static struct DrainInit : public DeclareComponent 
{
  DrainInit ()
    : DeclareComponent (Drain::component, "\
Lateral transport of water.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Drain_init;

// The 'none' model.

struct DrainNone : public Drain
{
  // Simulation.
  void tick (const Time&, const Scope&, 
             const Geometry&, const Soil&, const SoilHeat&, const Surface&, 
             SoilWater&, Treelog&)
  { }
  void output (Log&) const
  { }

  // Create and Destroy.
  void initialize (const Time&, const Scope&, const Geometry&, Treelog&)
  { }
  bool check (const Scope&, Treelog&) const
  { return true; }
  DrainNone (const BlockModel& al)
    : Drain (al)
  { }
};

static struct DrainNoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DrainNone (al); }
  DrainNoneSyntax ()
    : DeclareModel (Drain::component, "none", "No lateral transport.")
  { }
  void load_frame (Frame& frame) const
  { }
} DrainNone_syntax;

// drain.C ends here.
