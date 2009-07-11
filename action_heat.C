// action_heat.C
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

#include "action.h"
#include "block.h"
#include "daisy.h"
#include "field.h"
#include "check.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

struct ActionSetHeatSource : public Action
{
  // Content.
  const double height;
  const double value;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog& out)
  {
    out.message ("Adjusting heat source");
    daisy.field->set_heat_source (height, value);
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionSetHeatSource (const Block& al)
    : Action (al),
      height (al.number ("height")),
      value (al.number ("value"))
  { }
};

static struct ActionHeatSyntax : DeclareModel
{
  Model* make (const Block& al) const
  { return new ActionSetHeatSource (al); }
  
  ActionHeatSyntax ()
    : DeclareModel (Action::component, "set_heat_source", "\
Set external point heat source at height to value.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("height", "cm", Check::non_positive (), Attribute::Const,
		"Height of heat source (a negative number).");
    frame.declare ("value", "W/m^2", Check::non_negative (), Attribute::Const,
		"Value of heat source.");
    frame.order ("height", "value");
  }
} ActionHeat_syntax;

// action_heat.C ends here.
