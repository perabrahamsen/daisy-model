// action_spray.C -- Spray a chemical on the field.
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
#include "block_model.h"
#include "daisy.h"
#include "field.h"
#include "chemical.h"
#include "check.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>

struct ActionSpray : public Action
{
  const symbol chemical;
  const double amount;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "Spraying " << amount << " g " << chemical << "/ha";
    msg.message (tmp.str ());
    daisy.field ().spray_overhead (chemical, amount, msg); 
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ActionSpray (const BlockModel& al)
    : Action (al),
      chemical (al.name ("chemical")),
      amount (al.number ("amount"))
  { }
};

// Add the ActionSpray syntax to the syntax table.
static struct ActionSpraySyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionSpray (al); }

  ActionSpraySyntax ()
    : DeclareModel (Action::component, "spray", "\
Spray a chemical (typically a pesticide) on the field.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("chemical", Attribute::Const,
		"Name of pesticide to spray.");
    frame.set_check ("chemical", Chemical::check_buildable ());
    frame.declare ("amount", "g/ha", Check::non_negative (), Attribute::Const,
		"Amount of pesticide to spray.");
    frame.order ("chemical", "amount");
  }
} ActionSpray_syntax;

struct ActionSpraySurface : public Action
{
  const symbol chemical;
  const double amount;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "Spraying " << amount << " g " << chemical << "/ha";
    msg.message (tmp.str ());
    daisy.field ().spray_surface (chemical, amount, msg); 
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ActionSpraySurface (const BlockModel& al)
    : Action (al),
      chemical (al.name ("chemical")),
      amount (al.number ("amount"))
  { }
};

// Add the ActionSpraySurface syntax to the syntax table.
static struct ActionSpraySurfaceSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionSpraySurface (al); }

  ActionSpraySurfaceSyntax ()
    : DeclareModel (Action::component, "spray_surface", "\
Spray a chemical (typically a pesticide) on the field below the canopy.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("chemical", Attribute::Const,
		"Name of pesticide to spray.");
    frame.set_check ("chemical", Chemical::check_buildable ());
    frame.declare ("amount", "g/ha", Check::non_negative (), Attribute::Const,
		"Amount of pesticide to spray.");
    frame.order ("chemical", "amount");
  }
} ActionSpraySurface_syntax;

struct ActionRemoveSolute : public Action
{
  const symbol chemical;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "Removing " << daisy.field ().total_solute (chemical)
	<< " g " << chemical << "/ha from field.";
    msg.message (tmp.str ());
    daisy.field ().remove_solute (chemical); 
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ActionRemoveSolute (const BlockModel& al)
    : Action (al),
      chemical (al.name ("chemical"))
  { }
};

// Add the ActionRemoveSolute syntax to the syntax table.
static struct ActionRemoveSoluteSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionRemoveSolute (al); }

  ActionRemoveSoluteSyntax ()
    : DeclareModel (Action::component, "remove_solute", "\
Remove a specific chemical from the field.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("chemical", Attribute::Const,
			  "Name of chemical to remove.");
    frame.set_check ("chemical", Chemical::check_buildable ());
    frame.order ("chemical");
  }
} ActionRemoveSolute_syntax;

// action_spray.C ends here.
