// action_tillage.C
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

struct ActionMix : public Action
{
  // Content.
  const double depth;
  const double penetration;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
    {
      msg.message ("Tillage operation " + name);
      daisy.field->mix (daisy.metalib, 
                        0.0, depth, penetration, daisy.time, daisy.dt, msg);
    }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionMix (Block& al)
    : Action (al),
      depth (al.number ("depth")),
      penetration (al.number ("penetration"))
    { }
};

static struct ActionMixSyntax : DeclareModel
{
  Model* make (Block& al) const
    { return new ActionMix (al); }

  ActionMixSyntax ()
    : DeclareModel (Action::component, "mix", "\
Mix soil content down to the specified depth.\n\
The effect is that nitrogen, water, temperature and such are averaged in\n\
the interval.")
  { }
  void load_frame (Frame& frame) const
  { 
      frame.declare ("depth", "cm", Check::negative (), Attribute::Const,
		  "How far down to mix the soil (a negative number).");
      frame.order ("depth");
      frame.declare_fraction ("penetration", Attribute::Const, "\
Fraction of organic matter on surface that are incorporated in the soil\n\
by this operation.");
      frame.set ("penetration", 1.0);
    }
} ActionMix_syntax;

struct ActionSwap : public Action
{
  // Content.
  const double middle;
  const double depth;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    msg.message ("Tillage operation " + name);
    daisy.field->swap (daisy.metalib, 
                       0.0, middle, depth, daisy.time, daisy.dt, msg);
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionSwap (Block& al)
    : Action (al),
      middle (al.number ("middle")),
      depth (al.number ("depth"))
    { }
};

static struct ActionSwapSyntax : DeclareModel
{
  Model* make (Block& al) const
  { return new ActionSwap (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  {
    const double middle (al.number ("middle"));
    const double depth (al.number ("depth"));
    bool ok = true;
    if (middle <= depth)
      {
        err.entry ("swap middle should be above the depth");
        ok = false;
      }
    return ok;
  }

  ActionSwapSyntax ()
    : DeclareModel (Action::component, "swap", "\
Swap two soil layers.  The top layer start at the surface and goes down to\n\
'middle', and the second layer starts with 'middle' and goes down to\n\
 'depth'.  After the operation, the content (such as heat, water, and\n\
organic matter) will be averaged in each layer, and the bottom layer will\n\
be placed on top of what used to be the top layer.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare ("middle", "cm", Check::negative (), Attribute::Const, "\
The end of the first layer and the start of the second layer to swap.");
    frame.declare ("depth", "cm", Check::negative (), Attribute::Const, "\
The end of the second layer to swap.");
  }
} ActionSwap_syntax;


struct ActionSetPorosity : public Action
{
  // Content.
  const double porosity;
  const double depth;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog& out)
  {
    out.message ("Adjusting porosity");
    daisy.field->set_porosity (depth, porosity);
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionSetPorosity (Block& al)
    : Action (al),
      porosity (al.number ("porosity")),
      depth (al.number ("depth"))
    { }
};

static struct ActionSetPorositySyntax : DeclareModel
{
  Model* make (Block& al) const
  { return new ActionSetPorosity (al); }

  ActionSetPorositySyntax ()
    : DeclareModel (Action::component, "set_porosity", "\
Set the porosity of the horizon at the specified depth.\n\
To get useful results, you need to use a hydraulic model that supports this.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_fraction ("porosity", Attribute::Const, "\
Non-solid fraction of soil.");
    frame.declare ("depth", "cm", Check::non_positive (), Attribute::Const, "\
A point in the horizon to modify.");
    frame.set ("depth", 0.0);
  }
} ActionSetPorosity_syntax;

// action_tillage.C ends here.
