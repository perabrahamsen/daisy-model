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


#include "action.h"
#include "block.h"
#include "daisy.h"
#include "field.h"
#include "check.h"

struct ActionMix : public Action
{
  // Content.
  const double depth;
  const double penetration;

  // Simulation.
  void doIt (Daisy& daisy, Treelog& msg)
    {
      msg.message ("Tillage operation " + name);
      daisy.field->mix (0.0, depth, penetration, daisy.time, daisy.dt, msg);
    }

  ActionMix (Block& al)
    : Action (al),
      depth (al.number ("depth")),
      penetration (al.number ("penetration"))
    { }
};

static struct ActionMixSyntax
{
  static Model& make (Block& al)
    { return *new ActionMix (al); }

  ActionMixSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Mix soil content down to the specified depth.\n\
The effect is that nitrogen, water, temperature and such are averaged in\n\
the interval.");
      syntax.add ("depth", "cm", Check::negative (), Syntax::Const,
		  "How far down to mix the soil (a negative number).");
      syntax.order ("depth");
      syntax.add_fraction ("penetration", Syntax::Const, "\
Fraction of organic matter on surface that are incorporated in the soil\n\
by this operation.");
      alist.add ("penetration", 1.0);
      BuildBase::add_type (Action::component, "mix", alist, syntax, &make);
    }
} ActionMix_syntax;

struct ActionSwap : public Action
{
  // Content.
  const double middle;
  const double depth;

  // Simulation.
  void doIt (Daisy& daisy, Treelog& msg)
  {
    msg.message ("Tillage operation " + name);
    daisy.field->swap (0.0, middle, depth, daisy.time, daisy.dt, msg);
  }

  ActionSwap (Block& al)
    : Action (al),
      middle (al.number ("middle")),
      depth (al.number ("depth"))
    { }
};

static struct ActionSwapSyntax
{
  static Model& make (Block& al)
    { return *new ActionSwap (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
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
    {
      Syntax& syntax = *new Syntax ();
      syntax.add_check (check_alist);
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Swap two soil layers.  The top layer start at the surface and goes down to\n\
'middle', and the second layer starts with 'middle' and goes down to\n\
 'depth'.  After the operation, the content (such as heat, water, and\n\
organic matter) will be averaged in each layer, and the bottom layer will\n\
be placed on top of what used to be the top layer.");
      syntax.add ("middle", "cm", Check::negative (), Syntax::Const, "\
The end of the first layer and the start of the second layer to swap.");
      syntax.add ("depth", "cm", Check::negative (), Syntax::Const, "\
The end of the second layer to swap.");
      BuildBase::add_type (Action::component, "swap", alist, syntax, &make);
    }
} ActionSwap_syntax;


struct ActionSetPorosity : public Action
{
  // Content.
  const double porosity;
  const double depth;

  // Simulation.
  void doIt (Daisy& daisy, Treelog& out)
  {
    out.message ("Adjusting porosity");
    daisy.field->set_porosity (depth, porosity);
  }

  ActionSetPorosity (Block& al)
    : Action (al),
      porosity (al.number ("porosity")),
      depth (al.number ("depth"))
    { }
};

static struct ActionSetPorositySyntax
{
  static Model& make (Block& al)
    { return *new ActionSetPorosity (al); }

  ActionSetPorositySyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Set the porosity of the horizon at the specified depth.\n\
To get useful results, you need to use a hydraulic model that supports this.");
      syntax.add_fraction ("porosity", Syntax::Const, "\
Non-solid fraction of soil.");
      syntax.add ("depth", "cm", Check::non_positive (), Syntax::Const, "\
A point in the horizon to modify.");
      alist.add ("depth", 0.0);
      BuildBase::add_type (Action::component, "set_porosity", alist, syntax, &make);
    }
} ActionSetPorosity_syntax;
