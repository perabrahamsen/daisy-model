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


#include "action.h"
#include "daisy.h"
#include "field.h"
#include "check.h"

struct ActionSetHeatSource : public Action
{
  // Content.
  const double height;
  const double value;

  // Simulation.
  void doIt (Daisy& daisy, Treelog& out)
  {
    out.message ("Adjusting heat source");
    daisy.field.set_heat_source (height, value);
  }

  ActionSetHeatSource (const AttributeList& al)
    : Action (al),
      height (al.number ("height")),
      value (al.number ("value"))
  { }
};

static struct ActionHeatSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSetHeatSource (al); }
  
  ActionHeatSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Set external point heat source at height to value.");
    syntax.add ("height", "cm", Check::non_positive (), Syntax::Const,
		"Height of heat source (a negative number).");
    syntax.add ("value", "W/m^2", Check::non_negative (), Syntax::Const,
		"Value of heat source.");
    syntax.order ("height", "value");
    Librarian<Action>::add_type ("set_heat_source", 
				 alist, syntax, &make);
  }
} ActionHeat_syntax;
