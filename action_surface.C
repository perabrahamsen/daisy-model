// action_surface.C
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
#include "message.h"

struct ActionSetSurfaceDetentionCapacity : public Action
{
  // Content.
  const double height;

  // Simulation.
  void doIt (Daisy& daisy)
  {
    COUT << " [Surface]\n";
    daisy.field.set_surface_detention_capacity (height);
  }

  ActionSetSurfaceDetentionCapacity (const AttributeList& al)
    : Action (al),
      height (al.number ("height"))
  { }
};

static struct ActionSurfaceSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSetSurfaceDetentionCapacity (al); }
  
  ActionSurfaceSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Set amount of ponding the surface can retain.");
    syntax.add ("height", "cm", Check::non_negative (), Syntax::Const,
		"Max ponding height before runoff.");
    syntax.order ("height");
    Librarian<Action>::add_type ("set_surface_detention_capacity", 
				 alist, syntax, &make);
  }
} ActionSurface_syntax;

