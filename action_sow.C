// action_sow.C
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
#include "crop.h"
#include "librarian.h"
#include "check.h"
#include "dlf.h"
#include "treelog.h"

struct ActionSow : public Action
{
  const AttributeList& crop;
  const double row_width;
  const double row_pos;
  const double seed;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  { 
    msg.message ("Sowing " + crop.name ("type"));      
    daisy.field->sow (daisy.metalib, crop, row_width, row_pos, seed, 
                      daisy.time, daisy.dt, msg); 
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionSow (Block& al)
    : Action (al),
      crop (al.alist ("crop")),
      // Use 'plant_distance' if set, otherwise use 'row_width'.
      row_width (al.number ("plant_distance", al.number ("row_width"))),
      // Use 'plant_distance' if set, otherwise use 'row_width'.
      row_pos (al.number ("plant_position", al.number ("row_position"))),
      seed (al.number ("seed", -42.42e42))
  { }
};

// Add the ActionSow syntax to the syntax table.
static struct ActionSowSyntax
{
  static Model& make (Block& al)
  { return *new ActionSow (al); }

  ActionSowSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Sow a crop on the field.");
    syntax.add_object ("crop", Crop::component, "Crop to sow.");
    syntax.order ("crop");
    syntax.add ("row_width", "cm", Check::non_negative (), 
                Value::Const, "Distance between rows.\n\
Specify zero to spread equally over the area (no rows).");
    alist.add ("row_width", 0.0);
    syntax.add ("plant_distance", "cm", Check::non_negative (),
                Value::OptionalConst, "Distance between plants.\n\
\n\
Setting this will overrule 'row_width'.  The only purpose of this\n\
paramater is to provide the user with a more intuitive name for\n\
'row_width' for the situation where you have a 2D simulation, where\n\
the x axis is parallel with the actual rows in the field, rather than\n\
ortogonal to the rows as is otherwise assumed by Daisy.");
    syntax.add ("row_position", "cm", Check::non_negative (), 
                Value::Const, "Position of plant row on x-axes.\n\
Specify zero to spread equally over the area (no rows).");
    alist.add ("row_position", 0.0);
    syntax.add ("plant_position", "cm", Check::non_negative (),
                Value::OptionalConst, "Position of plant on x-axes.\n\
\n                                                                      \
Setting this will overrule 'row_position'.  The only purpose of this\n\
paramater is to provide the user with a more intuitive name for\n       \
'row_position' for the situation where you have a 2D simulation, where\n\
the x axis is parallel with the actual rows in the field, rather than\n\
ortogonal to the rows as is otherwise assumed by Daisy.");
    syntax.add ("seed", "g w.w./m^2", Check::positive (), Value::OptionalConst,
                "Amount of seed applied.\n\
By default, initial growth will be governed by 'typical' seed amounts.");
    Librarian::add_type (Action::component, "sow", alist, syntax, &make);
    Librarian::add_alias (Action::component, symbol ("plant"), symbol ("sow"));
  }
} ActionSow_syntax;

// action_sow.C ends here.
