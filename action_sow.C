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
#include "block_model.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"
#include "librarian.h"
#include "check.h"
#include "treelog.h"
#include "frame_model.h"

struct ActionSowBase : public Action
{
  const double row_width;
  const double row_pos;
  const double seed;

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionSowBase (const BlockModel& al)
    : Action (al),
      // Use 'plant_distance' if set, otherwise use 'row_width'.
      row_width (al.number ("plant_distance", al.number ("row_width"))),
      // Use 'plant_distance' if set, otherwise use 'row_width'.
      row_pos (al.number ("plant_position", al.number ("row_position"))),
      seed (al.number ("seed", -42.42e42))
  { }
};

// Add the ActionSowBase syntax to the syntax table.
static struct ActionSowBaseSyntax : DeclareBase
{
  ActionSowBaseSyntax ()
    : DeclareBase (Action::component, "sow_base", "Shared sowing parameters.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("row_width", "cm", Check::non_negative (), 
                   Attribute::Const, "Distance between rows.\n\
Specify zero to spread equally over the area (no rows).");
    frame.set ("row_width", 0.0);
    frame.declare ("plant_distance", "cm", Check::non_negative (),
                   Attribute::OptionalConst, "Distance between plants.\n\
\n\
Setting this will overrule 'row_width'.  The only purpose of this\n\
paramater is to provide the user with a more intuitive name for\n\
'row_width' for the situation where you have a 2D simulation, where\n\
the x axis is parallel with the actual rows in the field, rather than\n\
ortogonal to the rows as is otherwise assumed by Daisy.");
    frame.declare ("row_position", "cm", Check::non_negative (), 
                   Attribute::Const, "Position of plant row on x-axes.\n\
Specify zero to spread equally over the area (no rows).");
    frame.set ("row_position", 0.0);
    frame.declare ("plant_position", "cm", Check::non_negative (),
                   Attribute::OptionalConst, "Position of plant on x-axes.\n\
\n\
Setting this will overrule 'row_position'.  The only purpose of this\n\
paramater is to provide the user with a more intuitive name for\n       \
'row_position' for the situation where you have a 2D simulation, where\n\
the x axis is parallel with the actual rows in the field, rather than\n\
ortogonal to the rows as is otherwise assumed by Daisy.");
    frame.declare ("seed", "g w.w./m^2", Check::positive (),
                   Attribute::OptionalConst, "Amount of seed applied.\n\
By default, initial growth will be governed by 'typical' seed amounts.");
  }
} ActionSowBase_syntax;

struct ActionSow : public ActionSowBase
{
  const Metalib& metalib;
  const std::unique_ptr<FrameModel> crop;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  { 
    msg.message ("Sowing " + crop->type_name ());      
    daisy.field ().sow (metalib, *crop, row_width, row_pos, seed, 
                      daisy.time (), msg); 
  }

  ActionSow (const BlockModel& al)
    : ActionSowBase (al),
      metalib (al.metalib ()),
      crop (&al.model ("crop").clone ())
  { }
};

// Add the ActionSow syntax to the syntax table.
static struct ActionSowSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionSow (al); }

  ActionSowSyntax ()
    : DeclareModel (Action::component, "sow", "sow_base",
                    "Sow a crop on the field.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("crop", Crop::component, "Crop to sow.");
    frame.order ("crop");
  }
} ActionSow_syntax;

static DeclareAlias 
ActionPlant_syntax (Action::component, "plant", "sow");

struct ActionSowObject : public ActionSowBase
{
  const Metalib& metalib;
  Crop& crop;
  bool sown;
  
  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  { 
    if (sown)
      {
        msg.error ("'" + crop.objid + "' already sown");
        return;
      }
    sown = true;
    msg.message ("Sowing " + crop.objid + " once"); 
    daisy.field ().sow (metalib, crop, row_width, row_pos, seed, 
                      daisy.time (), msg); 
  }

  ActionSowObject (const BlockModel& al)
    : ActionSowBase (al),
      metalib (al.metalib ()),
      crop (*Librarian::build_item<Crop> (al, "crop")),
      sown (false)
  { }
  ~ActionSowObject ()
  {
    if (!sown)
      delete &crop;
  }
};

// Add the ActionSowObject syntax to the syntax table.
static struct ActionSowObjectSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionSowObject (al); }

  ActionSowObjectSyntax ()
    : DeclareModel (Action::component, "sow_object", "sow_base", "\
Variant of 'sow' that allows reference parameters.\n\
Unlike the normal 'sow', this action can only be called once per simulation.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("crop", Crop::component, "Crop to sow.");
    frame.order ("crop");
  }
} ActionSowObject_syntax;

struct ActionSowName : public ActionSowBase
{
  const Metalib& metalib;
  const symbol crop;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    Crop *const build
      = Librarian::build_stock<Crop> (metalib, msg, crop, "sow");
    if (!build)
      {
        msg.error ("Sowing failed");
        return;
      }
    msg.message ("Sowing '" + crop + "'"); 
    daisy.field ().sow (metalib, *build, row_width, row_pos, seed, 
                        daisy.time (), msg); 
  }

  ActionSowName (const BlockModel& al)
    : ActionSowBase (al),
      metalib (al.metalib ()),
      crop (al.name ("crop"))
  { }
  ~ActionSowName ()
  { }
};

// Add the ActionSowName syntax to the syntax table.
static struct ActionSowNameSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionSowName (al); }

  ActionSowNameSyntax ()
    : DeclareModel (Action::component, "sow_name", "sow_base", "\
Variant of 'sow' that takes the name of a crop.\n\
This name can be a reference parameter.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("crop", Attribute::Const, "Name of crop to sow.");
    frame.set_check ("crop", Crop::check_buildable ());
    frame.order ("crop");
  }
} ActionSowName_syntax;

// action_sow.C ends here.
