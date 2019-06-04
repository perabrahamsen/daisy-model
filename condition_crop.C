// condition_crop.C
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003 KVL.
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
//
// Checking crop state.

#define BUILD_DLL

#include "condition.h"
#include "block_model.h"
#include "crop.h"
#include "field.h"
#include "daisy.h"
#include "check_range.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

// The 'with' condition.

struct ConditionWith : public Condition
{
  const symbol where;
  std::unique_ptr<Condition> condition;

  bool match (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    Field::Restrict restriction (daisy.field (), where);
    return condition->match (daisy, scope, msg);
  }

  void tick (const Daisy& daisy, const Scope& scope, Treelog& out)
  {
    Field::Restrict restriction (daisy.field (), where);
    condition->tick (daisy, scope, out);
  }

  void output (Log&) const
  { }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    Field::Restrict restriction (daisy.field (), where);
    condition->initialize (daisy, scope, msg);
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    Treelog::Open nest (msg, std::string ("with ") + where);
  
    bool ok = true;

    if (!daisy.field ().find (where))
      {
	msg.entry (std::string ("No column '") + where + "'");
	ok = false;
      }
    else
      {
        Field::Restrict restriction (daisy.field (), where);
        ok = condition->check (daisy, scope, msg);
      }
    return ok;
  }

  ConditionWith (const BlockModel& al)
    : Condition (al),
      where (al.name ("where")),
      condition (Librarian::build_item<Condition> (al, "condition"))
  { }

  ~ConditionWith ()
  { }
};

static struct ConditionWithSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionWith (al); }

  ConditionWithSyntax ()
    : DeclareModel (Condition::component, "with", "\
Test condition for specific column.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("where", Attribute::Const, 
                          "Name of column to test condition on.");
    frame.declare_object ("condition", Condition::component,
                          "Condition to test on the specified column.");
    frame.order ("where", "condition");
  }
} ConditionWith_syntax;

// The 'crop_ds_after' condition.

struct ConditionDSAfter : public Condition
{
  const symbol crop;
  const double ds;

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { 
    const double crop_ds = daisy.field ().crop_ds (crop); 
    if (!approximate (crop_ds, Crop::DSremove) && crop_ds >= ds)
      return true;
    return false;
  }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionDSAfter (const BlockModel& al)
    : Condition (al),
      crop (al.name ("crop")),
      ds (al.number ("ds"))
  { }
};

static struct ConditionCropDSSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionDSAfter (al); }

  ConditionCropDSSyntax ()
    : DeclareModel (Condition::component, "crop_ds_after", "\
True iff the crop has reached development stage 'ds'.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("crop", Attribute::Const,
                "Name of crop on the field to test.\n\
Specify \"all\" to use combined weight of all crops on the field in test.");
    frame.set_check ("crop", Crop::check_all ());
    static RangeII ds_range (-1.0, 2.0);
    frame.declare ("ds", Attribute::None (), ds_range, Attribute::Const,
                "Development stage [-1.0:2.0].");
    frame.order ("crop", "ds");
  }
} ConditionCropDS_syntax;

// The 'crop_stage_after' condition.

struct ConditionStageAfter : public Condition
{
  const symbol crop;
  const double stage;

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { 
    const double crop_stage = daisy.field ().crop_stage (crop); 
    if (!approximate (crop_stage, Crop::DSremove) && crop_stage >= stage)
      return true;
    return false;
  }
  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionStageAfter (const BlockModel& al)
    : Condition (al),
      crop (al.name ("crop")),
      stage (al.number ("stage"))
  { }
};

static struct ConditionCropStageSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionStageAfter (al); }

  ConditionCropStageSyntax ()
    : DeclareModel (Condition::component, "crop_stage_after", "\
True iff the crop has reached development stage 'stage'.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("crop", Attribute::Const,
                "Name of crop on the field to test.\n\
Specify \"all\" to use combined weight of all crops on the field in test.");
    frame.set_check ("crop", Crop::check_all ());
    frame.declare ("stage", Attribute::None (),  Attribute::Const,
		   "Crop specific phenological stage.");
    frame.order ("crop", "stage");
  }
} ConditionCropStage_syntax;

// The 'crop_dm_over' condition.

struct ConditionDMOver : public Condition
{
  const symbol crop;
  const double weight;
  const double height;

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return (daisy.field ().crop_dm (crop, height) > weight); }

  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionDMOver (const BlockModel& al)
    : Condition (al),
      crop (al.name ("crop")),
      weight (al.number ("weight")),
      height (al.number ("height"))
  { }
};

static struct ConditionCropDMSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionDMOver (al); }

  ConditionCropDMSyntax ()
    : DeclareModel (Condition::component, "crop_dm_over", "\
True iff the crop has reached the specified amount of dry matter.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("crop", Attribute::Const,
                "Name of crop on the field to test.");
    frame.set_check ("crop", Crop::check_all ());
    frame.declare ("weight", "kg DM/ha", Check::non_negative (), Attribute::Const,
                "\
Amount of non-root dry-matter required for the condition to be true.");
    frame.declare ("height", "cm", Check::non_negative (), Attribute::Const,
                "\
Height above which we measure the DM weight.");
    frame.set ("height", 0.0);
    frame.order ("crop", "weight");
  }
} ConditionCropDM_syntax;

// The 'crop_dm_sorg_over' condition.

struct ConditionDMSOrgOver : public Condition
{
  const symbol crop;
  const double weight;

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  { return (daisy.field ().crop_sorg_dm (crop) > weight); }

  void output (Log&) const
  { }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionDMSOrgOver (const BlockModel& al)
    : Condition (al),
      crop (al.name ("crop")),
      weight (al.number ("weight"))
  { }
};

static struct ConditionCropDMSorgSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionDMSOrgOver (al); }

  ConditionCropDMSorgSyntax ()
    : DeclareModel (Condition::component, "crop_dm_sorg_over", "\
True iff the storage organ has reached the specified amount of dry matter.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("crop", Attribute::Const,
                "Name of crop on the field to test.");
    frame.set_check ("crop", Crop::check_all ());
    frame.declare ("weight", "kg DM/ha", Check::non_negative (), Attribute::Const,
                "\
Amount of non-root dry-matter required for the condition to be true.");
    frame.order ("crop", "weight");
  }
} ConditionCropDMSorg_syntax;

// condition_crop.C ends here.
