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

#include "condition.h"
#include "block.h"
#include "crop.h"
#include "field.h"
#include "daisy.h"
#include "check_range.h"
#include "mathlib.h"

struct ConditionDSAfter : public Condition
{
  const symbol crop;
  const double ds;

  bool match (const Daisy& daisy, Treelog&) const
  { 
    const double crop_ds = daisy.field->crop_ds (crop); 
    if (!approximate (crop_ds, Crop::DSremove) && crop_ds >= ds)
      return true;
    return false;
  }
  void output (Log&) const
  { }

  ConditionDSAfter (Block& al)
    : Condition (al),
      crop (al.identifier ("crop")),
      ds (al.number ("ds"))
  { }
};

struct ConditionDMOver : public Condition
{
  const symbol crop;
  const double weight;
  const double height;

  bool match (const Daisy& daisy, Treelog&) const
  { return (daisy.field->crop_dm (crop, height) >= weight); }

  void output (Log&) const
  { }

  ConditionDMOver (Block& al)
    : Condition (al),
      crop (al.identifier ("crop")),
      weight (al.number ("weight")),
      height (al.number ("height"))
  { }
};

static struct ConditionCropSyntax
{
  static Model& make_ds (Block& al)
  { return *new ConditionDSAfter (al); }
  static Model& make_dm (Block& al)
  { return *new ConditionDMOver (al); }

  ConditionCropSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
True iff the crop has reached development stage 'ds'.");
      syntax.add ("crop", Syntax::String, Syntax::Const,
                  "Name of crop on the field to test.\n\
Specify \"all\" to use combined weight of all crops on the field in test.");
      static RangeII ds_range (-1.0, 2.0);
      syntax.add ("ds", Syntax::None (), ds_range, Syntax::Const,
                  "Development stage [-1.0:2.0].");
      syntax.order ("crop", "ds");
      BuildBase::add_type (Condition::component, "crop_ds_after",
                                      alist, syntax, &make_ds);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
True iff the crop has reached the specified amount of dry matter.");
      syntax.add ("crop", Syntax::String, Syntax::Const,
                  "Name of crop on the field to test.");
      syntax.add ("weight", "kg DM/ha", Check::non_negative (), Syntax::Const,
                  "\
Amount of non-root dry-matter required for the condition to be true.");
      syntax.add ("height", "cm", Check::non_negative (), Syntax::Const,
                  "\
Height above which we measure the DM weight.");
      alist.add ("height", 0.0);
      syntax.order ("crop", "weight");
      BuildBase::add_type (Condition::component, "crop_dm_over",
                                      alist, syntax, &make_dm);
    }
  }
} ConditionCrop_syntax;
