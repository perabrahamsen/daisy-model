// action_harvest.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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
#include "daisy.h"
#include "field.h"
#include "harvest.h"
#include "librarian.h"
#include "vegetation.h"
#include "treelog.h"
#include "frame.h"
#include "crop.h"
#include "mathlib.h"
#include <sstream>

// The 'emerge' action model.

struct ActionEmerge : public Action
{
  const symbol crop;

  void doIt (Daisy& daisy, const Scope&, Treelog& out)
  {
    if (crop != Vegetation::all_crops ())
      {
        if (daisy.field ().crop_ds (crop) < -1.0)
          {
            out.warning ("Attempted forced emerge of " 
                         + crop + " which is not on the field");
            return;
          }
        if (daisy.field ().crop_ds (crop) >= 0.0)
          {
            out.warning ("Forced emerge of " + crop
                         + " which is already emerged");
            return;
          }
      }
    out.message ("Forcing emergence of " + crop);
    daisy.field ().emerge (crop, out);
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionEmerge (const BlockModel& al)
    : Action (al),
      crop (al.name ("crop"))
  { }
};

static struct ActionEmergeSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionEmerge (al); }
  ActionEmergeSyntax ()
    : DeclareModel (Action::component, "emerge", "Force a crop to emerge.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("crop", Attribute::Const, 
                "Name of the crop to emerge.\n\
If you specify 'all', all crops will emerge.\n\
If there are no crop on the field with the specified name,\n\
nothing will happen.");
    frame.set ("crop", Vegetation::all_crops ());
    frame.set_check ("crop", Crop::check_all ());
    frame.order ("crop");
  }
} ActionEmerge_syntax;

// The 'harvest_base' base model.

static struct ActionHarvestBaseSyntax : DeclareBase
{
  ActionHarvestBaseSyntax ()
    : DeclareBase (Action::component, "harvest_base", "\
Common parameters for harvest operations.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("crop", Attribute::Const, 
                "Name of the crop to harvest or cut.\n\
If you specify 'all', all crops will be harvested.\n\
If there are no crop on the field with the specified name,\n\
nothing will happen.");
    frame.set ("crop", Vegetation::all_crops ());
    frame.set_check ("crop", Crop::check_all ());
    frame.declare ("stub", "cm", Attribute::Const, "\
Leave stem and leafs below this height on the field.");
    frame.set ("stub", 0.0);
    frame.declare_fraction ("stem", Attribute::Const, "\
Fraction of stem (above stub) to harvest.");
    frame.set ("stem", 1.0);
    frame.declare_fraction ("leaf", Attribute::Const, "\
Fraction of leafs (above stub) to harvest.");
    frame.set ("leaf", 1.0);
    frame.declare_fraction ("sorg", Attribute::Const, "\
Fraction of storage organ to harvest.");
    frame.set ("sorg", 1.0);
    frame.declare_boolean ("combine", Attribute::Const, "\
Set this to 'true' in order to combine all crop parts into stem\n\
in the harvest log files.\n\
This is mostly useful for silage.");
    frame.set ("combine", false);
    frame.order ("crop");
  }
} ActionHarvestBase_syntax;

// The 'harvest' action model.

struct ActionHarvest : public Action
{
  const symbol crop;
  const double stub;
  const double stem;
  const double leaf;
  const double sorg;
  const bool combine;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    if (crop != Vegetation::all_crops () && daisy.field ().crop_ds (crop) < 0.0)
      {
	msg.warning ("Attempting to harvest " + crop 
		     + " which has not emerged on the field");
	return;
      }
    double old_DM = 0.0;
    double old_SOrg = 0.0;
    for (size_t i = 0; i < daisy.harvest ().size (); i++)
      {
	old_DM += daisy.harvest ()[i]->total_DM ();
	old_SOrg += daisy.harvest ()[i]->sorg_DM;
      }
    daisy.field ().harvest (daisy.time (), 
                            crop, stub, stem, leaf, sorg, combine,
                            daisy.harvest (), msg);
    double new_DM = 0.0;
    double new_SOrg = 0.0;
    for (size_t i = 0; i < daisy.harvest ().size (); i++)
      {
	new_DM += daisy.harvest ()[i]->total_DM ();
	new_SOrg += daisy.harvest ()[i]->sorg_DM;
      }

    std::ostringstream tmp;
    const bool killed = daisy.field ().crop_ds (crop) < 0.0;
    if (killed)
      tmp << "Harvesting ";
    else
      tmp << "Cutting ";
    tmp << crop;
    const double total = (new_DM - old_DM) * 0.01;
    const double sorg = (new_SOrg - old_SOrg) * 0.01;
    if (total < 1e-5)
      tmp << ", with no yield :-(";
    else if (approximate (total, sorg))
      tmp << ", removing " << total << " Mg DM/ha";
    else
      tmp << ", removing " << sorg << " + " << (total - sorg) << " Mg DM/ha";
    
    msg.message (tmp.str ());
    
    was_killed (killed, msg);
  }

  virtual void was_killed (const bool killed, Treelog& msg)
  { 
    if (!killed)
      msg.warning ("The crop survived harvest.\n\
If this was intended, you should use the 'cut' action instead to avoid this message");
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionHarvest (const BlockModel& al)
    : Action (al),
      crop (al.name ("crop")), 
      stub (al.number ("stub")),
      stem (al.number ("stem")),
      leaf (al.number ("leaf")),
      sorg (al.number ("sorg")),
      combine (al.flag ("combine"))
  { }
};

static struct ActionHarvestSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionHarvest (al); }
  ActionHarvestSyntax ()
    : DeclareModel (Action::component, "harvest", "harvest_base", "\
Harvest a crop.")
  { }
  void load_frame (Frame& frame) const
  { }
} ActionHarvest_syntax;

// The 'cut' action model.

struct ActionCut : public ActionHarvest
{
  void was_killed (const bool killed, Treelog& msg)
  { 
    if (killed)
      msg.warning ("The crop did not survive the cut.\n\
If this was intended, you should use the 'harvest' action instead to avoid this message");
  }

  ActionCut (const BlockModel& al)
    : ActionHarvest (al)
  { }
};

static struct ActionCutSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionCut (al); }
  ActionCutSyntax ()
    : DeclareModel (Action::component, "cut", "harvest_base", "Cut a crop.")
  { }
  void load_frame (Frame& frame) const
  { }
} ActionCut_syntax;

// The 'pluck' action model.

struct ActionPluck : public Action
{
  const symbol crop;
  const double stem;
  const double leaf;
  const double sorg;

  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    if (crop != Vegetation::all_crops () && daisy.field ().crop_ds (crop) < 0.0)
      {
	msg.warning ("Attempting to pluck " + crop 
		     + " which has not emerged on the field");
	return;
      }
    double old_DM = 0.0;
    for (size_t i = 0; i < daisy.harvest ().size (); i++)
      old_DM += daisy.harvest ()[i]->total_DM ();
    daisy.field ().pluck (daisy.time (), crop, stem, leaf, sorg, 
                          daisy.harvest (), msg);
    double new_DM = 0.0;
    for (size_t i = 0; i < daisy.harvest ().size (); i++)
      new_DM += daisy.harvest ()[i]->total_DM ();
    std::ostringstream tmp;
    const bool killed = daisy.field ().crop_ds (crop) < 0.0;
    if (killed)
      tmp << "Harvesting ";
    else
      tmp << "Plucking ";
    tmp << crop << ", removing " << (new_DM - old_DM) * 0.01 << " Mg DM/ha";
    msg.message (tmp.str ());
    if (killed)
      msg.warning ("The crop did not survive the plucking.\n\
If this was intended, you should use the 'harvest' action instead to avoid this message");
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  ActionPluck (const BlockModel& al)
    : Action (al),
      crop (al.name ("crop")), 
      stem (al.number ("stem")),
      leaf (al.number ("leaf")),
      sorg (al.number ("sorg"))
  { }
};

static struct ActionPluckSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionPluck (al); }
  ActionPluckSyntax ()
    : DeclareModel (Action::component, "pluck", "Pluck a crop.\n\
Unlike the 'harvest' operation, this allows you to pluck selected parts of\n\
the above ground dry matter without killing the crop.\n\
It is intended for crops like tomatoes, that are harvested multiple times.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("crop", Attribute::Const, 
                "Name of the crop to pluck.\n\
If you specify 'all', all crops will be plucked.\n\
If there are no crop on the field with the specified name,\n\
nothing will happen.");
    frame.set ("crop", Vegetation::all_crops ());
    frame.set_check ("crop", Crop::check_all ());
    frame.declare_fraction ("stem", Attribute::Const, "\
Fraction of stem to pluck.");
    frame.set ("stem", 0.0);
    frame.declare_fraction ("leaf", Attribute::Const, "\
Fraction of leaves to pluck.");
    frame.set ("leaf", 0.0);
    frame.declare_fraction ("sorg", Attribute::Const, "\
Fraction of storage organ to pluck.");
    frame.set ("sorg", 1.0);
    frame.order ("crop");
  }
} ActionPluck_syntax;

// action_harvest.C ends here.
