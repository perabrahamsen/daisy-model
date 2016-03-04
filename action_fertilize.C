// action_fertilize.C -- Apply fertilizer to the field.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and KVL.
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
#include "am.h"
#include "im.h"
#include "check.h"
#include "assertion.h"
#include "librarian.h"
#include "volume.h"
#include "units.h"
#include "treelog.h"
#include "frame_model.h"
#include "frame_submodel.h"
#include <sstream>

// Base class for fertilize actions.

struct ActionFertilize : public Action
{
  const Metalib& metalib;

  // Parameters.
  std::unique_ptr<FrameModel> am;
  const bool second_year_compensation;
  const double minimum_weight;

  struct Precision
  {
    // Parameters.
    const double target;
    const double from;
    const double to;
    
    // Create and Destroy.
    static bool check_alist (const Metalib&, const Frame& al, Treelog& err);
    static void load_syntax (Frame&);
    Precision (const FrameSubmodel& al);
    ~Precision ();
  };
  const std::unique_ptr<Precision> precision;

  // Simulation.
  void common_doIt (Daisy& daisy, double& water, Treelog& msg);
  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  // Create and Destroy.
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy& daisy, const Scope&, Treelog& err) const;
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);
  static void load_syntax (Frame&);
protected:
  ActionFertilize (const BlockModel& al);
  ~ActionFertilize ();
};

bool 
ActionFertilize::Precision::check_alist (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true; 

  const double target = al.number ("target");
  const double from = al.number ("from");
  const double to = al.number ("to");

  if (target <= 0.0)
    {
      err.entry ("You must specify a positive nitrogen target");
      ok = false;
    }
  if (from > 0.0 || to > 0.0)
    {
      err.entry ("You can only measure nitrogen below the ground");
      ok = false;
    }
  if (from < to)
    {
      err.entry ("'from' must be higher than 'to' in"
		 " the measurement area");
      ok = false;
    }
  return ok;
}

void 
ActionFertilize::Precision::load_syntax (Frame& frame)
{
  frame.add_check (check_alist);
  frame.declare ("target", "kg N/ha", Attribute::Const, 
	      "How much N you want.");
  frame.declare ("from", "cm", Attribute::Const, "\
Height where you want to start measuring (a negative number).");
  frame.set ("from", 0.0);
  frame.declare ("to", "cm", Attribute::Const, "\
Height where you want to end measuring (a negative number).");
  frame.set ("to", -100.0);
  frame.order ("target");
}

    
ActionFertilize::Precision::Precision (const FrameSubmodel& al)
  : target (al.number ("target")),
    from (al.number ("from")),
    to (al.number ("to"))
{ }

ActionFertilize::Precision::~Precision ()
{ }

void 
ActionFertilize::common_doIt (Daisy& daisy, double& water, Treelog& msg)
{
  if (precision.get ())
    {
      const double weight 
	= precision->target
	- daisy.field ().soil_inorganic_nitrogen (precision->from, precision->to);
      
      if (weight <= minimum_weight)
	{
	  msg.message ("Not fertilizing due to precision farming");
	  return;
	}
      AM::set_utilized_weight (metalib, *am, weight);
    }
  else if (second_year_compensation)
    {
      const double weight = AM::utilized_weight (metalib, *am);
      const double compensation = daisy.field ().second_year_utilization ();
      daisy.field ().clear_second_year_utilization ();

      if (weight - compensation <= minimum_weight)
	{
	  msg.message ("Not fertilizing due to second year effect");
	  return;
	}
      else
	AM::set_utilized_weight (metalib, *am, weight - compensation);
    }
  else if (minimum_weight > 0.0
	   && minimum_weight > AM::utilized_weight (metalib, *am))
    {
      msg.message ("Not fertilizing due to minimum weight");
      return;
    }

  std::ostringstream tmp;
  if (AM::is_mineral (metalib, *am))
    tmp << "Fertilizing " << am->number ("weight") 
	<< " kg "<< am->type_name () << "-N/ha";
  else if (AM::is_organic (metalib, *am))
    {
      tmp  << "Fertilizing " << am->number ("weight") 
	   << " ton "<< am->type_name () << " ww/ha";
      const double utilized_weight = AM::utilized_weight (metalib, *am);
      if (utilized_weight > 0.0)
        tmp << "; utilized " << utilized_weight << " kg N/ha";
      water = AM::get_water (metalib, *am);
      if (water > 0.0)
        tmp << "; water " << water << " mm";
    }
  else
    tmp << "Fertilizing " << am->type_name ();
  msg.message (tmp.str ());
}

bool 
ActionFertilize::check (const Daisy& daisy, const Scope&, Treelog& err) const
{
  bool ok = true;
  if (!AM::is_mineral (metalib, *am) && !daisy.field ().check_am (*am, err))
    ok = false;
  return ok;
}

ActionFertilize::ActionFertilize (const BlockModel& al)
  : Action (al),
    metalib (al.metalib ()),
    am (&al.model ("am").clone ()),
    second_year_compensation (al.flag ("second_year_compensation")),
    minimum_weight (al.number ("minimum_weight")),
    precision (al.check ("precision") 
	       ? new Precision (al.submodel ("precision"))
	       : NULL)
{ 
  if (al.check ("equivalent_weight"))
    AM::set_utilized_weight (al.metalib (), 
                             *am, al.number ("equivalent_weight"));
}

ActionFertilize::~ActionFertilize ()
{ }

static struct ActionFertilizeSyntax : public DeclareBase
{
  static bool check_alist (const Metalib& metalib, 
                           const Frame& al, Treelog& err)
  { 
    bool ok = true;

    bool second_year_compensation = al.flag ("second_year_compensation");
    bool precision = al.check ("precision");
    bool equivalent_weight = al.check ("equivalent_weight");
    const FrameModel& am = al.model ("am");
    bool fertilizer_weight 
      = (am.check ("weight") && am.number ("weight") > 0.0);

    if (second_year_compensation && precision)
      {
        err.entry ("You cannot use 'second_year_compensation' "
                   "with 'precision'");
        ok = false;
      }
    if (fertilizer_weight + equivalent_weight + precision != 1)      {
        err.entry ("You must specify exactly one of 'weight', "
                   "'equivalent_weight' and 'precision'");
        ok = false;
      }

    if (equivalent_weight || precision || second_year_compensation)
      {
        if (!AM::is_mineral (metalib, am))
          {
            if (!am.check ("first_year_utilization"))
              {
                err.entry ("You must specify 'first_year_utilization' for "
                           "the organic fertilizer");
                ok = false;
              }
            if (!am.check ("weight") || !am.check ("total_N_fraction"))
              {
                std::ostringstream tmp;
                tmp  << "You cannot use 'equivalent_weight' with "
                     << am.type_name () << " fertilizer";
                err.entry (tmp.str ());
              }
          }
      }
    return ok;
  }

  ActionFertilizeSyntax ()
    : DeclareBase (Action::component, "fertilize_base", "\
Shared parameters for all fertilize actions.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_object ("am", AM::component, "\
The fertilizer you want to apply.");
    frame.declare ("equivalent_weight", "kg N/ha", Check::non_negative (),
                Attribute::OptionalConst, 
                "\
When fertilizing with organic matter, you may let Daisy calculate the\n\
amount of dry matter that corresponds to the specified amount of\n\
nitrogen.  This requires that the fertilizer has specified the\n\
'first_year_utilization' parameter, but not the 'weight' parameter.");
    frame.declare ("minimum_weight", "kg N/ha", Check::non_negative (),
                Attribute::Const,
                "Minimum amount of nitrogen to fertilize with.");
    frame.set ("minimum_weight", 0.0);
    frame.declare_submodule ("precision", Attribute::OptionalConst, "\
Let the amount of fertilizer depend on the inorganic nitrogen in the soil.\n\
The amount of fertilizer will be the specified 'target', minus the amount\n\
already present in the soil zone between 'from' and 'to'.",
                          &ActionFertilize::Precision::load_syntax);
    frame.declare_boolean ("second_year_compensation", Attribute::Const, "\
Compensate for the second year effect of previous fertilizations.\n\
The second year effect is solely governed by the 'second_year_utilization'\n\
organic fertilizer parameter.  The second year effect does not fade with\n\
time, but is zeroed once you fertilize with this flag set.");
    frame.set ("second_year_compensation", false);
  }
} ActionFertilize_init;

// Surface fertilizer.

struct ActionFertilizeSurface : public ActionFertilize
{
  // Parameters.
  const double from;
  const double to;
  boost::shared_ptr<Volume> volume;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog&);

  // Create and destroy.
  ActionFertilizeSurface (const BlockModel& al)
    : ActionFertilize (al),
      from (al.number ("from")),
      to (al.number ("to")),
      volume (Librarian::build_stock<Volume> (al.metalib (), al.msg (),
                                              "box", objid))
  {  
    if (to < from)
      {
        volume->limit_top (from);
        volume->limit_bottom (to);
      }
  }
  ~ActionFertilizeSurface ()
  { }
};

void 
ActionFertilizeSurface::doIt (Daisy& daisy, const Scope&, Treelog& msg)
{
  const Units& units = daisy.units ();
  double water = 0.0;
  common_doIt (daisy, water, msg);
  const double duration = 0.1;  // [h]
  if (to < from)
    {
      daisy.field ().fertilize (metalib, *am, from, to, 
                              daisy.time (), msg);
      if (water > 0.0)
        daisy.field ().irrigate (duration, water / duration,
                               Irrigation::at_air_temperature,
                               Irrigation::subsoil,
                               IM (units.get_unit (IM::solute_unit ())),
                               volume, true, msg);
    }
  else
    {
      daisy.field ().fertilize (metalib, *am, daisy.time (), msg);
      if (water > 0.0)
        daisy.field ().irrigate (duration, water / duration,
                               Irrigation::at_air_temperature,
                               Irrigation::surface,
                               IM (units.get_unit (IM::solute_unit ())),
                               boost::shared_ptr<Volume> (), true, msg);
    }
}

static struct ActionFertilizeSurfaceSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionFertilizeSurface (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  { 
    bool ok = true;
    const double from = al.number ("from");
    const double to = al.number ("to");
    if (from > 0.0 || to > 0.0)
      {
	err.entry ("You can only fertilize on or below the ground");
	ok = false;
      }
    if (from < to)
      {
	err.entry ("'from' must be higher than 'to' in"
		   " the fertilization area");
	ok = false;
      }
    return ok;
  }

  ActionFertilizeSurfaceSyntax ()
    : DeclareModel (Action::component, "fertilize", "fertilize_base", "\
Apply fertilizer to the soil surface.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);

    frame.declare ("from", "cm", Check::non_positive (), Attribute::Const, "\
Height where you want to start the incorporation (a negative number)\n\
OBSOLETE:  Use 'fertilize_incorporate' instead.");
    frame.set ("from", 0.0);
    frame.declare ("to", "cm", Check::non_positive (), Attribute::Const, "\
Height where you want to end the incorporation (a negative number)\n\
OBSOLETE:  Use 'fertilize_incorporate' instead.");
    frame.set ("to", 0.0);
    frame.order ("am");
  }
} ActionFertilizeSurface_syntax;

// Incorporate fertilizer.

struct ActionFertilizeIncorporate : public ActionFertilize
{
  // Parameters.
  boost::shared_ptr<Volume> volume;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog&);

  // Create and destroy.
  ActionFertilizeIncorporate (const BlockModel& al)
    : ActionFertilize (al),
      volume (Librarian::build_item<Volume> (al, "volume"))
  {  }
  ~ActionFertilizeIncorporate ()
  { }
};

void 
ActionFertilizeIncorporate::doIt (Daisy& daisy, const Scope&, Treelog& msg)
{
  const Units& units = daisy.units ();
  double water = 0.0;
  common_doIt (daisy, water, msg);
  const double duration = 0.1;  // [h]

  daisy.field ().fertilize (metalib, *am, *volume,
                          daisy.time (), msg);
  if (water > 0.0)
    daisy.field ().irrigate (duration, water / duration,
                           Irrigation::at_air_temperature,
                           Irrigation::subsoil,
                           IM (units.get_unit (IM::solute_unit ())),
                           volume, true, msg);
}

static struct ActionFertilizeIncorporateSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionFertilizeIncorporate (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  { 
    bool ok = true;
    return ok;
  }

  ActionFertilizeIncorporateSyntax ()
    : DeclareModel (Action::component, "incorporate_fertilizer", 
                    "fertilize_base", "Incorporate fertilizer.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare_object ("volume", Volume::component, 
                       Attribute::Const, Attribute::Singleton,
                       "Soil volume to incorporate fertilizer in.");
    frame.order ("am");
  }
} ActionFertilizeIncorporate_syntax;

// action_fertilize.C ends here.
