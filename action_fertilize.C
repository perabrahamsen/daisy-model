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
#include "block.h"
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
  // Parameters.
  std::auto_ptr<FrameModel> am;
  const bool second_year_compensation;
  const double minimum_weight;

  struct Precision
  {
    // Parameters.
    const double target;
    const double from;
    const double to;
    
    // Create and Destroy.
    static bool check_alist (Metalib&, const Frame& al, Treelog& err);
    static void load_syntax (Frame&);
    Precision (const FrameSubmodel& al);
    ~Precision ();
  };
  const std::auto_ptr<Precision> precision;

  // Simulation.
  void common_doIt (Daisy& daisy, double& water, Treelog& msg);
  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  // Create and Destroy.
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy& daisy, const Scope&, Treelog& err) const;
  static bool check_alist (Metalib&, const Frame& al, Treelog& err);
  static void load_syntax (Frame&);
protected:
  ActionFertilize (Block& al);
  ~ActionFertilize ();
};

bool 
ActionFertilize::Precision::check_alist (Metalib&, const Frame& al, Treelog& err)
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
  frame.declare ("target", "kg N/ha", Value::Const, 
	      "How much N you want.");
  frame.declare ("from", "cm", Value::Const, "\
Height where you want to start measuring (a negative number).");
  frame.set ("from", 0.0);
  frame.declare ("to", "cm", Value::Const, "\
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
  Metalib& metalib = daisy.metalib;
  if (precision.get ())
    {
      const double weight 
	= precision->target
	- daisy.field->soil_inorganic_nitrogen (precision->from, precision->to);
      
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
      const double compensation = daisy.field->second_year_utilization ();
      daisy.field->clear_second_year_utilization ();

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
  if (!AM::is_mineral (daisy.metalib, *am) && !daisy.field->check_am (*am, err))
    ok = false;
  return ok;
}

ActionFertilize::ActionFertilize (Block& al)
  : Action (al),
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
  static bool check_alist (Metalib& metalib, 
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
                Value::OptionalConst, 
                "\
When fertilizing with organic matter, you may let Daisy calculate the\n\
amount of dry matter that corresponds to the specified amount of\n\
nitrogen.  This requires that the fertilizer has specified the\n\
'first_year_utilization' parameter, but not the 'weight' parameter.");
    frame.declare ("minimum_weight", "kg N/ha", Check::non_negative (),
                Value::Const,
                "Minimum amount of nitrogen to fertilize with.");
    frame.set ("minimum_weight", 0.0);
    frame.declare_submodule ("precision", Value::OptionalConst, "\
Let the amount of fertilizer depend on the inorganic nitrogen in the soil.\n\
The amount of fertilizer will be the specified 'target', minus the amount\n\
already present in the soil zone between 'from' and 'to'.",
                          &ActionFertilize::Precision::load_syntax);
    frame.declare ("second_year_compensation", Value::Boolean, Value::Const, "\
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

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog&);

  // Create and destroy.
  ActionFertilizeSurface (Block& al)
    : ActionFertilize (al),
      from (al.number ("from")),
      to (al.number ("to"))
  {  }
  ~ActionFertilizeSurface ()
  { }
};

void 
ActionFertilizeSurface::doIt (Daisy& daisy, const Scope&, Treelog& msg)
{
  const Units& units = daisy.units ();
  double water = 0.0;
  common_doIt (daisy, water, msg);

  if (to < from)
    {
      daisy.field->fertilize (daisy.metalib, *am, from, to, 
                              daisy.time, daisy.dt, msg);
      if (water > 0.0)
        daisy.field->irrigate_subsoil (water,
                                       IM (units.get_unit (IM::solute_unit ())),
                                       from, to, daisy.dt, msg);
    }
  else
    {
      daisy.field->fertilize (daisy.metalib, *am, daisy.time, daisy.dt, msg);
      if (water > 0.0)
        daisy.field->irrigate_surface (water, 
                                       IM (units.get_unit (IM::solute_unit ())),
                                       daisy.dt, msg);
    }
}

static struct ActionFertilizeSurfaceSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionFertilizeSurface (al); }

  static bool check_alist (Metalib&, const Frame& al, Treelog& err)
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

    frame.declare ("from", "cm", Check::non_positive (), Value::Const, "\
Height where you want to start the incorporation (a negative number)\n\
OBSOLETE:  Use 'fertilize_incorporate' instead.");
    frame.set ("from", 0.0);
    frame.declare ("to", "cm", Check::non_positive (), Value::Const, "\
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
  std::auto_ptr<Volume> volume;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog&);

  // Create and destroy.
  ActionFertilizeIncorporate (Block& al)
    : ActionFertilize (al),
      volume (Librarian::build_item<Volume> (al, "volume"))
  {  }
  ~ActionFertilizeIncorporate ()
  { }
};

void 
ActionFertilizeIncorporate::doIt (Daisy& daisy, const Scope&, Treelog& msg)
{
  double water = 0.0;
  common_doIt (daisy, water, msg);

  daisy.field->fertilize (daisy.metalib, *am, *volume,
                          daisy.time, daisy.dt, msg);
  if (water > 0.0)
    daisy.field->irrigate_subsoil (water,
                                   IM (daisy.units ()
                                       .get_unit (IM::solute_unit ())),
                                   *volume, daisy.dt, msg);
}

static struct ActionFertilizeIncorporateSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ActionFertilizeIncorporate (al); }

  static bool check_alist (Metalib&, const Frame& al, Treelog& err)
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
                       Value::Const, Value::Singleton,
                       "Soil volume to incorporate fertilizer in.");
    frame.order ("am");
  }
} ActionFertilizeIncorporate_syntax;

// action_fertilize.C ends here.
