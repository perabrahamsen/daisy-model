// action_irrigate.C
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
#include "scope.h"
#include "block_model.h"
#include "daisy.h"
#include "chemical.h"
#include "number.h"
#include "units.h"
#include "field.h"
#include "im.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "volume.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>

// #define NEW_IRRIGATE

struct ActionIrrigate : public Action
{
  const int days;
  const int hours;
  const double remaining_time;

  const std::unique_ptr<Number> expr_flux;
  double flux;
  const double temp;
  const IM sm;
  
  static const symbol mm_per_h;

  virtual void irrigate (Field&, double flux, double temp, const IM&, 
                         double dt, Treelog& msg) const = 0;

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    expr_flux->initialize (daisy.units (), scope, msg);
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    bool ok = true;
    if (!expr_flux->check_dim (daisy.units (), scope, mm_per_h, msg))
      ok = false;
    return ok;
  }

  void tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
  { 
    if (!expr_flux->tick_value (daisy.units (), flux, mm_per_h, scope, msg))
      flux = 0.0;
  }
  
  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    irrigate (daisy.field (), flux, temp, sm, remaining_time, msg);
  }

  ActionIrrigate (const BlockModel& al)
    : Action (al),
      days (al.integer ("days")),
      hours (al.integer ("hours", (days > 0) ? 0 : 1)),
      remaining_time (al.number ("remaining_time", days * 24.0 + hours)),
      expr_flux (Librarian::build_item<Number> (al, "flux")),
      flux (-42.42e42),
      temp (al.number ("temperature", Irrigation::at_air_temperature)),
      sm (al, "solute")
  { }
  ~ActionIrrigate ()
  { }
};

static struct ActionIrrigateBaseSyntax : public DeclareBase
{
  ActionIrrigateBaseSyntax ()
    : DeclareBase (Action::component, "irrigate_base", "\
Shared parameter for irrigate actions.")
  { }

  static bool check_alist (const Metalib&, const Frame& alist, Treelog& err)
  {
    bool ok = true;

    const int days = alist.integer ("days");
    const int hours = alist.integer ("hours", (days > 0) ? 0 : 1);

    if (days * 24 + hours < 1)
      {
	err.entry ("you must irrigate at least 1 hour");
	ok = false;
      }
    return ok;
  }

  static void load_ppm (Frame& frame)
  { IM::add_syntax (frame, Attribute::Const, Units::ppm ()); }

  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);	
    frame.declare_integer ("days", Attribute::Const, 
                "Irrigate this number of days.");
    frame.set ("days", 0);
    frame.declare_integer ("hours", Attribute::OptionalConst, 
                "Irrigate this number of hours.\n\
By default, irrigate 1 hour if days is 0, and 0 hours plus the specified\n\
number of days else.");
    frame.declare ("remaining_time", "h", Attribute::OptionalState,
                "Irrigate this number of hours.\
Setting this overrides the 'days' and 'hours' parameters.");
    frame.declare_object ("flux", Number::component, 
                       Attribute::Const, Attribute::Singleton, 
"Amount of irrigation applied.");
    frame.order ("flux");
    frame.declare ("temperature", "dg C", 
		Check::positive (), Attribute::OptionalConst,
		"Temperature of irrigation (default: air temperature).");
    frame.declare_submodule_sequence ("solute", Attribute::Const, "\
Solutes in irrigation water.", load_ppm);
    frame.set_empty ("solute");
  }
} ActionIrrigateBase_syntax;

const symbol ActionIrrigate::mm_per_h ("mm/h");

struct ActionIrrigateOverhead : public ActionIrrigate
{
  void irrigate (Field& f, const double flux, const double temp, const IM& im, 
                 const double dt, Treelog& msg) const
  { 
    f.irrigate (dt, flux, temp, Irrigation::overhead, im, 
                boost::shared_ptr<Volume> (), false, msg);
  }
  ActionIrrigateOverhead (const BlockModel& al)
    : ActionIrrigate (al)
  { }
};

struct ActionIrrigateSurface : public ActionIrrigate
{
  void irrigate (Field& f, const double flux, const double temp, const IM& im, 
                 const double dt, Treelog& msg) const
  {
    f.irrigate (dt, flux, temp, Irrigation::surface, im, 
                boost::shared_ptr<Volume> (), false, msg);
  }
  ActionIrrigateSurface (const BlockModel& al)
    : ActionIrrigate (al)
  { }
};

struct ActionIrrigateSubsoil : public ActionIrrigate
{
  boost::shared_ptr<Volume> volume;

  void irrigate (Field& f, const double flux, const double /* temp */, 
                 const IM& im, const double dt, Treelog& msg) const
  { 
    f.irrigate (dt, flux, Irrigation::at_air_temperature, 
                Irrigation::subsoil, im, volume, false, msg);
  }
  ActionIrrigateSubsoil (const BlockModel& al)
    : ActionIrrigate (al),
      volume (Volume::build_obsolete (al).release ())
  { }
};

static struct ActionIrrigateOverheadSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionIrrigateOverhead (al); }
  ActionIrrigateOverheadSyntax ()
    : DeclareModel (Action::component, "irrigate_overhead", "irrigate_base", "\
Irrigate the field from above.")
  { }
  void load_frame (Frame&) const
  { }
} ActionIrrigateOverhead_syntax;

static struct ActionIrrigateSurfaceSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionIrrigateSurface (al); }
  ActionIrrigateSurfaceSyntax ()
    : DeclareModel (Action::component, "irrigate_surface", "irrigate_base", "\
Irrigate the field directly on the soil surface, bypassing the canopy.")
  { }
  void load_frame (Frame&) const
  { }
} ActionIrrigateSurface_syntax;

static struct ActionIrrigateSubsoilSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionIrrigateSubsoil (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  { 
    bool ok = true;
    if (al.check ("from") && al.check ("to"))
      {
        const double from = al.number ("from");
        const double to = al.number ("to");
        if (from <= to)
          {
            err.entry ("'from' must be higher than 'to' in"
                       " the subsoil irrigation zone");
            ok = false;
          }
      }
    return ok;
  }

  ActionIrrigateSubsoilSyntax ()
    : DeclareModel (Action::component, "irrigate_subsoil", "irrigate_base", "\
Irrigate the field directly into the soil.\n\
Currently, the 'temperature' parameter is ignored.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);	

    frame.declare_object ("volume", Volume::component, 
                       Attribute::Const, Attribute::Singleton,
                       "Soil volume to add irritaion.");
    frame.set ("volume", "box");
    frame.declare ("from", "cm", Check::non_positive (), Attribute::OptionalConst, "\
Height where you want to start the incorporation (a negative number).\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    frame.declare ("to", "cm", Check::non_positive (), Attribute::OptionalConst, "\
Height where you want to end the incorporation (a negative number).\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
  }
} ActionIrrigateSubsoil_syntax;

// action_irrigate.C ends here.
