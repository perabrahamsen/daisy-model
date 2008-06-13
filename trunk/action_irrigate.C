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
#include "block.h"
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
#include <sstream>

struct ActionIrrigate : public Action
{
  const int days;
  const int hours;
  bool activated;
  double remaining_time;

  static const double at_air_temperature;

  const std::auto_ptr<Number> expr_flux;
  double flux;
  const double temp;
  const IM sm;
  
  static const symbol mm_per_h;

  virtual void irrigate (Field&, double flux, double temp, const IM&, 
                         double dt, Treelog& msg) const = 0;

  void initialize (const Daisy&, const Scope&, Treelog& msg)
  { 
    expr_flux->initialize (msg);
  }

  bool check (const Daisy&, const Scope& scope, Treelog& msg) const
  {
    bool ok = true;
    if (!expr_flux->check_dim (scope, mm_per_h, msg))
      ok = false;
    return ok;
  }

  void tick (const Daisy&, const Scope& scope, Treelog& msg)
  { 
    if (!expr_flux->tick_value (flux, mm_per_h, scope, msg))
      flux = 0.0;
  }
  
  void doIt (Daisy& daisy, const Scope&, Treelog& msg)
  {
    if (!activated)
      {
	activated = true;
        remaining_time = days * 24 + hours ;
        std::ostringstream tmp;
        tmp << "Irrigating " << flux << " mm/h for "
            << remaining_time << " hour";
        if (!approximate (remaining_time, 1.0))
          tmp << "s";
	static const symbol conc_flux_unit ("kg/ha/mm");
        const double N = (sm.get_value (Chemical::NO3 (), conc_flux_unit)
			  + sm.get_value (Chemical::NH4 (), conc_flux_unit))
	  * flux * (days * 24 + hours);
        if (N > 1e-10)
          tmp << "; " << N << " kg N/ha";
	for (IM::const_iterator i = sm.begin (); i != sm.end (); i++)
	  {
	    const symbol chem = *i;
	    if (chem == Chemical::NO3 () || chem == Chemical::NH4 ())
	      continue;
	    const double value = sm.get_value (chem, conc_flux_unit)
	      * flux * (days * 24 + hours) * 1000.0 /* [g/kg] */;
	    if (!std::isnormal (value))
	      continue;
	    tmp << "; " << value << " g " << chem << "/ha";
	  }
			  
        msg.message (tmp.str ());      
      }
    const double dt = daisy.dt;
    double this_flux = flux;
    if (remaining_time < dt * 1.001)
      {
        if (remaining_time < dt * 0.999)
          this_flux *= remaining_time / dt;

        remaining_time = 0.0;
        msg.message ("Irrigating done");
      }
    else 
      remaining_time -= dt;
    daisy_assert (std::isnormal (this_flux));
    irrigate (*daisy.field, this_flux, temp, sm, daisy.dt, msg);
  }

  bool done (const Daisy& daisy, const Scope&, Treelog&) const
  {
    daisy_assert (activated);
    return remaining_time < daisy.dt * 0.0001; 
  }

  static bool check_alist (const AttributeList& alist, Treelog& err)
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

  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add_check (check_alist);	
    syntax.add ("days", Syntax::Integer, Syntax::Const, 
                "Irrigate this number of days.");
    alist.add ("days", 0);
    syntax.add ("hours", Syntax::Integer, Syntax::OptionalConst, 
                "Irrigate this number of hours.\n\
By default, irrigate 1 hour if days is 0, and 0 hours plus the specified\n\
number of days else.");
    syntax.add ("remaining_time", "h", Syntax::OptionalState,
                "Irrigate this number of hours.\
Setting this overrides the 'days' and 'hours' parameters.");
    syntax.add_object ("flux", Number::component, 
                       Syntax::Const, Syntax::Singleton, 
"Amount of irrigation applied.");
    syntax.order ("flux");
    syntax.add ("temperature", "dg C", 
		Check::positive (), Syntax::OptionalConst,
		"Temperature of irrigation (default: air temperature).");
    IM::add_syntax (syntax, alist, Syntax::Const, "solute", Units::ppm (), 
		    "Solutes in irrigation water.");
  }

  ActionIrrigate (Block& al)
    : Action (al),
      days (al.integer ("days")),
      hours (al.integer ("hours", (days > 0) ? 0 : 1)),
      activated (al.check ("remaining_time")),
      remaining_time (al.number ("remaining_time", 0.0)),
      expr_flux (Librarian::build_item<Number> (al, "flux")),
      flux (-42.42e42),
      temp (al.number ("temperature", at_air_temperature)),
      sm (al, "solute")
  { }
  ~ActionIrrigate ()
  { }
};

const double ActionIrrigate::at_air_temperature = -500;

const symbol ActionIrrigate::mm_per_h ("mm/h");

struct ActionIrrigateOverhead : public ActionIrrigate
{
  void irrigate (Field& f, const double flux, const double temp, const IM& im, 
                 const double dt, Treelog& msg) const
  { 
    if (approximate (temp, at_air_temperature))
      f.irrigate_overhead (flux, im, dt, msg); 
    else
      f.irrigate_overhead (flux, temp, im, dt, msg); 
  }
  ActionIrrigateOverhead (Block& al)
    : ActionIrrigate (al)
  { }
};

struct ActionIrrigateSurface : public ActionIrrigate
{
  void irrigate (Field& f, const double flux, const double temp, const IM& im, 
                 const double dt, Treelog& msg) const
  {
    if (approximate (temp, at_air_temperature))
      f.irrigate_surface (flux, im, dt, msg);
    else
      f.irrigate_surface (flux, temp, im, dt, msg); 
  }
  ActionIrrigateSurface (Block& al)
    : ActionIrrigate (al)
  { }
};

struct ActionIrrigateSubsoil : public ActionIrrigate
{
  std::auto_ptr<Volume> volume;

  void irrigate (Field& f, const double flux, const double /* temp */, 
                 const IM& im, const double dt, Treelog& msg) const
  { f.irrigate_subsoil (flux, im, *volume, dt, msg); }
  ActionIrrigateSubsoil (Block& al)
    : ActionIrrigate (al),
      volume (Volume::build_obsolete (al))
  { }
};

static struct ActionIrrigateOverheadSyntax
{
  static Model& make (Block& al)
  { return *new ActionIrrigateOverhead (al); }
  ActionIrrigateOverheadSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    alist.add ("description", "\
Irrigate the field from above.");
    Librarian::add_type (Action::component, "irrigate_overhead", alist, syntax, &make);
  }
} ActionIrrigateOverhead_syntax;

static struct ActionIrrigateSurfaceSyntax
{
  static Model& make (Block& al)
  { return *new ActionIrrigateSurface (al); }
  ActionIrrigateSurfaceSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    alist.add ("description", "\
Irrigate the field directly on the soil surface, bypassing the canopy.");
    Librarian::add_type (Action::component, "irrigate_surface", alist, syntax, &make);
  }
} ActionIrrigateSurface_syntax;

static struct ActionIrrigateTopSyntax
{
  static Model& make (Block& al)
  { return *new ActionIrrigateOverhead (al); }
  static bool check_alist (const AttributeList&, Treelog& err)
  {
    static bool warned = false;
    if (warned)
      return true;
    warned = true;
    err.entry ("OBSOLETE: Use 'irrigate_overhead' instead of 'irrigate_top'");
    return true;
  }
  ActionIrrigateTopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    syntax.add_check (&check_alist);
    alist.add ("description", "\
OBSOLETE.  Use 'irrigate_overhead' instead.");
    Librarian::add_type (Action::component, "irrigate_top", alist, syntax, &make);
  }
} ActionIrrigateTop_syntax;

static struct ActionIrrigateSubsoilSyntax
{
  static Model& make (Block& al)
  { return *new ActionIrrigateSubsoil (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
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
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    syntax.add_check (check_alist);	

    alist.add ("description", "\
Irrigate the field directly into the soil.\n\
Currently, the 'temperature' parameter is ignored.");
    syntax.add_object ("volume", Volume::component, 
                       Syntax::Const, Syntax::Singleton,
                       "Soil volume to add irritaion.");
    alist.add ("volume", Volume::infinite_box ());
    syntax.add ("from", "cm", Check::non_positive (), Syntax::OptionalConst, "\
Height where you want to start the incorporation (a negative number).\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    syntax.add ("to", "cm", Check::non_positive (), Syntax::OptionalConst, "\
Height where you want to end the incorporation (a negative number).\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");

    Librarian::add_type (Action::component, "irrigate_subsoil", alist, syntax, &make);
  }

} ActionIrrigateSubsoil_syntax;

