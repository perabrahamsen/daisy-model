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


#include "action.h"
#include "daisy.h"
#include "field.h"
#include "im.h"
#include "check.h"
#include "tmpstream.h"

struct ActionIrrigate : public Action
{
  const int days;
  const int hours;
  bool activated;
  Time end_time;

  static const double at_air_temperature;

  const double flux;
  const double temp;
  const IM sm;
  
  virtual void irrigate (Field&,
			 double flux, double temp, const IM&) const = 0;

  void doIt (Daisy& daisy, Treelog& out)
  {
    if (!activated)
      {
	activated = true;
	end_time = daisy.time;
	end_time.tick_hour (hours - 1);
	end_time.tick_day (days);
        TmpStream tmp;
        if (daisy.time == end_time)
          tmp () << "Irrigating " << flux << " mm";
        else
          tmp () << "Irrigating " << flux << " mm/h";
        out.message (tmp.str ());      
      }
    else if (daisy.time == end_time)
      out.message ("Irrating done");

    irrigate (daisy.field, flux, temp, sm);
  }

  bool done (const Daisy& daisy) const
  {
    daisy_assert (activated);
    return daisy.time >= end_time; 
  }

  static bool check_alist (const AttributeList& alist, Treelog& err)
  {
    bool ok = true;

    const int days = alist.integer ("days");
    const int hours = alist.integer ("hours");

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
    syntax.add ("hours", Syntax::Integer, Syntax::Const, 
                "Irrigate this number of days.");
    alist.add ("hours", 1);
    syntax.add_submodule ("end_time", alist, Syntax::OptionalState,
                          "Irrigate until this date.\
Setting this overrides the 'days' and 'hours' parameters.", Time::load_syntax);
    syntax.add ("flux", "mm/h", Check::non_negative (), Syntax::Const, 
		"Amount of irrigation applied.");
    syntax.order ("flux");
    syntax.add ("temperature", "dg C", 
		Check::positive (), Syntax::OptionalConst,
		"Temperature of irrigation (default: air temperature).");
    syntax.add_submodule ("solute", alist, Syntax::Const, "\
Nitrogen content of irrigation water [mg N/l] (default: none).",
			  IM::load_ppm);
  }

  ActionIrrigate (const AttributeList& al)
    : Action (al),
      days (al.integer ("days")),
      hours (al.integer ("hours")),
      activated (al.check ("end_time")),
      end_time (1, 1, 1, 1),
      flux (al.number ("flux")),
      temp (al.check ("temperature") 
	    ? al.number ("temperature")
	    : at_air_temperature),
      sm (al.alist ("solute"))
  { }
  ~ActionIrrigate ()
  { }
};

const double ActionIrrigate::at_air_temperature = -500;

struct ActionIrrigateOverhead : public ActionIrrigate
{
  void irrigate (Field& f, double flux, double temp, const IM& im) const
  { 
    if (temp == at_air_temperature)
      f.irrigate_overhead (flux, im); 
    else
      f.irrigate_overhead (flux, temp, im); 
  }
  ActionIrrigateOverhead (const AttributeList& al)
    : ActionIrrigate (al)
  { }
};

struct ActionIrrigateSurface : public ActionIrrigate
{
  void irrigate (Field& f, double flux, double temp, const IM& im) const
  {
    if (temp == at_air_temperature)
      f.irrigate_surface (flux, im);
    else
      f.irrigate_surface (flux, temp, im); 
  }
  ActionIrrigateSurface (const AttributeList& al)
    : ActionIrrigate (al)
  { }
};

struct ActionIrrigateSubsoil : public ActionIrrigate
{
  const double from;
  const double to;

  void irrigate (Field& f, double flux, double /* temp */, const IM& im) const
  { f.irrigate_subsoil (flux, im, from, to); }
  ActionIrrigateSubsoil (const AttributeList& al)
    : ActionIrrigate (al),
      from (al.number ("from")),
      to (al.number ("to"))
  { }
};

struct ActionIrrigateSubsoilStart : public Action
{
  const double flux;
  const double from;
  const double to;
  const IM sm;

  void doIt (Daisy& daisy, Treelog& out)
  {
    daisy.field.set_subsoil_irrigation (flux, sm, from, to);
    TmpStream tmp;
    if (flux != 0.0)
      tmp () << "Subsoil irrigating with " << flux << " mm/h";
    else
      tmp () << "Subsoil irrigating turned off";
    out.message (tmp.str ());
  }

  ActionIrrigateSubsoilStart (const AttributeList& al)
    : Action (al),
      flux (al.number ("flux")),
      from (al.number ("from")),
      to (al.number ("to")),
      sm (al.alist ("solute"))
  { }
  ~ActionIrrigateSubsoilStart ()
  { }
};

struct ActionIrrigateStop : public Action
{
  void doIt (Daisy& daisy, Treelog& out)
  {
    IM sm;
    daisy.field.set_subsoil_irrigation (0.0, sm, 0.0, -0.1);
    out.message ("Subsoil irrigating turned off");
  }
  ActionIrrigateStop (const AttributeList& al)
    : Action (al)
  { }
  ~ActionIrrigateStop ()
  { }
};

static struct ActionIrrigateOverheadSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateOverhead (al); }
  ActionIrrigateOverheadSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    alist.add ("description", "\
Irrigate the field from above.");
    Librarian<Action>::add_type ("irrigate_overhead", alist, syntax, &make);
  }
} ActionIrrigateOverhead_syntax;

static struct ActionIrrigateSurfaceSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateSurface (al); }
  ActionIrrigateSurfaceSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    alist.add ("description", "\
Irrigate the field directly on the soil surface, bypassing the canopy.");
    Librarian<Action>::add_type ("irrigate_surface", alist, syntax, &make);
  }
} ActionIrrigateSurface_syntax;

static struct ActionIrrigateTopSyntax
{
  static Action& make (const AttributeList& al)
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
    Librarian<Action>::add_type ("irrigate_top", alist, syntax, &make);
  }
} ActionIrrigateTop_syntax;

static struct ActionIrrigateSubsoilSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateSubsoil (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  { 
    bool ok = true;
    const double from = al.number ("from");
    const double to = al.number ("to");
    if (from <= to)
      {
	err.entry ("'from' must be higher than 'to' in"
		   " the subsoil irrigation zone");
	ok = false;
      }
    return ok;
  }

  ActionIrrigateSubsoilSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    alist.add ("description", "\
Irrigate the field directly into the soil.\n\
Currently, the 'temperature' parameter is ignored.");
    syntax.add ("from", "cm", Check::non_positive (), Syntax::Const, "\
Height where you want to start the incorporation (a negative number).");
    alist.add ("from", 0.0);
    syntax.add ("to", "cm", Check::non_positive (), Syntax::Const, "\
Height where you want to end the incorporation (a negative number).");

    Librarian<Action>::add_type ("irrigate_subsoil", alist, syntax, &make);
  }

} ActionIrrigateSubsoil_syntax;

static struct ActionIrrigateSubsoilStartSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateSubsoilStart (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  { 
    bool ok = true;
    const double from = al.number ("from");
    const double to = al.number ("to");
    if (from <= to)
      {
	err.entry ("'from' must be higher than 'to' in"
		   " the subsoilirrigation zone");
	ok = false;
      }
    return ok;
  }

  ActionIrrigateSubsoilStartSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (&check_alist);
    alist.add ("description", "\
Incorporate irrigation water directly in the soil.\n\
This command specifies the flux, set it to zero to turn off irrigation.");
    syntax.add ("flux", "mm/h", Check::non_negative (), Syntax::Const, 
		"Amount of irrigation applied.");
    syntax.order ("flux");
    syntax.add ("from", "cm", Check::non_positive (), Syntax::Const, "\
Height where you want to start the incorporation (a negative number).");
    alist.add ("from", 0.0);
    syntax.add ("to", "cm", Check::non_positive (), Syntax::Const, "\
Height where you want to end the incorporation (a negative number).");

    syntax.add_submodule ("solute", alist, Syntax::Const, "\
Nitrogen content of irrigation water [mg N/l] (default: none).",
			  IM::load_ppm);

    Librarian<Action>::add_type ("set_subsoil_irrigation",
				 alist, syntax, &make);
  }
} ActionIrrigateSubsoilStart_syntax;

static struct ActionIrrigateStopSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateStop (al); }

  ActionIrrigateStopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Stop subsoil irrigation.");
    Librarian<Action>::add_type ("stop_subsoil_irrigation",
				 alist, syntax, &make);
  }
} ActionIrrigateStop_syntax;
