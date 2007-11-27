// action_fertilize.C
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
#include "am.h"
#include "im.h"
#include "check.h"
#include "assertion.h"
#include "librarian.h"
#include <sstream>

struct ActionFertilize : public Action
{
  // Parameters.
  AttributeList am;
  const double from;
  const double to;
  const bool second_year_compensation;
  const double minimum_weight;

  struct Precision
  {
    // Parameters.
    const double target;
    const double from;
    const double to;
    
    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog& err);
    static void load_syntax (Syntax& syntax, AttributeList& alist);
    Precision (const AttributeList& al);
    ~Precision ();
  };
  Precision *const precision;

  // Simulation.
  void doIt (Daisy& daisy, const Scope&, Treelog&);
  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  // Create and Destroy.
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy& daisy, const Scope&, Treelog& err) const;
  ActionFertilize (Block& al);
  ~ActionFertilize ();
};

bool 
ActionFertilize::Precision::check_alist (const AttributeList& al, Treelog& err)
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
ActionFertilize::Precision::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (&check_alist);
  syntax.add ("target", "kg N/ha", Syntax::Const, 
	      "How much N you want.");
  syntax.add ("from", "cm", Syntax::Const, "\
Height where you want to start measuring (a negative number).");
  alist.add ("from", 0.0);
  syntax.add ("to", "cm", Syntax::Const, "\
Height where you want to end measuring (a negative number).");
  alist.add ("to", -100.0);
  syntax.order ("target");
}

    
ActionFertilize::Precision::Precision (const AttributeList& al)
  : target (al.number ("target")),
    from (al.number ("from")),
    to (al.number ("to"))
{ }

ActionFertilize::Precision::~Precision ()
{ }

void 
ActionFertilize::doIt (Daisy& daisy, const Scope&, Treelog& msg)
{
  if (precision)
    {
      const double weight 
	= precision->target
	- daisy.field->soil_inorganic_nitrogen (precision->from, precision->to);
      
      if (weight <= minimum_weight)
	{
	  msg.message ("Not fertilizing due to precision farming");
	  return;
	}
      AM::set_utilized_weight (am, weight);
    }
  else if (second_year_compensation)
    {
      const double weight = AM::utilized_weight (am);
      const double compensation = daisy.field->second_year_utilization ();
      daisy.field->clear_second_year_utilization ();

      if (weight - compensation <= minimum_weight)
	{
	  msg.message ("Not fertilizing due to second year effect");
	  return;
	}
      else
	AM::set_utilized_weight (am, weight - compensation);
    }
  else if (minimum_weight > 0.0
	   && minimum_weight > AM::utilized_weight (am))
    {
      msg.message ("Not fertilizing due to minimum weight");
      return;
    }

  double water = 0.0;

  const std::string syntax = am.name ("syntax");
  std::ostringstream tmp;
  if (syntax == "mineral")
    tmp << "Fertilizing " << am.number ("weight") 
	<< " kg "<< am.name ("type") << "-N/ha";
  else if (syntax == "organic")
    {
      tmp  << "Fertilizing " << am.number ("weight") 
	   << " ton "<< am.name ("type") << " ww/ha";
      const double utilized_weight = AM::utilized_weight (am);
      if (utilized_weight > 0.0)
        tmp << "; utilized " << utilized_weight << " kg N/ha";
      water = AM::get_water (am);
      if (water > 0.0)
        tmp << "; water " << water << " mm";
    }
  else
    tmp << "Fertilizing " << am.name ("type");
  msg.message (tmp.str ());
  if (syntax != "mineral")
    {
      AttributeList new_time;
      daisy.time.set_alist (new_time);
      am.add ("creation", new_time);
    }

  if (to < from)
    {
      daisy.field->fertilize (am, from, to, daisy.dt, msg);
      if (water > 0.0)
        daisy.field->irrigate_subsoil (water, IM (), from, to, daisy.dt, msg);
    }
  else
    {
      daisy.field->fertilize (am, daisy.dt, msg);
      if (water > 0.0)
        daisy.field->irrigate_surface (water, IM (), daisy.dt, msg);
    }
}

bool 
ActionFertilize::check (const Daisy& daisy, const Scope&, Treelog& err) const
{
  bool ok = true;
  if (am.name ("syntax") != "mineral" && !daisy.field->check_am (am, err))
    ok = false;
  return ok;
}

ActionFertilize::ActionFertilize (Block& al)
  : Action (al),
    am (al.alist ("am")),
    from (al.number ("from")),
    to (al.number ("to")),
    second_year_compensation (al.flag ("second_year_compensation")),
    minimum_weight (al.number ("minimum_weight")),
    precision (al.check ("precision") 
	       ? new Precision (al.alist ("precision"))
	       : NULL)
{ 
  daisy_assert (am.check ("syntax")); 
  if (al.check ("equivalent_weight"))
    AM::set_utilized_weight (am, al.number ("equivalent_weight"));
}

ActionFertilize::~ActionFertilize ()
{ 
  if (precision)
    delete precision;
}

static struct ActionFertilizeSyntax
{
  static Model& make (Block& al)
  { return *new ActionFertilize (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
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

    bool second_year_compensation = al.flag ("second_year_compensation");
    bool precision = al.check ("precision");
    bool equivalent_weight = al.check ("equivalent_weight");
    const AttributeList& am = al.alist ("am");
    bool fertilizer_weight 
      = (am.check ("weight") && am.number ("weight") > 0.0);

    if (second_year_compensation && precision)
      {
	err.entry ("You cannot use 'second_year_compensation' "
		   "with 'precision'");
	ok = false;
      }
    if (fertilizer_weight + equivalent_weight + precision != 1)
      {
	err.entry ("You must specify exactly one of 'weight', "
		   "'equivalent_weight' and 'precision'");
	ok = false;
      }

    if (equivalent_weight || precision || second_year_compensation)
      {
	const std::string syntax = am.name ("syntax");
	if (syntax != "mineral")
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
		     << syntax << " fertilizer";
		err.entry (tmp.str ());
	      }
	  }
	
      }
    return ok;
  }

  ActionFertilizeSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Apply fertilizer to the soil.\n\
If you want to incorporate the fertilizer directly in the soil, specify\n\
the 'from' and 'to' parameters.  By default, the fertilizer will be\n\
left on the surface.");
    syntax.add_object ("am", AM::component, "\
The fertilizer you want to apply.");
    syntax.add ("from", "cm", Check::non_positive (), Syntax::Const, "\
Height where you want to start the incorporation (a negative number).");
    alist.add ("from", 0.0);
    syntax.add ("to", "cm", Check::non_positive (), Syntax::Const, "\
Height where you want to end the incorporation (a negative number).");
    alist.add ("to", 0.0);
    syntax.add ("equivalent_weight", "kg N/ha", Check::non_negative (),
		Syntax::OptionalConst, 
		"\
When fertilizing with organic matter, you may let Daisy calculate the\n\
amount of dry matter that corresponds to the specified amount of\n\
nitrogen.  This requires that the fertilizer has specified the\n\
'first_year_utilization' parameter, but not the 'weight' parameter.");
    syntax.add ("minimum_weight", "kg N/ha", Check::non_negative (),
		Syntax::Const,
		"Minimum amount of nitrogen to fertilize with.");
    alist.add ("minimum_weight", 0.0);
    syntax.add_submodule ("precision", alist, 
			  Syntax::OptionalConst, "\
Let the amount of fertilizer depend on the inorganic nitrogen in the soil.\n\
The amount of fertilizer will be the specified 'target', minus the amount\n\
already present in the soil zone between 'from' and 'to'.",
			  &ActionFertilize::Precision::load_syntax);
    syntax.add ("second_year_compensation", Syntax::Boolean, Syntax::Const, "\
Compensate for the second year effect of previous fertilizations.\n\
The second year effect is solely governed by the 'second_year_utilization'\n\
organic fertilizer parameter.  The second year effect does not fade with\n\
time, but is zeroed once you fertilize with this flag set.");
    alist.add ("second_year_compensation", false);

    syntax.order ("am");
    Librarian::add_type (Action::component, "fertilize", alist, syntax, &make);
  }
} ActionFertilize_syntax;
