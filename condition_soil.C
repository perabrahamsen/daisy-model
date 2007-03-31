// condition_soil.C --- Checking soil state.
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

#include "condition.h"
#include "block.h"
#include "alist.h"
#include "field.h"
#include "daisy.h"
#include "check.h"
#include "librarian.h"

struct ConditionSoilTemperature : public Condition
{
  const double temperature;
  const double height;

  bool match (const Daisy& daisy, Treelog&) const
  { 
    if (daisy.field->soil_temperature (height) > temperature)
      return true;
    return false;
  }
  void output (Log&) const
  { }

  ConditionSoilTemperature (Block& al)
    : Condition (al),
      temperature (al.number ("temperature")),
      height (al.number ("height"))
  { }
};

struct ConditionSoilPotential : public Condition
{
  const double potential;
  const double height;

  bool match (const Daisy& daisy, Treelog&) const
  { return (daisy.field->soil_water_potential (height) > potential); }
  void output (Log&) const
  { }

  ConditionSoilPotential (Block& al)
    : Condition (al),
      potential (al.number ("potential")),
      height (al.number ("height"))
  { }
};

struct ConditionSoilWater : public Condition
{
  const double water;		// [mm]
  const double from;		// [cm]
  const double to;		// [cm]

  bool match (const Daisy& daisy, Treelog&) const
  { return (daisy.field->soil_water_content (from, to) * 10 > water); }
  void output (Log&) const
  { }

  ConditionSoilWater (Block& al)
    : Condition (al),
      water (al.number ("water")),
      from (al.number ("from")),
      to (al.number ("to"))
  { }
};

struct ConditionSoilN_min : public Condition
{
  const double amount;		// [mm]
  const double from;		// [cm]
  const double to;		// [cm]

  bool match (const Daisy& daisy, Treelog&) const
  { return (daisy.field->soil_inorganic_nitrogen (from, to)  > amount); }
  void output (Log&) const
  { }

  ConditionSoilN_min (Block& al)
    : Condition (al),
      amount (al.number ("amount")),
      from (al.number ("from")),
      to (al.number ("to"))
  { }
};

static struct ConditionSoilSyntax
{
  static Model& make_temperature (Block& al)
  { return *new ConditionSoilTemperature (al); }
  static Model& make_potential (Block& al)
  { return *new ConditionSoilPotential (al); }
  static Model& make_water (Block& al)
  { return *new ConditionSoilWater (al); }
  static Model& make_N_min (Block& al)
  { return *new ConditionSoilN_min (al); }

  static bool check_water_content (const AttributeList& al, Treelog& err)
  {
    bool ok = true;

    const double from = al.number ("from");
    const double to = al.number ("to");
    if (from < to)
      {
	err.entry ("'from' must be higher than 'to' in"
		   " the measured area");
	ok = false;
      }
    return ok;
  }

  ConditionSoilSyntax ()
  {
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Test if the soil is warmer than the specified temperature.");
      syntax.add ("temperature", "dg C", Syntax::Const, "\
Lowest soil temperature for which the condition is true.");
      syntax.add ("height", "cm", Check::non_positive (), Syntax::Const, "\
Soil depth in which to test the temperature.");
      Librarian::add_type (Condition::component, "soil_temperature_above",
				      alist, syntax, &make_temperature);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Test if the soil is wetter than the specified pressure potential.");
      syntax.add ("potential", "cm", Syntax::Const, "\
The soil should be wetter than this for the condition to be true.");
      syntax.add ("height", "cm", Check::non_positive (), Syntax::Const, "\
Depth at which to example the pressure potential.");
      Librarian::add_type (Condition::component, "soil_water_pressure_above",
				      alist, syntax, &make_potential);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add_check (check_water_content);
      alist.add ("description", "\
Test if the soil contains more water than the specified amount.");
      syntax.add ("water", "mm", Check::non_negative (), Syntax::Const, "\
The soil should contain more water than this for the condition to be true.");
      syntax.add ("from", "cm", Check::non_positive (), Syntax::Const, "\
Top of interval to measure soil water content in.");
      alist.add ("from", 0.0);
      syntax.add ("to", "cm", Check::non_positive (), Syntax::Const, "\
Bottom of interval to measure soil water content in.");
      syntax.order ("water");
      Librarian::add_type (Condition::component, "soil_water_content_above",
				      alist, syntax, &make_water);
    }
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add_check (check_water_content);
      alist.add ("description", "\
Test if the soil contains more mineral nitrogen than the specified amount.");
      syntax.add ("amount", "kg N/ha",
		  Check::non_negative (), Syntax::Const, "\
The soil should contain more inorganic nitrogen than this for\n\
the condition to be true.");
      syntax.add ("from", "cm", Check::non_positive (), Syntax::Const, "\
Top of interval to measure soil content in.");
      alist.add ("from", 0.0);
      syntax.add ("to", "cm", Check::non_positive (), Syntax::Const, "\
Bottom of interval to measure soil content in.");
      syntax.order ("amount");
      Librarian::add_type (Condition::component, "soil_inorganic_N_above",
				      alist, syntax, &make_N_min);
    }
  }
} ConditionSoil_syntax;
