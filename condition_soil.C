// condition_soil.C
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

//
// Checking soil state.

#include "condition.h"
#include "field.h"
#include "daisy.h"
#include "check.h"

struct ConditionSoilTemperature : public Condition
{
  double temperature;
  double height;

  bool match (const Daisy& daisy) const
    { 
      if (daisy.field.soil_temperature (height) > temperature)
	return true;
      return false;
    }
  void output (Log&) const
    { }

  ConditionSoilTemperature (const AttributeList& al)
    : Condition (al),
      temperature (al.number ("temperature")),
      height (al.number ("height"))
    { }
};

struct ConditionSoilPotential : public Condition
{
  double potential;
  double height;

  bool match (const Daisy& daisy) const
    { return (daisy.field.soil_water_potential (height) > potential); }
  void output (Log&) const
    { }

  ConditionSoilPotential (const AttributeList& al)
    : Condition (al),
      potential (al.number ("potential")),
      height (al.number ("height"))
    { }
};

static struct ConditionSoilSyntax
{
  static Condition& make_temperature (const AttributeList& al)
    { return *new ConditionSoilTemperature (al); }
  static Condition& make_potential (const AttributeList& al)
    { return *new ConditionSoilPotential (al); }

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
	Librarian<Condition>::add_type ("soil_temperature_above",
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
	Librarian<Condition>::add_type ("soil_water_pressure_above",
					alist, syntax, &make_potential);
      }
    }
} ConditionSoil_syntax;
