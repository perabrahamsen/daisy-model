// condition_soil.C
//
// Checking soil state.

#include "condition.h"
#include "field.h"
#include "daisy.h"

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
  void output (Log&, Filter&) const
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
  void output (Log&, Filter&) const
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

  static bool check (const AttributeList& al)
    {
      const double height (al.number ("height"));
      bool ok = true;
      non_positive (height, "height", ok);

      if (!ok)
	CERR << "in soil condition\n";
      return ok;
    }

  ConditionSoilSyntax ()
    {
      {
	Syntax& syntax = *new Syntax ();
	syntax.add_check (check);
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Test if the soil is warmer than the specified temperature.");
	syntax.add ("temperature", "dg C", Syntax::Const, "\
Lowest soil temperature for which the condition is true.");
	syntax.add ("height", "cm", Syntax::Const, "\
Soil depth in which to test the temperature (a negative number).");
	Librarian<Condition>::add_type ("soil_temperature_above",
					alist, syntax, &make_temperature);
      }
      {
	Syntax& syntax = *new Syntax ();
	syntax.add_check (check);
	AttributeList& alist = *new AttributeList ();
	alist.add ("description", "\
Test if the soil is wetter than the specified pressure potential.");
	syntax.add ("potential", "h", Syntax::Const, "\
The soil should be wetter than this for the condition to be true.");
	syntax.add ("height", "cm", Syntax::Const, "\
Depth at which to example the pressure potential (a negative number).");
	Librarian<Condition>::add_type ("soil_water_pressure_above",
					alist, syntax, &make_potential);
      }
    }
} ConditionSoil_syntax;
