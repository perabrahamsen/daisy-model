// condition_soil.C
//
// Checking soil state.

#include "condition.h"
#include "column.h"
#include "daisy.h"
#include "frame.h"
#include "options.h"

struct ConditionSoilTemperature : public Condition
{
  double temperature;
  double height;

  bool match (const Frame& frame, const Daisy& daisy) const
    { 
      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{ 
	  if (frame.match_column (**i))
	    {
	      if ((*i)->soil_temperature (height) > temperature)
		return true;
	    }
	}
      return false;
    }

  ConditionSoilTemperature (const AttributeList& al)
    : temperature (al.number ("temperature")),
      height (al.number ("height"))
    { }
};

struct ConditionSoilPotential : public Condition
{
  double potential;
  double height;

  bool match (const Frame& frame, const Daisy& daisy) const
    { 
      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{ 
	  if (frame.match_column (**i))
	    {
	      if ((*i)->soil_water_potential (height) > potential)
		return true;
	    }
	}
      return false;
    }

  ConditionSoilPotential (const AttributeList& al)
    : potential (al.number ("potential")),
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
	Syntax& syntax = *new Syntax (&check);
	AttributeList& alist = *new AttributeList ();
	syntax.add ("temperature", Syntax::Number, Syntax::Const);
	syntax.add ("height", Syntax::Number, Syntax::Const);
	Librarian<Condition>::add_type ("soil_temperature_above",
					alist, syntax, &make_temperature);
      }
      {
	Syntax& syntax = *new Syntax (&check);
	AttributeList& alist = *new AttributeList ();
	syntax.add ("potential", Syntax::Number, Syntax::Const);
	syntax.add ("height", Syntax::Number, Syntax::Const);
	Librarian<Condition>::add_type ("soil_water_potential_above",
					alist, syntax, &make_potential);
      }
    }
} ConditionSoil_syntax;
