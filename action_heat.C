// action_heat.C

#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionSetHeatSource : public Action
{
  // Content.
  const double height;
  const double value;

  // Simulation.
  void doIt (Daisy& daisy)
  {
    COUT << " [Heat]\n";
    daisy.field.set_heat_source (height, value);
  }

  ActionSetHeatSource (const AttributeList& al)
    : Action (al),
      height (al.number ("height")),
      value (al.number ("value"))
  { }
};

static struct ActionHeatSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionSetHeatSource (al); }
  
  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    const double height  = al.number ("height");
    const double value  = al.number ("value");
    bool ok = true;
    non_positive (height, "height", ok, err);
    non_negative (value, "value", ok, err);
    return ok;
  }
  ActionHeatSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Set external point heat source at height to value.");
    syntax.add ("height", "cm", Syntax::Const,
		"Height of heat source (a negative number).");
    syntax.add ("value", "W/m^2", Syntax::Const,
		"Value of heat source.");
    syntax.order ("height", "value");
    Librarian<Action>::add_type ("set_heat_source", 
				 alist, syntax, &make);
  }
} ActionHeat_syntax;
